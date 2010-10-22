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

%% @type mem().    memory block
%% @type enum().   An integer defined in gl.hrl
%% @type offset(). An integer which is an offset in an array
%% @type clamp().  A float clamped between 0.0 - 1.0
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
-type clamp() :: float().
-type offset() :: non_neg_integer().
-type enum() :: non_neg_integer().
-type mem() :: binary() | tuple().

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
  getGraphicsResetStatusARB/0,resizeBuffersMESA/0,windowPos4dMESA/4,
  windowPos4dvMESA/1,windowPos4fMESA/4,windowPos4fvMESA/1,windowPos4iMESA/4,
  windowPos4ivMESA/1,windowPos4sMESA/4,windowPos4svMESA/1,depthBoundsEXT/2,
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

%% @spec (Op::enum(),Value::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAccum.xml">external</a> documentation.
-spec accum(enum(),float()) -> ok.
accum(Op,Value) ->
  cast(5037, <<Op:?GLenum,Value:?GLfloat>>).

%% @spec (Func::enum(),Ref::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAlphaFunc.xml">external</a> documentation.
-spec alphaFunc(enum(),clamp()) -> ok.
alphaFunc(Func,Ref) ->
  cast(5038, <<Func:?GLenum,Ref:?GLclampf>>).

%% @spec (Textures::[integer()]) -> {0|1,Residences::[0|1]}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAreTexturesResident.xml">external</a> documentation.
-spec areTexturesResident([integer()]) -> {0|1,[0|1]}.
areTexturesResident(Textures) ->
  call(5039, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

%% @spec (I::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glArrayElement.xml">external</a> documentation.
-spec arrayElement(integer()) -> ok.
arrayElement(I) ->
  cast(5040, <<I:?GLint>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBegin.xml">external</a> documentation.
-spec 'begin'(enum()) -> ok.
'begin'(Mode) ->
  cast(5041, <<Mode:?GLenum>>).

%% @spec (Target::enum(),Texture::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindTexture.xml">external</a> documentation.
-spec bindTexture(enum(),integer()) -> ok.
bindTexture(Target,Texture) ->
  cast(5042, <<Target:?GLenum,Texture:?GLuint>>).

%% @spec (Width::integer(),Height::integer(),Xorig::float(),Yorig::float(),Xmove::float(),Ymove::float(),Bitmap::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBitmap.xml">external</a> documentation.
-spec bitmap(integer(),integer(),float(),float(),float(),float(),offset()|mem()) -> ok.
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when  is_integer(Bitmap) ->
  cast(5043, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat,Bitmap:?GLuint>>);
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) ->
  send_bin(Bitmap),
  cast(5044, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat>>).

%% @spec (Sfactor::enum(),Dfactor::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunc.xml">external</a> documentation.
-spec blendFunc(enum(),enum()) -> ok.
blendFunc(Sfactor,Dfactor) ->
  cast(5045, <<Sfactor:?GLenum,Dfactor:?GLenum>>).

%% @spec (List::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallList.xml">external</a> documentation.
-spec callList(integer()) -> ok.
callList(List) ->
  cast(5046, <<List:?GLuint>>).

%% @spec (Lists::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallLists.xml">external</a> documentation.
-spec callLists([integer()]) -> ok.
callLists(Lists) ->
  cast(5047, <<(length(Lists)):?GLuint,
        (<< <<C:?GLuint>> || C <- Lists>>)/binary,0:(((1+length(Lists)) rem 2)*32)>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClear.xml">external</a> documentation.
-spec clear(integer()) -> ok.
clear(Mask) ->
  cast(5048, <<Mask:?GLbitfield>>).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearAccum.xml">external</a> documentation.
-spec clearAccum(float(),float(),float(),float()) -> ok.
clearAccum(Red,Green,Blue,Alpha) ->
  cast(5049, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @spec (Red::clamp(),Green::clamp(),Blue::clamp(),Alpha::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearColor.xml">external</a> documentation.
-spec clearColor(clamp(),clamp(),clamp(),clamp()) -> ok.
clearColor(Red,Green,Blue,Alpha) ->
  cast(5050, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @spec (Depth::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearDepth.xml">external</a> documentation.
-spec clearDepth(clamp()) -> ok.
clearDepth(Depth) ->
  cast(5051, <<Depth:?GLclampd>>).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearIndex.xml">external</a> documentation.
-spec clearIndex(float()) -> ok.
clearIndex(C) ->
  cast(5052, <<C:?GLfloat>>).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearStencil.xml">external</a> documentation.
-spec clearStencil(integer()) -> ok.
clearStencil(S) ->
  cast(5053, <<S:?GLint>>).

%% @spec (Plane::enum(),Equation::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClipPlane.xml">external</a> documentation.
-spec clipPlane(enum(),{float(),float(),float(),float()}) -> ok.
clipPlane(Plane,{E1,E2,E3,E4}) ->
  cast(5054, <<Plane:?GLenum,0:32,E1:?GLdouble,E2:?GLdouble,E3:?GLdouble,E4:?GLdouble>>).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3b(integer(),integer(),integer()) -> ok.
color3b(Red,Green,Blue) ->
  cast(5055, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3b(Red,Green,Blue)
-spec color3bv({integer(),integer(),integer()}) -> ok.
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3d(float(),float(),float()) -> ok.
color3d(Red,Green,Blue) ->
  cast(5056, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3d(Red,Green,Blue)
-spec color3dv({float(),float(),float()}) -> ok.
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3f(float(),float(),float()) -> ok.
color3f(Red,Green,Blue) ->
  cast(5057, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3f(Red,Green,Blue)
-spec color3fv({float(),float(),float()}) -> ok.
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3i(integer(),integer(),integer()) -> ok.
color3i(Red,Green,Blue) ->
  cast(5058, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3i(Red,Green,Blue)
-spec color3iv({integer(),integer(),integer()}) -> ok.
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3s(integer(),integer(),integer()) -> ok.
color3s(Red,Green,Blue) ->
  cast(5059, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3s(Red,Green,Blue)
-spec color3sv({integer(),integer(),integer()}) -> ok.
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3ub(integer(),integer(),integer()) -> ok.
color3ub(Red,Green,Blue) ->
  cast(5060, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3ub(Red,Green,Blue)
-spec color3ubv({integer(),integer(),integer()}) -> ok.
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3ui(integer(),integer(),integer()) -> ok.
color3ui(Red,Green,Blue) ->
  cast(5061, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3ui(Red,Green,Blue)
-spec color3uiv({integer(),integer(),integer()}) -> ok.
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color3us(integer(),integer(),integer()) -> ok.
color3us(Red,Green,Blue) ->
  cast(5062, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3us(Red,Green,Blue)
-spec color3usv({integer(),integer(),integer()}) -> ok.
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4b(integer(),integer(),integer(),integer()) -> ok.
color4b(Red,Green,Blue,Alpha) ->
  cast(5063, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte,Alpha:?GLbyte>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4b(Red,Green,Blue,Alpha)
-spec color4bv({integer(),integer(),integer(),integer()}) -> ok.
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4d(float(),float(),float(),float()) -> ok.
color4d(Red,Green,Blue,Alpha) ->
  cast(5064, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble,Alpha:?GLdouble>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4d(Red,Green,Blue,Alpha)
-spec color4dv({float(),float(),float(),float()}) -> ok.
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4f(float(),float(),float(),float()) -> ok.
color4f(Red,Green,Blue,Alpha) ->
  cast(5065, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4f(Red,Green,Blue,Alpha)
-spec color4fv({float(),float(),float(),float()}) -> ok.
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4i(integer(),integer(),integer(),integer()) -> ok.
color4i(Red,Green,Blue,Alpha) ->
  cast(5066, <<Red:?GLint,Green:?GLint,Blue:?GLint,Alpha:?GLint>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4i(Red,Green,Blue,Alpha)
-spec color4iv({integer(),integer(),integer(),integer()}) -> ok.
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4s(integer(),integer(),integer(),integer()) -> ok.
color4s(Red,Green,Blue,Alpha) ->
  cast(5067, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort,Alpha:?GLshort>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4s(Red,Green,Blue,Alpha)
-spec color4sv({integer(),integer(),integer(),integer()}) -> ok.
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4ub(integer(),integer(),integer(),integer()) -> ok.
color4ub(Red,Green,Blue,Alpha) ->
  cast(5068, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte,Alpha:?GLubyte>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4ub(Red,Green,Blue,Alpha)
-spec color4ubv({integer(),integer(),integer(),integer()}) -> ok.
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4ui(integer(),integer(),integer(),integer()) -> ok.
color4ui(Red,Green,Blue,Alpha) ->
  cast(5069, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint,Alpha:?GLuint>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4ui(Red,Green,Blue,Alpha)
-spec color4uiv({integer(),integer(),integer(),integer()}) -> ok.
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
-spec color4us(integer(),integer(),integer(),integer()) -> ok.
color4us(Red,Green,Blue,Alpha) ->
  cast(5070, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort,Alpha:?GLushort>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4us(Red,Green,Blue,Alpha)
-spec color4usv({integer(),integer(),integer(),integer()}) -> ok.
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).

%% @spec (Red::0|1,Green::0|1,Blue::0|1,Alpha::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMask.xml">external</a> documentation.
-spec colorMask(0|1,0|1,0|1,0|1) -> ok.
colorMask(Red,Green,Blue,Alpha) ->
  cast(5071, <<Red:?GLboolean,Green:?GLboolean,Blue:?GLboolean,Alpha:?GLboolean>>).

%% @spec (Face::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaterial.xml">external</a> documentation.
-spec colorMaterial(enum(),enum()) -> ok.
colorMaterial(Face,Mode) ->
  cast(5072, <<Face:?GLenum,Mode:?GLenum>>).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorPointer.xml">external</a> documentation.
-spec colorPointer(integer(),enum(),integer(),offset()|mem()) -> ok.
colorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5073, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
colorPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5074, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer(),Type::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyPixels.xml">external</a> documentation.
-spec copyPixels(integer(),integer(),integer(),integer(),enum()) -> ok.
copyPixels(X,Y,Width,Height,Type) ->
  cast(5075, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),InternalFormat::enum(),X::integer(),Y::integer(),Width::integer(),Border::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage1D.xml">external</a> documentation.
-spec copyTexImage1D(enum(),integer(),enum(),integer(),integer(),integer(),integer()) -> ok.
copyTexImage1D(Target,Level,InternalFormat,X,Y,Width,Border) ->
  cast(5076, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Border:?GLint>>).

%% @spec (Target::enum(),Level::integer(),InternalFormat::enum(),X::integer(),Y::integer(),Width::integer(),Height::integer(),Border::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage2D.xml">external</a> documentation.
-spec copyTexImage2D(enum(),integer(),enum(),integer(),integer(),integer(),integer(),integer()) -> ok.
copyTexImage2D(Target,Level,InternalFormat,X,Y,Width,Height,Border) ->
  cast(5077, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage1D.xml">external</a> documentation.
-spec copyTexSubImage1D(enum(),integer(),integer(),integer(),integer(),integer()) -> ok.
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) ->
  cast(5078, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage2D.xml">external</a> documentation.
-spec copyTexSubImage2D(enum(),integer(),integer(),integer(),integer(),integer(),integer(),integer()) -> ok.
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) ->
  cast(5079, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCullFace.xml">external</a> documentation.
-spec cullFace(enum()) -> ok.
cullFace(Mode) ->
  cast(5080, <<Mode:?GLenum>>).

%% @spec (List::integer(),Range::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteLists.xml">external</a> documentation.
-spec deleteLists(integer(),integer()) -> ok.
deleteLists(List,Range) ->
  cast(5081, <<List:?GLuint,Range:?GLsizei>>).

%% @spec (Textures::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTextures.xml">external</a> documentation.
-spec deleteTextures([integer()]) -> ok.
deleteTextures(Textures) ->
  cast(5082, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

%% @spec (Func::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthFunc.xml">external</a> documentation.
-spec depthFunc(enum()) -> ok.
depthFunc(Func) ->
  cast(5083, <<Func:?GLenum>>).

%% @spec (Flag::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthMask.xml">external</a> documentation.
-spec depthMask(0|1) -> ok.
depthMask(Flag) ->
  cast(5084, <<Flag:?GLboolean>>).

%% @spec (ZNear::clamp(),ZFar::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRange.xml">external</a> documentation.
-spec depthRange(clamp(),clamp()) -> ok.
depthRange(ZNear,ZFar) ->
  cast(5085, <<ZNear:?GLclampd,ZFar:?GLclampd>>).

%% @spec (Cap::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisable.xml">external</a> documentation.
-spec disable(enum()) -> ok.
disable(Cap) ->
  cast(5086, <<Cap:?GLenum>>).

%% @spec (Array::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisableClientState.xml">external</a> documentation.
-spec disableClientState(enum()) -> ok.
disableClientState(Array) ->
  cast(5087, <<Array:?GLenum>>).

%% @spec (Mode::enum(),First::integer(),Count::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArrays.xml">external</a> documentation.
-spec drawArrays(enum(),integer(),integer()) -> ok.
drawArrays(Mode,First,Count) ->
  cast(5088, <<Mode:?GLenum,First:?GLint,Count:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffer.xml">external</a> documentation.
-spec drawBuffer(enum()) -> ok.
drawBuffer(Mode) ->
  cast(5089, <<Mode:?GLenum>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElements.xml">external</a> documentation.
-spec drawElements(enum(),integer(),enum(),offset()|mem()) -> ok.
drawElements(Mode,Count,Type,Indices) when  is_integer(Indices) ->
  cast(5090, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawElements(Mode,Count,Type,Indices) ->
  send_bin(Indices),
  cast(5091, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum>>).

%% @spec (Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawPixels.xml">external</a> documentation.
-spec drawPixels(integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
drawPixels(Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5092, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
drawPixels(Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5093, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Flag::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlag.xml">external</a> documentation.
-spec edgeFlag(0|1) -> ok.
edgeFlag(Flag) ->
  cast(5094, <<Flag:?GLboolean>>).

%% @spec (Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlagPointer.xml">external</a> documentation.
-spec edgeFlagPointer(integer(),offset()|mem()) -> ok.
edgeFlagPointer(Stride,Pointer) when  is_integer(Pointer) ->
  cast(5095, <<Stride:?GLsizei,Pointer:?GLuint>>);
edgeFlagPointer(Stride,Pointer) ->
  send_bin(Pointer),
  cast(5096, <<Stride:?GLsizei>>).

%% @spec ({Flag}) -> ok
%% @equiv edgeFlag(Flag)
-spec edgeFlagv({0|1}) -> ok.
edgeFlagv({Flag}) ->  edgeFlag(Flag).

%% @spec (Cap::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml">external</a> documentation.
-spec enable(enum()) -> ok.
enable(Cap) ->
  cast(5097, <<Cap:?GLenum>>).

%% @spec (Array::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableClientState.xml">external</a> documentation.
-spec enableClientState(enum()) -> ok.
enableClientState(Array) ->
  cast(5098, <<Array:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnd.xml">external</a> documentation.
-spec 'end'() -> ok.
'end'() ->
  cast(5099, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndList.xml">external</a> documentation.
-spec endList() -> ok.
endList() ->
  cast(5100, <<>>).

%% @spec (U::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
-spec evalCoord1d(float()) -> ok.
evalCoord1d(U) ->
  cast(5101, <<U:?GLdouble>>).

%% @spec ({U}) -> ok
%% @equiv evalCoord1d(U)
-spec evalCoord1dv({float()}) -> ok.
evalCoord1dv({U}) ->  evalCoord1d(U).

%% @spec (U::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
-spec evalCoord1f(float()) -> ok.
evalCoord1f(U) ->
  cast(5102, <<U:?GLfloat>>).

%% @spec ({U}) -> ok
%% @equiv evalCoord1f(U)
-spec evalCoord1fv({float()}) -> ok.
evalCoord1fv({U}) ->  evalCoord1f(U).

%% @spec (U::float(),V::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
-spec evalCoord2d(float(),float()) -> ok.
evalCoord2d(U,V) ->
  cast(5103, <<U:?GLdouble,V:?GLdouble>>).

%% @spec ({U,V}) -> ok
%% @equiv evalCoord2d(U,V)
-spec evalCoord2dv({float(),float()}) -> ok.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).

%% @spec (U::float(),V::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
-spec evalCoord2f(float(),float()) -> ok.
evalCoord2f(U,V) ->
  cast(5104, <<U:?GLfloat,V:?GLfloat>>).

%% @spec ({U,V}) -> ok
%% @equiv evalCoord2f(U,V)
-spec evalCoord2fv({float(),float()}) -> ok.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).

%% @spec (Mode::enum(),I1::integer(),I2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml">external</a> documentation.
-spec evalMesh1(enum(),integer(),integer()) -> ok.
evalMesh1(Mode,I1,I2) ->
  cast(5105, <<Mode:?GLenum,I1:?GLint,I2:?GLint>>).

%% @spec (Mode::enum(),I1::integer(),I2::integer(),J1::integer(),J2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml">external</a> documentation.
-spec evalMesh2(enum(),integer(),integer(),integer(),integer()) -> ok.
evalMesh2(Mode,I1,I2,J1,J2) ->
  cast(5106, <<Mode:?GLenum,I1:?GLint,I2:?GLint,J1:?GLint,J2:?GLint>>).

%% @spec (I::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml">external</a> documentation.
-spec evalPoint1(integer()) -> ok.
evalPoint1(I) ->
  cast(5107, <<I:?GLint>>).

%% @spec (I::integer(),J::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml">external</a> documentation.
-spec evalPoint2(integer(),integer()) -> ok.
evalPoint2(I,J) ->
  cast(5108, <<I:?GLint,J:?GLint>>).

%% @spec (Size::integer(),Type::enum(),Buffer::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFeedbackBuffer.xml">external</a> documentation.
-spec feedbackBuffer(integer(),enum(),mem()) -> ok.
feedbackBuffer(Size,Type,Buffer) ->
  send_bin(Buffer),
  call(5109, <<Size:?GLsizei,Type:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFinish.xml">external</a> documentation.
-spec finish() -> ok.
finish() ->
  cast(5110, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlush.xml">external</a> documentation.
-spec flush() -> ok.
flush() ->
  cast(5111, <<>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
-spec fogf(enum(),float()) -> ok.
fogf(Pname,Param) ->
  cast(5112, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
-spec fogfv(enum(),{float()}) -> ok.
fogfv(Pname,Params) ->
  cast(5113, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
-spec fogi(enum(),integer()) -> ok.
fogi(Pname,Param) ->
  cast(5114, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
-spec fogiv(enum(),{integer()}) -> ok.
fogiv(Pname,Params) ->
  cast(5115, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrontFace.xml">external</a> documentation.
-spec frontFace(enum()) -> ok.
frontFace(Mode) ->
  cast(5116, <<Mode:?GLenum>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrustum.xml">external</a> documentation.
-spec frustum(float(),float(),float(),float(),float(),float()) -> ok.
frustum(Left,Right,Bottom,Top,ZNear,ZFar) ->
  cast(5117, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (Range::integer()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenLists.xml">external</a> documentation.
-spec genLists(integer()) -> integer().
genLists(Range) ->
  call(5118, <<Range:?GLsizei>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenTextures.xml">external</a> documentation.
-spec genTextures(integer()) -> [integer()].
genTextures(N) ->
  call(5119, <<N:?GLsizei>>).

%% @spec (Pname::enum()) -> [0|1]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBooleanv.xml">external</a> documentation.
-spec getBooleanv(enum()) -> [0|1].
getBooleanv(Pname) ->
  call(5120, <<Pname:?GLenum>>).

%% @spec (Plane::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetClipPlane.xml">external</a> documentation.
-spec getClipPlane(enum()) -> {float(),float(),float(),float()}.
getClipPlane(Plane) ->
  call(5121, <<Plane:?GLenum>>).

%% @spec (Pname::enum()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetDoublev.xml">external</a> documentation.
-spec getDoublev(enum()) -> [float()].
getDoublev(Pname) ->
  call(5122, <<Pname:?GLenum>>).

%% @spec () -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetError.xml">external</a> documentation.
-spec getError() -> enum().
getError() ->
  call(5123, <<>>).

%% @spec (Pname::enum()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFloatv.xml">external</a> documentation.
-spec getFloatv(enum()) -> [float()].
getFloatv(Pname) ->
  call(5124, <<Pname:?GLenum>>).

%% @spec (Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetIntegerv.xml">external</a> documentation.
-spec getIntegerv(enum()) -> [integer()].
getIntegerv(Pname) ->
  call(5125, <<Pname:?GLenum>>).

%% @spec (Light::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml">external</a> documentation.
-spec getLightfv(enum(),enum()) -> {float(),float(),float(),float()}.
getLightfv(Light,Pname) ->
  call(5126, <<Light:?GLenum,Pname:?GLenum>>).

%% @spec (Light::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml">external</a> documentation.
-spec getLightiv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getLightiv(Light,Pname) ->
  call(5127, <<Light:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
-spec getMapdv(enum(),enum(),mem()) -> ok.
getMapdv(Target,Query,V) ->
  send_bin(V),
  call(5128, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
-spec getMapfv(enum(),enum(),mem()) -> ok.
getMapfv(Target,Query,V) ->
  send_bin(V),
  call(5129, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
-spec getMapiv(enum(),enum(),mem()) -> ok.
getMapiv(Target,Query,V) ->
  send_bin(V),
  call(5130, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Face::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml">external</a> documentation.
-spec getMaterialfv(enum(),enum()) -> {float(),float(),float(),float()}.
getMaterialfv(Face,Pname) ->
  call(5131, <<Face:?GLenum,Pname:?GLenum>>).

%% @spec (Face::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml">external</a> documentation.
-spec getMaterialiv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getMaterialiv(Face,Pname) ->
  call(5132, <<Face:?GLenum,Pname:?GLenum>>).

%% @spec (Map::enum(),Values::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
-spec getPixelMapfv(enum(),mem()) -> ok.
getPixelMapfv(Map,Values) ->
  send_bin(Values),
  call(5133, <<Map:?GLenum>>).

%% @spec (Map::enum(),Values::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
-spec getPixelMapuiv(enum(),mem()) -> ok.
getPixelMapuiv(Map,Values) ->
  send_bin(Values),
  call(5134, <<Map:?GLenum>>).

%% @spec (Map::enum(),Values::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
-spec getPixelMapusv(enum(),mem()) -> ok.
getPixelMapusv(Map,Values) ->
  send_bin(Values),
  call(5135, <<Map:?GLenum>>).

%% @spec () -> binary()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPolygonStipple.xml">external</a> documentation.
-spec getPolygonStipple() -> binary().
getPolygonStipple() ->
  call(5136, <<>>).

%% @spec (Name::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml">external</a> documentation.
-spec getString(enum()) -> string().
getString(Name) ->
  call(5137, <<Name:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexEnv.xml">external</a> documentation.
-spec getTexEnvfv(enum(),enum()) -> {float(),float(),float(),float()}.
getTexEnvfv(Target,Pname) ->
  call(5138, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexEnv.xml">external</a> documentation.
-spec getTexEnviv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getTexEnviv(Target,Pname) ->
  call(5139, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
-spec getTexGendv(enum(),enum()) -> {float(),float(),float(),float()}.
getTexGendv(Coord,Pname) ->
  call(5140, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
-spec getTexGenfv(enum(),enum()) -> {float(),float(),float(),float()}.
getTexGenfv(Coord,Pname) ->
  call(5141, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
-spec getTexGeniv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getTexGeniv(Coord,Pname) ->
  call(5142, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Format::enum(),Type::enum(),Pixels::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexImage.xml">external</a> documentation.
-spec getTexImage(enum(),integer(),enum(),enum(),mem()) -> ok.
getTexImage(Target,Level,Format,Type,Pixels) ->
  send_bin(Pixels),
  call(5143, <<Target:?GLenum,Level:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml">external</a> documentation.
-spec getTexLevelParameterfv(enum(),integer(),enum()) -> {float()}.
getTexLevelParameterfv(Target,Level,Pname) ->
  call(5144, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml">external</a> documentation.
-spec getTexLevelParameteriv(enum(),integer(),enum()) -> {integer()}.
getTexLevelParameteriv(Target,Level,Pname) ->
  call(5145, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml">external</a> documentation.
-spec getTexParameterfv(enum(),enum()) -> {float(),float(),float(),float()}.
getTexParameterfv(Target,Pname) ->
  call(5146, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml">external</a> documentation.
-spec getTexParameteriv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getTexParameteriv(Target,Pname) ->
  call(5147, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHint.xml">external</a> documentation.
-spec hint(enum(),enum()) -> ok.
hint(Target,Mode) ->
  cast(5148, <<Target:?GLenum,Mode:?GLenum>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexMask.xml">external</a> documentation.
-spec indexMask(integer()) -> ok.
indexMask(Mask) ->
  cast(5149, <<Mask:?GLuint>>).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexPointer.xml">external</a> documentation.
-spec indexPointer(enum(),integer(),offset()|mem()) -> ok.
indexPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5150, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
indexPointer(Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5151, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
-spec indexd(float()) -> ok.
indexd(C) ->
  cast(5152, <<C:?GLdouble>>).

%% @spec ({C}) -> ok
%% @equiv indexd(C)
-spec indexdv({float()}) -> ok.
indexdv({C}) ->  indexd(C).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
-spec indexf(float()) -> ok.
indexf(C) ->
  cast(5153, <<C:?GLfloat>>).

%% @spec ({C}) -> ok
%% @equiv indexf(C)
-spec indexfv({float()}) -> ok.
indexfv({C}) ->  indexf(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
-spec indexi(integer()) -> ok.
indexi(C) ->
  cast(5154, <<C:?GLint>>).

%% @spec ({C}) -> ok
%% @equiv indexi(C)
-spec indexiv({integer()}) -> ok.
indexiv({C}) ->  indexi(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
-spec indexs(integer()) -> ok.
indexs(C) ->
  cast(5155, <<C:?GLshort>>).

%% @spec ({C}) -> ok
%% @equiv indexs(C)
-spec indexsv({integer()}) -> ok.
indexsv({C}) ->  indexs(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
-spec indexub(integer()) -> ok.
indexub(C) ->
  cast(5156, <<C:?GLubyte>>).

%% @spec ({C}) -> ok
%% @equiv indexub(C)
-spec indexubv({integer()}) -> ok.
indexubv({C}) ->  indexub(C).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInitNames.xml">external</a> documentation.
-spec initNames() -> ok.
initNames() ->
  cast(5157, <<>>).

%% @spec (Format::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInterleavedArrays.xml">external</a> documentation.
-spec interleavedArrays(enum(),integer(),offset()|mem()) -> ok.
interleavedArrays(Format,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5158, <<Format:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
interleavedArrays(Format,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5159, <<Format:?GLenum,Stride:?GLsizei>>).

%% @spec (Cap::enum()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabled.xml">external</a> documentation.
-spec isEnabled(enum()) -> 0|1.
isEnabled(Cap) ->
  call(5160, <<Cap:?GLenum>>).

%% @spec (List::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsList.xml">external</a> documentation.
-spec isList(integer()) -> 0|1.
isList(List) ->
  call(5161, <<List:?GLuint>>).

%% @spec (Texture::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsTexture.xml">external</a> documentation.
-spec isTexture(integer()) -> 0|1.
isTexture(Texture) ->
  call(5162, <<Texture:?GLuint>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
-spec lightModelf(enum(),float()) -> ok.
lightModelf(Pname,Param) ->
  cast(5163, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
-spec lightModelfv(enum(),{float()}) -> ok.
lightModelfv(Pname,Params) ->
  cast(5164, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
-spec lightModeli(enum(),integer()) -> ok.
lightModeli(Pname,Param) ->
  cast(5165, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
-spec lightModeliv(enum(),{integer()}) -> ok.
lightModeliv(Pname,Params) ->
  cast(5166, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Light::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
-spec lightf(enum(),enum(),float()) -> ok.
lightf(Light,Pname,Param) ->
  cast(5167, <<Light:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Light::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
-spec lightfv(enum(),enum(),{float()}) -> ok.
lightfv(Light,Pname,Params) ->
  cast(5168, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Light::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
-spec lighti(enum(),enum(),integer()) -> ok.
lighti(Light,Pname,Param) ->
  cast(5169, <<Light:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Light::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
-spec lightiv(enum(),enum(),{integer()}) -> ok.
lightiv(Light,Pname,Params) ->
  cast(5170, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Factor::integer(),Pattern::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineStipple.xml">external</a> documentation.
-spec lineStipple(integer(),integer()) -> ok.
lineStipple(Factor,Pattern) ->
  cast(5171, <<Factor:?GLint,Pattern:?GLushort>>).

%% @spec (Width::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineWidth.xml">external</a> documentation.
-spec lineWidth(float()) -> ok.
lineWidth(Width) ->
  cast(5172, <<Width:?GLfloat>>).

%% @spec (Base::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glListBase.xml">external</a> documentation.
-spec listBase(integer()) -> ok.
listBase(Base) ->
  cast(5173, <<Base:?GLuint>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadIdentity.xml">external</a> documentation.
-spec loadIdentity() -> ok.
loadIdentity() ->
  cast(5174, <<>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml">external</a> documentation.
-spec loadMatrixd({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5175, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5175, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml">external</a> documentation.
-spec loadMatrixf({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5176, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5176, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (Name::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadName.xml">external</a> documentation.
-spec loadName(integer()) -> ok.
loadName(Name) ->
  cast(5177, <<Name:?GLuint>>).

%% @spec (Opcode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLogicOp.xml">external</a> documentation.
-spec logicOp(enum()) -> ok.
logicOp(Opcode) ->
  cast(5178, <<Opcode:?GLenum>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Stride::integer(),Order::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map1d(enum(),float(),float(),integer(),integer(),binary()) -> ok.
map1d(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5179, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Stride:?GLint,Order:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Stride::integer(),Order::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map1f(enum(),float(),float(),integer(),integer(),binary()) -> ok.
map1f(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5180, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Stride:?GLint,Order:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Ustride::integer(),Uorder::integer(),V1::float(),V2::float(),Vstride::integer(),Vorder::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map2d(enum(),float(),float(),integer(),integer(),float(),float(),integer(),integer(),binary()) -> ok.
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  send_bin(Points),
  cast(5181, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Ustride:?GLint,Uorder:?GLint,V1:?GLdouble,V2:?GLdouble,Vstride:?GLint,Vorder:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Ustride::integer(),Uorder::integer(),V1::float(),V2::float(),Vstride::integer(),Vorder::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map2f(enum(),float(),float(),integer(),integer(),float(),float(),integer(),integer(),binary()) -> ok.
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  send_bin(Points),
  cast(5182, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Ustride:?GLint,Uorder:?GLint,V1:?GLfloat,V2:?GLfloat,Vstride:?GLint,Vorder:?GLint>>).

%% @spec (Un::integer(),U1::float(),U2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
-spec mapGrid1d(integer(),float(),float()) -> ok.
mapGrid1d(Un,U1,U2) ->
  cast(5183, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble>>).

%% @spec (Un::integer(),U1::float(),U2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
-spec mapGrid1f(integer(),float(),float()) -> ok.
mapGrid1f(Un,U1,U2) ->
  cast(5184, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat>>).

%% @spec (Un::integer(),U1::float(),U2::float(),Vn::integer(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
-spec mapGrid2d(integer(),float(),float(),integer(),float(),float()) -> ok.
mapGrid2d(Un,U1,U2,Vn,V1,V2) ->
  cast(5185, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble,Vn:?GLint,0:32,V1:?GLdouble,V2:?GLdouble>>).

%% @spec (Un::integer(),U1::float(),U2::float(),Vn::integer(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
-spec mapGrid2f(integer(),float(),float(),integer(),float(),float()) -> ok.
mapGrid2f(Un,U1,U2,Vn,V1,V2) ->
  cast(5186, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat,Vn:?GLint,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (Face::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
-spec materialf(enum(),enum(),float()) -> ok.
materialf(Face,Pname,Param) ->
  cast(5187, <<Face:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Face::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
-spec materialfv(enum(),enum(),{float()}) -> ok.
materialfv(Face,Pname,Params) ->
  cast(5188, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Face::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
-spec materiali(enum(),enum(),integer()) -> ok.
materiali(Face,Pname,Param) ->
  cast(5189, <<Face:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Face::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
-spec materialiv(enum(),enum(),{integer()}) -> ok.
materialiv(Face,Pname,Params) ->
  cast(5190, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixMode.xml">external</a> documentation.
-spec matrixMode(enum()) -> ok.
matrixMode(Mode) ->
  cast(5191, <<Mode:?GLenum>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml">external</a> documentation.
-spec multMatrixd({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5192, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5192, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml">external</a> documentation.
-spec multMatrixf({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5193, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5193, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (List::integer(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNewList.xml">external</a> documentation.
-spec newList(integer(),enum()) -> ok.
newList(List,Mode) ->
  cast(5194, <<List:?GLuint,Mode:?GLenum>>).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
-spec normal3b(integer(),integer(),integer()) -> ok.
normal3b(Nx,Ny,Nz) ->
  cast(5195, <<Nx:?GLbyte,Ny:?GLbyte,Nz:?GLbyte>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3b(Nx,Ny,Nz)
-spec normal3bv({integer(),integer(),integer()}) -> ok.
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).

%% @spec (Nx::float(),Ny::float(),Nz::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
-spec normal3d(float(),float(),float()) -> ok.
normal3d(Nx,Ny,Nz) ->
  cast(5196, <<Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3d(Nx,Ny,Nz)
-spec normal3dv({float(),float(),float()}) -> ok.
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).

%% @spec (Nx::float(),Ny::float(),Nz::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
-spec normal3f(float(),float(),float()) -> ok.
normal3f(Nx,Ny,Nz) ->
  cast(5197, <<Nx:?GLfloat,Ny:?GLfloat,Nz:?GLfloat>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3f(Nx,Ny,Nz)
-spec normal3fv({float(),float(),float()}) -> ok.
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
-spec normal3i(integer(),integer(),integer()) -> ok.
normal3i(Nx,Ny,Nz) ->
  cast(5198, <<Nx:?GLint,Ny:?GLint,Nz:?GLint>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3i(Nx,Ny,Nz)
-spec normal3iv({integer(),integer(),integer()}) -> ok.
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
-spec normal3s(integer(),integer(),integer()) -> ok.
normal3s(Nx,Ny,Nz) ->
  cast(5199, <<Nx:?GLshort,Ny:?GLshort,Nz:?GLshort>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3s(Nx,Ny,Nz)
-spec normal3sv({integer(),integer(),integer()}) -> ok.
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormalPointer.xml">external</a> documentation.
-spec normalPointer(enum(),integer(),offset()|mem()) -> ok.
normalPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5200, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
normalPointer(Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5201, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml">external</a> documentation.
-spec ortho(float(),float(),float(),float(),float(),float()) -> ok.
ortho(Left,Right,Bottom,Top,ZNear,ZFar) ->
  cast(5202, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (Token::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPassThrough.xml">external</a> documentation.
-spec passThrough(float()) -> ok.
passThrough(Token) ->
  cast(5203, <<Token:?GLfloat>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
-spec pixelMapfv(enum(),integer(),binary()) -> ok.
pixelMapfv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5204, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
-spec pixelMapuiv(enum(),integer(),binary()) -> ok.
pixelMapuiv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5205, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
-spec pixelMapusv(enum(),integer(),binary()) -> ok.
pixelMapusv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5206, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml">external</a> documentation.
-spec pixelStoref(enum(),float()) -> ok.
pixelStoref(Pname,Param) ->
  cast(5207, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml">external</a> documentation.
-spec pixelStorei(enum(),integer()) -> ok.
pixelStorei(Pname,Param) ->
  cast(5208, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml">external</a> documentation.
-spec pixelTransferf(enum(),float()) -> ok.
pixelTransferf(Pname,Param) ->
  cast(5209, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml">external</a> documentation.
-spec pixelTransferi(enum(),integer()) -> ok.
pixelTransferi(Pname,Param) ->
  cast(5210, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Xfactor::float(),Yfactor::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelZoom.xml">external</a> documentation.
-spec pixelZoom(float(),float()) -> ok.
pixelZoom(Xfactor,Yfactor) ->
  cast(5211, <<Xfactor:?GLfloat,Yfactor:?GLfloat>>).

%% @spec (Size::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointSize.xml">external</a> documentation.
-spec pointSize(float()) -> ok.
pointSize(Size) ->
  cast(5212, <<Size:?GLfloat>>).

%% @spec (Face::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonMode.xml">external</a> documentation.
-spec polygonMode(enum(),enum()) -> ok.
polygonMode(Face,Mode) ->
  cast(5213, <<Face:?GLenum,Mode:?GLenum>>).

%% @spec (Factor::float(),Units::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml">external</a> documentation.
-spec polygonOffset(float(),float()) -> ok.
polygonOffset(Factor,Units) ->
  cast(5214, <<Factor:?GLfloat,Units:?GLfloat>>).

%% @spec (Mask::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonStipple.xml">external</a> documentation.
-spec polygonStipple(binary()) -> ok.
polygonStipple(Mask) ->
  send_bin(Mask),
  cast(5215, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopAttrib.xml">external</a> documentation.
-spec popAttrib() -> ok.
popAttrib() ->
  cast(5216, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopClientAttrib.xml">external</a> documentation.
-spec popClientAttrib() -> ok.
popClientAttrib() ->
  cast(5217, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopMatrix.xml">external</a> documentation.
-spec popMatrix() -> ok.
popMatrix() ->
  cast(5218, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopName.xml">external</a> documentation.
-spec popName() -> ok.
popName() ->
  cast(5219, <<>>).

%% @spec (Textures::[integer()],Priorities::[clamp()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrioritizeTextures.xml">external</a> documentation.
-spec prioritizeTextures([integer()],[clamp()]) -> ok.
prioritizeTextures(Textures,Priorities) ->
  cast(5220, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32),(length(Priorities)):?GLuint,
        (<< <<C:?GLclampf>> || C <- Priorities>>)/binary,0:(((1+length(Priorities)) rem 2)*32)>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushAttrib.xml">external</a> documentation.
-spec pushAttrib(integer()) -> ok.
pushAttrib(Mask) ->
  cast(5221, <<Mask:?GLbitfield>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushClientAttrib.xml">external</a> documentation.
-spec pushClientAttrib(integer()) -> ok.
pushClientAttrib(Mask) ->
  cast(5222, <<Mask:?GLbitfield>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushMatrix.xml">external</a> documentation.
-spec pushMatrix() -> ok.
pushMatrix() ->
  cast(5223, <<>>).

%% @spec (Name::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushName.xml">external</a> documentation.
-spec pushName(integer()) -> ok.
pushName(Name) ->
  cast(5224, <<Name:?GLuint>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos2d(float(),float()) -> ok.
rasterPos2d(X,Y) ->
  cast(5225, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2d(X,Y)
-spec rasterPos2dv({float(),float()}) -> ok.
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos2f(float(),float()) -> ok.
rasterPos2f(X,Y) ->
  cast(5226, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2f(X,Y)
-spec rasterPos2fv({float(),float()}) -> ok.
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos2i(integer(),integer()) -> ok.
rasterPos2i(X,Y) ->
  cast(5227, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2i(X,Y)
-spec rasterPos2iv({integer(),integer()}) -> ok.
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos2s(integer(),integer()) -> ok.
rasterPos2s(X,Y) ->
  cast(5228, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2s(X,Y)
-spec rasterPos2sv({integer(),integer()}) -> ok.
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos3d(float(),float(),float()) -> ok.
rasterPos3d(X,Y,Z) ->
  cast(5229, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3d(X,Y,Z)
-spec rasterPos3dv({float(),float(),float()}) -> ok.
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos3f(float(),float(),float()) -> ok.
rasterPos3f(X,Y,Z) ->
  cast(5230, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3f(X,Y,Z)
-spec rasterPos3fv({float(),float(),float()}) -> ok.
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos3i(integer(),integer(),integer()) -> ok.
rasterPos3i(X,Y,Z) ->
  cast(5231, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3i(X,Y,Z)
-spec rasterPos3iv({integer(),integer(),integer()}) -> ok.
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos3s(integer(),integer(),integer()) -> ok.
rasterPos3s(X,Y,Z) ->
  cast(5232, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3s(X,Y,Z)
-spec rasterPos3sv({integer(),integer(),integer()}) -> ok.
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos4d(float(),float(),float(),float()) -> ok.
rasterPos4d(X,Y,Z,W) ->
  cast(5233, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4d(X,Y,Z,W)
-spec rasterPos4dv({float(),float(),float(),float()}) -> ok.
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos4f(float(),float(),float(),float()) -> ok.
rasterPos4f(X,Y,Z,W) ->
  cast(5234, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4f(X,Y,Z,W)
-spec rasterPos4fv({float(),float(),float(),float()}) -> ok.
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos4i(integer(),integer(),integer(),integer()) -> ok.
rasterPos4i(X,Y,Z,W) ->
  cast(5235, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4i(X,Y,Z,W)
-spec rasterPos4iv({integer(),integer(),integer(),integer()}) -> ok.
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos4s(integer(),integer(),integer(),integer()) -> ok.
rasterPos4s(X,Y,Z,W) ->
  cast(5236, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4s(X,Y,Z,W)
-spec rasterPos4sv({integer(),integer(),integer(),integer()}) -> ok.
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadBuffer.xml">external</a> documentation.
-spec readBuffer(enum()) -> ok.
readBuffer(Mode) ->
  cast(5237, <<Mode:?GLenum>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadPixels.xml">external</a> documentation.
-spec readPixels(integer(),integer(),integer(),integer(),enum(),enum(),mem()) -> ok.
readPixels(X,Y,Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  call(5238, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (X1::float(),Y1::float(),X2::float(),Y2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectd(float(),float(),float(),float()) -> ok.
rectd(X1,Y1,X2,Y2) ->
  cast(5239, <<X1:?GLdouble,Y1:?GLdouble,X2:?GLdouble,Y2:?GLdouble>>).

%% @spec (V1::{float(),float()},V2::{float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectdv({float(),float()},{float(),float()}) -> ok.
rectdv({V1,V2},{V1,V2}) ->
  cast(5240, <<V1:?GLdouble,V2:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @spec (X1::float(),Y1::float(),X2::float(),Y2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectf(float(),float(),float(),float()) -> ok.
rectf(X1,Y1,X2,Y2) ->
  cast(5241, <<X1:?GLfloat,Y1:?GLfloat,X2:?GLfloat,Y2:?GLfloat>>).

%% @spec (V1::{float(),float()},V2::{float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectfv({float(),float()},{float(),float()}) -> ok.
rectfv({V1,V2},{V1,V2}) ->
  cast(5242, <<V1:?GLfloat,V2:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (X1::integer(),Y1::integer(),X2::integer(),Y2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec recti(integer(),integer(),integer(),integer()) -> ok.
recti(X1,Y1,X2,Y2) ->
  cast(5243, <<X1:?GLint,Y1:?GLint,X2:?GLint,Y2:?GLint>>).

%% @spec (V1::{integer(),integer()},V2::{integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectiv({integer(),integer()},{integer(),integer()}) -> ok.
rectiv({V1,V2},{V1,V2}) ->
  cast(5244, <<V1:?GLint,V2:?GLint,V1:?GLint,V2:?GLint>>).

%% @spec (X1::integer(),Y1::integer(),X2::integer(),Y2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rects(integer(),integer(),integer(),integer()) -> ok.
rects(X1,Y1,X2,Y2) ->
  cast(5245, <<X1:?GLshort,Y1:?GLshort,X2:?GLshort,Y2:?GLshort>>).

%% @spec (V1::{integer(),integer()},V2::{integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
-spec rectsv({integer(),integer()},{integer(),integer()}) -> ok.
rectsv({V1,V2},{V1,V2}) ->
  cast(5246, <<V1:?GLshort,V2:?GLshort,V1:?GLshort,V2:?GLshort>>).

%% @spec (Mode::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderMode.xml">external</a> documentation.
-spec renderMode(enum()) -> integer().
renderMode(Mode) ->
  call(5247, <<Mode:?GLenum>>).

%% @spec (Angle::float(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml">external</a> documentation.
-spec rotated(float(),float(),float(),float()) -> ok.
rotated(Angle,X,Y,Z) ->
  cast(5248, <<Angle:?GLdouble,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Angle::float(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml">external</a> documentation.
-spec rotatef(float(),float(),float(),float()) -> ok.
rotatef(Angle,X,Y,Z) ->
  cast(5249, <<Angle:?GLfloat,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml">external</a> documentation.
-spec scaled(float(),float(),float()) -> ok.
scaled(X,Y,Z) ->
  cast(5250, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml">external</a> documentation.
-spec scalef(float(),float(),float()) -> ok.
scalef(X,Y,Z) ->
  cast(5251, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml">external</a> documentation.
-spec scissor(integer(),integer(),integer(),integer()) -> ok.
scissor(X,Y,Width,Height) ->
  cast(5252, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Size::integer(),Buffer::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSelectBuffer.xml">external</a> documentation.
-spec selectBuffer(integer(),mem()) -> ok.
selectBuffer(Size,Buffer) ->
  send_bin(Buffer),
  call(5253, <<Size:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShadeModel.xml">external</a> documentation.
-spec shadeModel(enum()) -> ok.
shadeModel(Mode) ->
  cast(5254, <<Mode:?GLenum>>).

%% @spec (Func::enum(),Ref::integer(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFunc.xml">external</a> documentation.
-spec stencilFunc(enum(),integer(),integer()) -> ok.
stencilFunc(Func,Ref,Mask) ->
  cast(5255, <<Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMask.xml">external</a> documentation.
-spec stencilMask(integer()) -> ok.
stencilMask(Mask) ->
  cast(5256, <<Mask:?GLuint>>).

%% @spec (Fail::enum(),Zfail::enum(),Zpass::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOp.xml">external</a> documentation.
-spec stencilOp(enum(),enum(),enum()) -> ok.
stencilOp(Fail,Zfail,Zpass) ->
  cast(5257, <<Fail:?GLenum,Zfail:?GLenum,Zpass:?GLenum>>).

%% @spec (S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord1d(float()) -> ok.
texCoord1d(S) ->
  cast(5258, <<S:?GLdouble>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1d(S)
-spec texCoord1dv({float()}) -> ok.
texCoord1dv({S}) ->  texCoord1d(S).

%% @spec (S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord1f(float()) -> ok.
texCoord1f(S) ->
  cast(5259, <<S:?GLfloat>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1f(S)
-spec texCoord1fv({float()}) -> ok.
texCoord1fv({S}) ->  texCoord1f(S).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord1i(integer()) -> ok.
texCoord1i(S) ->
  cast(5260, <<S:?GLint>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1i(S)
-spec texCoord1iv({integer()}) -> ok.
texCoord1iv({S}) ->  texCoord1i(S).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord1s(integer()) -> ok.
texCoord1s(S) ->
  cast(5261, <<S:?GLshort>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1s(S)
-spec texCoord1sv({integer()}) -> ok.
texCoord1sv({S}) ->  texCoord1s(S).

%% @spec (S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord2d(float(),float()) -> ok.
texCoord2d(S,T) ->
  cast(5262, <<S:?GLdouble,T:?GLdouble>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2d(S,T)
-spec texCoord2dv({float(),float()}) -> ok.
texCoord2dv({S,T}) ->  texCoord2d(S,T).

%% @spec (S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord2f(float(),float()) -> ok.
texCoord2f(S,T) ->
  cast(5263, <<S:?GLfloat,T:?GLfloat>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2f(S,T)
-spec texCoord2fv({float(),float()}) -> ok.
texCoord2fv({S,T}) ->  texCoord2f(S,T).

%% @spec (S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord2i(integer(),integer()) -> ok.
texCoord2i(S,T) ->
  cast(5264, <<S:?GLint,T:?GLint>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2i(S,T)
-spec texCoord2iv({integer(),integer()}) -> ok.
texCoord2iv({S,T}) ->  texCoord2i(S,T).

%% @spec (S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord2s(integer(),integer()) -> ok.
texCoord2s(S,T) ->
  cast(5265, <<S:?GLshort,T:?GLshort>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2s(S,T)
-spec texCoord2sv({integer(),integer()}) -> ok.
texCoord2sv({S,T}) ->  texCoord2s(S,T).

%% @spec (S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord3d(float(),float(),float()) -> ok.
texCoord3d(S,T,R) ->
  cast(5266, <<S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3d(S,T,R)
-spec texCoord3dv({float(),float(),float()}) -> ok.
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).

%% @spec (S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord3f(float(),float(),float()) -> ok.
texCoord3f(S,T,R) ->
  cast(5267, <<S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3f(S,T,R)
-spec texCoord3fv({float(),float(),float()}) -> ok.
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).

%% @spec (S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord3i(integer(),integer(),integer()) -> ok.
texCoord3i(S,T,R) ->
  cast(5268, <<S:?GLint,T:?GLint,R:?GLint>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3i(S,T,R)
-spec texCoord3iv({integer(),integer(),integer()}) -> ok.
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).

%% @spec (S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord3s(integer(),integer(),integer()) -> ok.
texCoord3s(S,T,R) ->
  cast(5269, <<S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3s(S,T,R)
-spec texCoord3sv({integer(),integer(),integer()}) -> ok.
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).

%% @spec (S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord4d(float(),float(),float(),float()) -> ok.
texCoord4d(S,T,R,Q) ->
  cast(5270, <<S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4d(S,T,R,Q)
-spec texCoord4dv({float(),float(),float(),float()}) -> ok.
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).

%% @spec (S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord4f(float(),float(),float(),float()) -> ok.
texCoord4f(S,T,R,Q) ->
  cast(5271, <<S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4f(S,T,R,Q)
-spec texCoord4fv({float(),float(),float(),float()}) -> ok.
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).

%% @spec (S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord4i(integer(),integer(),integer(),integer()) -> ok.
texCoord4i(S,T,R,Q) ->
  cast(5272, <<S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4i(S,T,R,Q)
-spec texCoord4iv({integer(),integer(),integer(),integer()}) -> ok.
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).

%% @spec (S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord4s(integer(),integer(),integer(),integer()) -> ok.
texCoord4s(S,T,R,Q) ->
  cast(5273, <<S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4s(S,T,R,Q)
-spec texCoord4sv({integer(),integer(),integer(),integer()}) -> ok.
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoordPointer.xml">external</a> documentation.
-spec texCoordPointer(integer(),enum(),integer(),offset()|mem()) -> ok.
texCoordPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5274, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
texCoordPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5275, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Target::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvf.xml">external</a> documentation.
-spec texEnvf(enum(),enum(),float()) -> ok.
texEnvf(Target,Pname,Param) ->
  cast(5276, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml">external</a> documentation.
-spec texEnvfv(enum(),enum(),{float()}) -> ok.
texEnvfv(Target,Pname,Params) ->
  cast(5277, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvi.xml">external</a> documentation.
-spec texEnvi(enum(),enum(),integer()) -> ok.
texEnvi(Target,Pname,Param) ->
  cast(5278, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml">external</a> documentation.
-spec texEnviv(enum(),enum(),{integer()}) -> ok.
texEnviv(Target,Pname,Params) ->
  cast(5279, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Coord::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGend(enum(),enum(),float()) -> ok.
texGend(Coord,Pname,Param) ->
  cast(5280, <<Coord:?GLenum,Pname:?GLenum,Param:?GLdouble>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGendv(enum(),enum(),{float()}) -> ok.
texGendv(Coord,Pname,Params) ->
  cast(5281, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,0:32,
      (<< <<C:?GLdouble>> ||C <- tuple_to_list(Params)>>)/binary>>).

%% @spec (Coord::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGenf(enum(),enum(),float()) -> ok.
texGenf(Coord,Pname,Param) ->
  cast(5282, <<Coord:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGenfv(enum(),enum(),{float()}) -> ok.
texGenfv(Coord,Pname,Params) ->
  cast(5283, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Coord::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGeni(enum(),enum(),integer()) -> ok.
texGeni(Coord,Pname,Param) ->
  cast(5284, <<Coord:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
-spec texGeniv(enum(),enum(),{integer()}) -> ok.
texGeniv(Coord,Pname,Params) ->
  cast(5285, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage1D.xml">external</a> documentation.
-spec texImage1D(enum(),integer(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texImage1D(Target,Level,Internalformat,Width,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5286, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage1D(Target,Level,Internalformat,Width,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5287, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2D.xml">external</a> documentation.
-spec texImage2D(enum(),integer(),integer(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texImage2D(Target,Level,Internalformat,Width,Height,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5288, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage2D(Target,Level,Internalformat,Width,Height,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5289, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
-spec texParameterf(enum(),enum(),float()) -> ok.
texParameterf(Target,Pname,Param) ->
  cast(5290, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
-spec texParameterfv(enum(),enum(),{float()}) -> ok.
texParameterfv(Target,Pname,Params) ->
  cast(5291, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
-spec texParameteri(enum(),enum(),integer()) -> ok.
texParameteri(Target,Pname,Param) ->
  cast(5292, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
-spec texParameteriv(enum(),enum(),{integer()}) -> ok.
texParameteriv(Target,Pname,Params) ->
  cast(5293, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Width::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage1D.xml">external</a> documentation.
-spec texSubImage1D(enum(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5294, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5295, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage2D.xml">external</a> documentation.
-spec texSubImage2D(enum(),integer(),integer(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5296, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5297, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml">external</a> documentation.
-spec translated(float(),float(),float()) -> ok.
translated(X,Y,Z) ->
  cast(5298, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml">external</a> documentation.
-spec translatef(float(),float(),float()) -> ok.
translatef(X,Y,Z) ->
  cast(5299, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex2d(float(),float()) -> ok.
vertex2d(X,Y) ->
  cast(5300, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2d(X,Y)
-spec vertex2dv({float(),float()}) -> ok.
vertex2dv({X,Y}) ->  vertex2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex2f(float(),float()) -> ok.
vertex2f(X,Y) ->
  cast(5301, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2f(X,Y)
-spec vertex2fv({float(),float()}) -> ok.
vertex2fv({X,Y}) ->  vertex2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex2i(integer(),integer()) -> ok.
vertex2i(X,Y) ->
  cast(5302, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2i(X,Y)
-spec vertex2iv({integer(),integer()}) -> ok.
vertex2iv({X,Y}) ->  vertex2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex2s(integer(),integer()) -> ok.
vertex2s(X,Y) ->
  cast(5303, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2s(X,Y)
-spec vertex2sv({integer(),integer()}) -> ok.
vertex2sv({X,Y}) ->  vertex2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex3d(float(),float(),float()) -> ok.
vertex3d(X,Y,Z) ->
  cast(5304, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3d(X,Y,Z)
-spec vertex3dv({float(),float(),float()}) -> ok.
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex3f(float(),float(),float()) -> ok.
vertex3f(X,Y,Z) ->
  cast(5305, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3f(X,Y,Z)
-spec vertex3fv({float(),float(),float()}) -> ok.
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex3i(integer(),integer(),integer()) -> ok.
vertex3i(X,Y,Z) ->
  cast(5306, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3i(X,Y,Z)
-spec vertex3iv({integer(),integer(),integer()}) -> ok.
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex3s(integer(),integer(),integer()) -> ok.
vertex3s(X,Y,Z) ->
  cast(5307, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3s(X,Y,Z)
-spec vertex3sv({integer(),integer(),integer()}) -> ok.
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex4d(float(),float(),float(),float()) -> ok.
vertex4d(X,Y,Z,W) ->
  cast(5308, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4d(X,Y,Z,W)
-spec vertex4dv({float(),float(),float(),float()}) -> ok.
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex4f(float(),float(),float(),float()) -> ok.
vertex4f(X,Y,Z,W) ->
  cast(5309, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4f(X,Y,Z,W)
-spec vertex4fv({float(),float(),float(),float()}) -> ok.
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex4i(integer(),integer(),integer(),integer()) -> ok.
vertex4i(X,Y,Z,W) ->
  cast(5310, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4i(X,Y,Z,W)
-spec vertex4iv({integer(),integer(),integer(),integer()}) -> ok.
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
-spec vertex4s(integer(),integer(),integer(),integer()) -> ok.
vertex4s(X,Y,Z,W) ->
  cast(5311, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4s(X,Y,Z,W)
-spec vertex4sv({integer(),integer(),integer(),integer()}) -> ok.
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexPointer.xml">external</a> documentation.
-spec vertexPointer(integer(),enum(),integer(),offset()|mem()) -> ok.
vertexPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5312, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5313, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewport.xml">external</a> documentation.
-spec viewport(integer(),integer(),integer(),integer()) -> ok.
viewport(X,Y,Width,Height) ->
  cast(5314, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Red::clamp(),Green::clamp(),Blue::clamp(),Alpha::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendColor.xml">external</a> documentation.
-spec blendColor(clamp(),clamp(),clamp(),clamp()) -> ok.
blendColor(Red,Green,Blue,Alpha) ->
  cast(5315, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquation.xml">external</a> documentation.
-spec blendEquation(enum()) -> ok.
blendEquation(Mode) ->
  cast(5316, <<Mode:?GLenum>>).

%% @spec (Mode::enum(),Start::integer(),End::integer(),Count::integer(),Type::enum(),Indices::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawRangeElements.xml">external</a> documentation.
-spec drawRangeElements(enum(),integer(),integer(),integer(),enum(),offset()|mem()) -> ok.
drawRangeElements(Mode,Start,End,Count,Type,Indices) when  is_integer(Indices) ->
  cast(5317, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawRangeElements(Mode,Start,End,Count,Type,Indices) ->
  send_bin(Indices),
  cast(5318, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Depth::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3D.xml">external</a> documentation.
-spec texImage3D(enum(),integer(),integer(),integer(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5319, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5320, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Pixels::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage3D.xml">external</a> documentation.
-spec texSubImage3D(enum(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5321, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5322, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage3D.xml">external</a> documentation.
-spec copyTexSubImage3D(enum(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()) -> ok.
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) ->
  cast(5323, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Format::enum(),Type::enum(),Table::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTable.xml">external</a> documentation.
-spec colorTable(enum(),enum(),integer(),enum(),enum(),offset()|mem()) -> ok.
colorTable(Target,Internalformat,Width,Format,Type,Table) when  is_integer(Table) ->
  cast(5324, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Table:?GLuint>>);
colorTable(Target,Internalformat,Width,Format,Type,Table) ->
  send_bin(Table),
  cast(5325, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTableParameter.xml">external</a> documentation.
-spec colorTableParameterfv(enum(),enum(),{float(),float(),float(),float()}) -> ok.
colorTableParameterfv(Target,Pname,{P1,P2,P3,P4}) ->
  cast(5326, <<Target:?GLenum,Pname:?GLenum,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTableParameter.xml">external</a> documentation.
-spec colorTableParameteriv(enum(),enum(),{integer(),integer(),integer(),integer()}) -> ok.
colorTableParameteriv(Target,Pname,{P1,P2,P3,P4}) ->
  cast(5327, <<Target:?GLenum,Pname:?GLenum,P1:?GLint,P2:?GLint,P3:?GLint,P4:?GLint>>).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorTable.xml">external</a> documentation.
-spec copyColorTable(enum(),enum(),integer(),integer(),integer()) -> ok.
copyColorTable(Target,Internalformat,X,Y,Width) ->
  cast(5328, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Format::enum(),Type::enum(),Table::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTable.xml">external</a> documentation.
-spec getColorTable(enum(),enum(),enum(),mem()) -> ok.
getColorTable(Target,Format,Type,Table) ->
  send_bin(Table),
  call(5329, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTableParameter.xml">external</a> documentation.
-spec getColorTableParameterfv(enum(),enum()) -> {float(),float(),float(),float()}.
getColorTableParameterfv(Target,Pname) ->
  call(5330, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTableParameter.xml">external</a> documentation.
-spec getColorTableParameteriv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getColorTableParameteriv(Target,Pname) ->
  call(5331, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Start::integer(),Count::integer(),Format::enum(),Type::enum(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorSubTable.xml">external</a> documentation.
-spec colorSubTable(enum(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
colorSubTable(Target,Start,Count,Format,Type,Data) when  is_integer(Data) ->
  cast(5332, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum,Data:?GLuint>>);
colorSubTable(Target,Start,Count,Format,Type,Data) ->
  send_bin(Data),
  cast(5333, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Start::integer(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorSubTable.xml">external</a> documentation.
-spec copyColorSubTable(enum(),integer(),integer(),integer(),integer()) -> ok.
copyColorSubTable(Target,Start,X,Y,Width) ->
  cast(5334, <<Target:?GLenum,Start:?GLsizei,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Format::enum(),Type::enum(),Image::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter1D.xml">external</a> documentation.
-spec convolutionFilter1D(enum(),enum(),integer(),enum(),enum(),offset()|mem()) -> ok.
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when  is_integer(Image) ->
  cast(5335, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) ->
  send_bin(Image),
  cast(5336, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Image::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter2D.xml">external</a> documentation.
-spec convolutionFilter2D(enum(),enum(),integer(),integer(),enum(),enum(),offset()|mem()) -> ok.
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when  is_integer(Image) ->
  cast(5337, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) ->
  send_bin(Image),
  cast(5338, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionParameter.xml">external</a> documentation.
-spec convolutionParameterf(enum(),enum(),{float()}) -> ok.
convolutionParameterf(Target,Pname,Params) ->
  cast(5339, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target,Pname,{Params}) -> ok
%% @equiv convolutionParameterf(Target,Pname,Params)
-spec convolutionParameterfv(enum(),enum(),{{float()}}) -> ok.
convolutionParameterfv(Target,Pname,{Params}) ->  convolutionParameterf(Target,Pname,Params).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionParameter.xml">external</a> documentation.
-spec convolutionParameteri(enum(),enum(),{integer()}) -> ok.
convolutionParameteri(Target,Pname,Params) ->
  cast(5340, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target,Pname,{Params}) -> ok
%% @equiv convolutionParameteri(Target,Pname,Params)
-spec convolutionParameteriv(enum(),enum(),{{integer()}}) -> ok.
convolutionParameteriv(Target,Pname,{Params}) ->  convolutionParameteri(Target,Pname,Params).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter1D.xml">external</a> documentation.
-spec copyConvolutionFilter1D(enum(),enum(),integer(),integer(),integer()) -> ok.
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) ->
  cast(5341, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter2D.xml">external</a> documentation.
-spec copyConvolutionFilter2D(enum(),enum(),integer(),integer(),integer(),integer()) -> ok.
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) ->
  cast(5342, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Format::enum(),Type::enum(),Image::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionFilter.xml">external</a> documentation.
-spec getConvolutionFilter(enum(),enum(),enum(),mem()) -> ok.
getConvolutionFilter(Target,Format,Type,Image) ->
  send_bin(Image),
  call(5343, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
-spec getConvolutionParameterfv(enum(),enum()) -> {float(),float(),float(),float()}.
getConvolutionParameterfv(Target,Pname) ->
  call(5344, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
-spec getConvolutionParameteriv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getConvolutionParameteriv(Target,Pname) ->
  call(5345, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Row::offset()|mem(),Column::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSeparableFilter2D.xml">external</a> documentation.
-spec separableFilter2D(enum(),enum(),integer(),integer(),enum(),enum(),offset()|mem(),offset()|mem()) -> ok.
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when  is_integer(Row), is_integer(Column) ->
  cast(5346, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Row:?GLuint,Column:?GLuint>>);
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) ->
  send_bin(Row),
  send_bin(Column),
  cast(5347, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Reset::0|1,Format::enum(),Type::enum(),Values::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogram.xml">external</a> documentation.
-spec getHistogram(enum(),0|1,enum(),enum(),mem()) -> ok.
getHistogram(Target,Reset,Format,Type,Values) ->
  send_bin(Values),
  call(5348, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogramParameter.xml">external</a> documentation.
-spec getHistogramParameterfv(enum(),enum()) -> {float()}.
getHistogramParameterfv(Target,Pname) ->
  call(5349, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogramParameter.xml">external</a> documentation.
-spec getHistogramParameteriv(enum(),enum()) -> {integer()}.
getHistogramParameteriv(Target,Pname) ->
  call(5350, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Reset::0|1,Format::enum(),Type::enum(),Values::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmax.xml">external</a> documentation.
-spec getMinmax(enum(),0|1,enum(),enum(),mem()) -> ok.
getMinmax(Target,Reset,Format,Type,Values) ->
  send_bin(Values),
  call(5351, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
-spec getMinmaxParameterfv(enum(),enum()) -> {float()}.
getMinmaxParameterfv(Target,Pname) ->
  call(5352, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
-spec getMinmaxParameteriv(enum(),enum()) -> {integer()}.
getMinmaxParameteriv(Target,Pname) ->
  call(5353, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Width::integer(),Internalformat::enum(),Sink::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHistogram.xml">external</a> documentation.
-spec histogram(enum(),integer(),enum(),0|1) -> ok.
histogram(Target,Width,Internalformat,Sink) ->
  cast(5354, <<Target:?GLenum,Width:?GLsizei,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @spec (Target::enum(),Internalformat::enum(),Sink::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMinmax.xml">external</a> documentation.
-spec minmax(enum(),enum(),0|1) -> ok.
minmax(Target,Internalformat,Sink) ->
  cast(5355, <<Target:?GLenum,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetHistogram.xml">external</a> documentation.
-spec resetHistogram(enum()) -> ok.
resetHistogram(Target) ->
  cast(5356, <<Target:?GLenum>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetMinmax.xml">external</a> documentation.
-spec resetMinmax(enum()) -> ok.
resetMinmax(Target) ->
  cast(5357, <<Target:?GLenum>>).

%% @spec (Texture::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glActiveTexture.xml">external</a> documentation.
-spec activeTexture(enum()) -> ok.
activeTexture(Texture) ->
  cast(5358, <<Texture:?GLenum>>).

%% @spec (Value::clamp(),Invert::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSampleCoverage.xml">external</a> documentation.
-spec sampleCoverage(clamp(),0|1) -> ok.
sampleCoverage(Value,Invert) ->
  cast(5359, <<Value:?GLclampf,Invert:?GLboolean>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Height::integer(),Depth::integer(),Border::integer(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage3D.xml">external</a> documentation.
-spec compressedTexImage3D(enum(),integer(),enum(),integer(),integer(),integer(),integer(),integer(),offset()|mem()) -> ok.
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5360, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5361, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Height::integer(),Border::integer(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage2D.xml">external</a> documentation.
-spec compressedTexImage2D(enum(),integer(),enum(),integer(),integer(),integer(),integer(),offset()|mem()) -> ok.
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5362, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5363, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Border::integer(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage1D.xml">external</a> documentation.
-spec compressedTexImage1D(enum(),integer(),enum(),integer(),integer(),integer(),offset()|mem()) -> ok.
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5364, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5365, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage3D.xml">external</a> documentation.
-spec compressedTexSubImage3D(enum(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),enum(),integer(),offset()|mem()) -> ok.
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5366, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5367, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Width::integer(),Height::integer(),Format::enum(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage2D.xml">external</a> documentation.
-spec compressedTexSubImage2D(enum(),integer(),integer(),integer(),integer(),integer(),enum(),integer(),offset()|mem()) -> ok.
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5368, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5369, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Width::integer(),Format::enum(),ImageSize::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage1D.xml">external</a> documentation.
-spec compressedTexSubImage1D(enum(),integer(),integer(),integer(),enum(),integer(),offset()|mem()) -> ok.
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5370, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5371, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Img::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetCompressedTexImage.xml">external</a> documentation.
-spec getCompressedTexImage(enum(),integer(),mem()) -> ok.
getCompressedTexImage(Target,Level,Img) ->
  send_bin(Img),
  call(5372, <<Target:?GLenum,Level:?GLint>>).

%% @spec (Texture::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClientActiveTexture.xml">external</a> documentation.
-spec clientActiveTexture(enum()) -> ok.
clientActiveTexture(Texture) ->
  cast(5373, <<Texture:?GLenum>>).

%% @spec (Target::enum(),S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord1d(enum(),float()) -> ok.
multiTexCoord1d(Target,S) ->
  cast(5374, <<Target:?GLenum,0:32,S:?GLdouble>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1d(Target,S)
-spec multiTexCoord1dv(enum(),{float()}) -> ok.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).

%% @spec (Target::enum(),S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord1f(enum(),float()) -> ok.
multiTexCoord1f(Target,S) ->
  cast(5375, <<Target:?GLenum,S:?GLfloat>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1f(Target,S)
-spec multiTexCoord1fv(enum(),{float()}) -> ok.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).

%% @spec (Target::enum(),S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord1i(enum(),integer()) -> ok.
multiTexCoord1i(Target,S) ->
  cast(5376, <<Target:?GLenum,S:?GLint>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1i(Target,S)
-spec multiTexCoord1iv(enum(),{integer()}) -> ok.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).

%% @spec (Target::enum(),S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord1s(enum(),integer()) -> ok.
multiTexCoord1s(Target,S) ->
  cast(5377, <<Target:?GLenum,S:?GLshort>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1s(Target,S)
-spec multiTexCoord1sv(enum(),{integer()}) -> ok.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).

%% @spec (Target::enum(),S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord2d(enum(),float(),float()) -> ok.
multiTexCoord2d(Target,S,T) ->
  cast(5378, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2d(Target,S,T)
-spec multiTexCoord2dv(enum(),{float(),float()}) -> ok.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).

%% @spec (Target::enum(),S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord2f(enum(),float(),float()) -> ok.
multiTexCoord2f(Target,S,T) ->
  cast(5379, <<Target:?GLenum,S:?GLfloat,T:?GLfloat>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2f(Target,S,T)
-spec multiTexCoord2fv(enum(),{float(),float()}) -> ok.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).

%% @spec (Target::enum(),S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord2i(enum(),integer(),integer()) -> ok.
multiTexCoord2i(Target,S,T) ->
  cast(5380, <<Target:?GLenum,S:?GLint,T:?GLint>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2i(Target,S,T)
-spec multiTexCoord2iv(enum(),{integer(),integer()}) -> ok.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).

%% @spec (Target::enum(),S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord2s(enum(),integer(),integer()) -> ok.
multiTexCoord2s(Target,S,T) ->
  cast(5381, <<Target:?GLenum,S:?GLshort,T:?GLshort>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2s(Target,S,T)
-spec multiTexCoord2sv(enum(),{integer(),integer()}) -> ok.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).

%% @spec (Target::enum(),S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord3d(enum(),float(),float(),float()) -> ok.
multiTexCoord3d(Target,S,T,R) ->
  cast(5382, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3d(Target,S,T,R)
-spec multiTexCoord3dv(enum(),{float(),float(),float()}) -> ok.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).

%% @spec (Target::enum(),S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord3f(enum(),float(),float(),float()) -> ok.
multiTexCoord3f(Target,S,T,R) ->
  cast(5383, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3f(Target,S,T,R)
-spec multiTexCoord3fv(enum(),{float(),float(),float()}) -> ok.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord3i(enum(),integer(),integer(),integer()) -> ok.
multiTexCoord3i(Target,S,T,R) ->
  cast(5384, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3i(Target,S,T,R)
-spec multiTexCoord3iv(enum(),{integer(),integer(),integer()}) -> ok.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord3s(enum(),integer(),integer(),integer()) -> ok.
multiTexCoord3s(Target,S,T,R) ->
  cast(5385, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3s(Target,S,T,R)
-spec multiTexCoord3sv(enum(),{integer(),integer(),integer()}) -> ok.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).

%% @spec (Target::enum(),S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord4d(enum(),float(),float(),float(),float()) -> ok.
multiTexCoord4d(Target,S,T,R,Q) ->
  cast(5386, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4d(Target,S,T,R,Q)
-spec multiTexCoord4dv(enum(),{float(),float(),float(),float()}) -> ok.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).

%% @spec (Target::enum(),S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord4f(enum(),float(),float(),float(),float()) -> ok.
multiTexCoord4f(Target,S,T,R,Q) ->
  cast(5387, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4f(Target,S,T,R,Q)
-spec multiTexCoord4fv(enum(),{float(),float(),float(),float()}) -> ok.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord4i(enum(),integer(),integer(),integer(),integer()) -> ok.
multiTexCoord4i(Target,S,T,R,Q) ->
  cast(5388, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4i(Target,S,T,R,Q)
-spec multiTexCoord4iv(enum(),{integer(),integer(),integer(),integer()}) -> ok.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord4s(enum(),integer(),integer(),integer(),integer()) -> ok.
multiTexCoord4s(Target,S,T,R,Q) ->
  cast(5389, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4s(Target,S,T,R,Q)
-spec multiTexCoord4sv(enum(),{integer(),integer(),integer(),integer()}) -> ok.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
-spec loadTransposeMatrixf({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
-spec loadTransposeMatrixd({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
-spec multTransposeMatrixf({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
-spec multTransposeMatrixd({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (SfactorRGB::enum(),DfactorRGB::enum(),SfactorAlpha::enum(),DfactorAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFuncSeparate.xml">external</a> documentation.
-spec blendFuncSeparate(enum(),enum(),enum(),enum()) -> ok.
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) ->
  cast(5394, <<SfactorRGB:?GLenum,DfactorRGB:?GLenum,SfactorAlpha:?GLenum,DfactorAlpha:?GLenum>>).

%% @spec (Mode::enum(),First::[integer()],Count::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiDrawArrays.xml">external</a> documentation.
-spec multiDrawArrays(enum(),[integer()],[integer()]) -> ok.
multiDrawArrays(Mode,First,Count) ->
  cast(5395, <<Mode:?GLenum,(length(First)):?GLuint,
        (<< <<C:?GLint>> || C <- First>>)/binary,0:(((length(First)) rem 2)*32),(length(Count)):?GLuint,
        (<< <<C:?GLsizei>> || C <- Count>>)/binary,0:(((1+length(Count)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
-spec pointParameterf(enum(),float()) -> ok.
pointParameterf(Pname,Param) ->
  cast(5396, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
-spec pointParameterfv(enum(),{float()}) -> ok.
pointParameterfv(Pname,Params) ->
  cast(5397, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
-spec pointParameteri(enum(),integer()) -> ok.
pointParameteri(Pname,Param) ->
  cast(5398, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
-spec pointParameteriv(enum(),{integer()}) -> ok.
pointParameteriv(Pname,Params) ->
  cast(5399, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Coord::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoord.xml">external</a> documentation.
-spec fogCoordf(float()) -> ok.
fogCoordf(Coord) ->
  cast(5400, <<Coord:?GLfloat>>).

%% @spec ({Coord}) -> ok
%% @equiv fogCoordf(Coord)
-spec fogCoordfv({float()}) -> ok.
fogCoordfv({Coord}) ->  fogCoordf(Coord).

%% @spec (Coord::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoord.xml">external</a> documentation.
-spec fogCoordd(float()) -> ok.
fogCoordd(Coord) ->
  cast(5401, <<Coord:?GLdouble>>).

%% @spec ({Coord}) -> ok
%% @equiv fogCoordd(Coord)
-spec fogCoorddv({float()}) -> ok.
fogCoorddv({Coord}) ->  fogCoordd(Coord).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoordPointer.xml">external</a> documentation.
-spec fogCoordPointer(enum(),integer(),offset()|mem()) -> ok.
fogCoordPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5402, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
fogCoordPointer(Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5403, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3b(integer(),integer(),integer()) -> ok.
secondaryColor3b(Red,Green,Blue) ->
  cast(5404, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3b(Red,Green,Blue)
-spec secondaryColor3bv({integer(),integer(),integer()}) -> ok.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3d(float(),float(),float()) -> ok.
secondaryColor3d(Red,Green,Blue) ->
  cast(5405, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3d(Red,Green,Blue)
-spec secondaryColor3dv({float(),float(),float()}) -> ok.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3f(float(),float(),float()) -> ok.
secondaryColor3f(Red,Green,Blue) ->
  cast(5406, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3f(Red,Green,Blue)
-spec secondaryColor3fv({float(),float(),float()}) -> ok.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3i(integer(),integer(),integer()) -> ok.
secondaryColor3i(Red,Green,Blue) ->
  cast(5407, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3i(Red,Green,Blue)
-spec secondaryColor3iv({integer(),integer(),integer()}) -> ok.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3s(integer(),integer(),integer()) -> ok.
secondaryColor3s(Red,Green,Blue) ->
  cast(5408, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3s(Red,Green,Blue)
-spec secondaryColor3sv({integer(),integer(),integer()}) -> ok.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3ub(integer(),integer(),integer()) -> ok.
secondaryColor3ub(Red,Green,Blue) ->
  cast(5409, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3ub(Red,Green,Blue)
-spec secondaryColor3ubv({integer(),integer(),integer()}) -> ok.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3ui(integer(),integer(),integer()) -> ok.
secondaryColor3ui(Red,Green,Blue) ->
  cast(5410, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3ui(Red,Green,Blue)
-spec secondaryColor3uiv({integer(),integer(),integer()}) -> ok.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3us(integer(),integer(),integer()) -> ok.
secondaryColor3us(Red,Green,Blue) ->
  cast(5411, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3us(Red,Green,Blue)
-spec secondaryColor3usv({integer(),integer(),integer()}) -> ok.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColorPointer.xml">external</a> documentation.
-spec secondaryColorPointer(integer(),enum(),integer(),offset()|mem()) -> ok.
secondaryColorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5412, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
secondaryColorPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5413, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2d(float(),float()) -> ok.
windowPos2d(X,Y) ->
  cast(5414, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2d(X,Y)
-spec windowPos2dv({float(),float()}) -> ok.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2f(float(),float()) -> ok.
windowPos2f(X,Y) ->
  cast(5415, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2f(X,Y)
-spec windowPos2fv({float(),float()}) -> ok.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2i(integer(),integer()) -> ok.
windowPos2i(X,Y) ->
  cast(5416, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2i(X,Y)
-spec windowPos2iv({integer(),integer()}) -> ok.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2s(integer(),integer()) -> ok.
windowPos2s(X,Y) ->
  cast(5417, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2s(X,Y)
-spec windowPos2sv({integer(),integer()}) -> ok.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos3d(float(),float(),float()) -> ok.
windowPos3d(X,Y,Z) ->
  cast(5418, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3d(X,Y,Z)
-spec windowPos3dv({float(),float(),float()}) -> ok.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos3f(float(),float(),float()) -> ok.
windowPos3f(X,Y,Z) ->
  cast(5419, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3f(X,Y,Z)
-spec windowPos3fv({float(),float(),float()}) -> ok.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos3i(integer(),integer(),integer()) -> ok.
windowPos3i(X,Y,Z) ->
  cast(5420, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3i(X,Y,Z)
-spec windowPos3iv({integer(),integer(),integer()}) -> ok.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos3s(integer(),integer(),integer()) -> ok.
windowPos3s(X,Y,Z) ->
  cast(5421, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3s(X,Y,Z)
-spec windowPos3sv({integer(),integer(),integer()}) -> ok.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenQueries.xml">external</a> documentation.
-spec genQueries(integer()) -> [integer()].
genQueries(N) ->
  call(5422, <<N:?GLsizei>>).

%% @spec (Ids::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteQueries.xml">external</a> documentation.
-spec deleteQueries([integer()]) -> ok.
deleteQueries(Ids) ->
  cast(5423, <<(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+length(Ids)) rem 2)*32)>>).

%% @spec (Id::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsQuery.xml">external</a> documentation.
-spec isQuery(integer()) -> 0|1.
isQuery(Id) ->
  call(5424, <<Id:?GLuint>>).

%% @spec (Target::enum(),Id::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQuery.xml">external</a> documentation.
-spec beginQuery(enum(),integer()) -> ok.
beginQuery(Target,Id) ->
  cast(5425, <<Target:?GLenum,Id:?GLuint>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndQuery.xml">external</a> documentation.
-spec endQuery(enum()) -> ok.
endQuery(Target) ->
  cast(5426, <<Target:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQuery.xml">external</a> documentation.
-spec getQueryiv(enum(),enum()) -> integer().
getQueryiv(Target,Pname) ->
  call(5427, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObject.xml">external</a> documentation.
-spec getQueryObjectiv(integer(),enum()) -> integer().
getQueryObjectiv(Id,Pname) ->
  call(5428, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObject.xml">external</a> documentation.
-spec getQueryObjectuiv(integer(),enum()) -> integer().
getQueryObjectuiv(Id,Pname) ->
  call(5429, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Target::enum(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBuffer.xml">external</a> documentation.
-spec bindBuffer(enum(),integer()) -> ok.
bindBuffer(Target,Buffer) ->
  cast(5430, <<Target:?GLenum,Buffer:?GLuint>>).

%% @spec (Buffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteBuffers.xml">external</a> documentation.
-spec deleteBuffers([integer()]) -> ok.
deleteBuffers(Buffers) ->
  cast(5431, <<(length(Buffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Buffers>>)/binary,0:(((1+length(Buffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenBuffers.xml">external</a> documentation.
-spec genBuffers(integer()) -> [integer()].
genBuffers(N) ->
  call(5432, <<N:?GLsizei>>).

%% @spec (Buffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsBuffer.xml">external</a> documentation.
-spec isBuffer(integer()) -> 0|1.
isBuffer(Buffer) ->
  call(5433, <<Buffer:?GLuint>>).

%% @spec (Target::enum(),Size::integer(),Data::offset()|mem(),Usage::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferData.xml">external</a> documentation.
-spec bufferData(enum(),integer(),offset()|mem(),enum()) -> ok.
bufferData(Target,Size,Data,Usage) when  is_integer(Data) ->
  cast(5434, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Data:?GLuint,Usage:?GLenum>>);
bufferData(Target,Size,Data,Usage) ->
  send_bin(Data),
  cast(5435, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Usage:?GLenum>>).

%% @spec (Target::enum(),Offset::integer(),Size::integer(),Data::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferSubData.xml">external</a> documentation.
-spec bufferSubData(enum(),integer(),integer(),offset()|mem()) -> ok.
bufferSubData(Target,Offset,Size,Data) when  is_integer(Data) ->
  cast(5436, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr,Data:?GLuint>>);
bufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  cast(5437, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Offset::integer(),Size::integer(),Data::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferSubData.xml">external</a> documentation.
-spec getBufferSubData(enum(),integer(),integer(),mem()) -> ok.
getBufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  call(5438, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameteriv.xml">external</a> documentation.
-spec getBufferParameteriv(enum(),enum()) -> integer().
getBufferParameteriv(Target,Pname) ->
  call(5439, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (ModeRGB::enum(),ModeAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquationSeparate.xml">external</a> documentation.
-spec blendEquationSeparate(enum(),enum()) -> ok.
blendEquationSeparate(ModeRGB,ModeAlpha) ->
  cast(5440, <<ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @spec (Bufs::[enum()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffers.xml">external</a> documentation.
-spec drawBuffers([enum()]) -> ok.
drawBuffers(Bufs) ->
  cast(5441, <<(length(Bufs)):?GLuint,
        (<< <<C:?GLenum>> || C <- Bufs>>)/binary,0:(((1+length(Bufs)) rem 2)*32)>>).

%% @spec (Face::enum(),Sfail::enum(),Dpfail::enum(),Dppass::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOpSeparate.xml">external</a> documentation.
-spec stencilOpSeparate(enum(),enum(),enum(),enum()) -> ok.
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) ->
  cast(5442, <<Face:?GLenum,Sfail:?GLenum,Dpfail:?GLenum,Dppass:?GLenum>>).

%% @spec (Face::enum(),Func::enum(),Ref::integer(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFuncSeparate.xml">external</a> documentation.
-spec stencilFuncSeparate(enum(),enum(),integer(),integer()) -> ok.
stencilFuncSeparate(Face,Func,Ref,Mask) ->
  cast(5443, <<Face:?GLenum,Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @spec (Face::enum(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMaskSeparate.xml">external</a> documentation.
-spec stencilMaskSeparate(enum(),integer()) -> ok.
stencilMaskSeparate(Face,Mask) ->
  cast(5444, <<Face:?GLenum,Mask:?GLuint>>).

%% @spec (Program::integer(),Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachShader.xml">external</a> documentation.
-spec attachShader(integer(),integer()) -> ok.
attachShader(Program,Shader) ->
  cast(5445, <<Program:?GLuint,Shader:?GLuint>>).

%% @spec (Program::integer(),Index::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocation.xml">external</a> documentation.
-spec bindAttribLocation(integer(),integer(),string()) -> ok.
bindAttribLocation(Program,Index,Name) ->
  cast(5446, <<Program:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShader.xml">external</a> documentation.
-spec compileShader(integer()) -> ok.
compileShader(Shader) ->
  cast(5447, <<Shader:?GLuint>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml">external</a> documentation.
-spec createProgram() -> integer().
createProgram() ->
  call(5448, <<>>).

%% @spec (Type::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml">external</a> documentation.
-spec createShader(enum()) -> integer().
createShader(Type) ->
  call(5449, <<Type:?GLenum>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgram.xml">external</a> documentation.
-spec deleteProgram(integer()) -> ok.
deleteProgram(Program) ->
  cast(5450, <<Program:?GLuint>>).

%% @spec (Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteShader.xml">external</a> documentation.
-spec deleteShader(integer()) -> ok.
deleteShader(Shader) ->
  cast(5451, <<Shader:?GLuint>>).

%% @spec (Program::integer(),Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachShader.xml">external</a> documentation.
-spec detachShader(integer(),integer()) -> ok.
detachShader(Program,Shader) ->
  cast(5452, <<Program:?GLuint,Shader:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisableVertexAttribArray.xml">external</a> documentation.
-spec disableVertexAttribArray(integer()) -> ok.
disableVertexAttribArray(Index) ->
  cast(5453, <<Index:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml">external</a> documentation.
-spec enableVertexAttribArray(integer()) -> ok.
enableVertexAttribArray(Index) ->
  cast(5454, <<Index:?GLuint>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttrib.xml">external</a> documentation.
-spec getActiveAttrib(integer(),integer(),integer()) -> {integer(),enum(),string()}.
getActiveAttrib(Program,Index,BufSize) ->
  call(5455, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniform.xml">external</a> documentation.
-spec getActiveUniform(integer(),integer(),integer()) -> {integer(),enum(),string()}.
getActiveUniform(Program,Index,BufSize) ->
  call(5456, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),MaxCount::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedShaders.xml">external</a> documentation.
-spec getAttachedShaders(integer(),integer()) -> [integer()].
getAttachedShaders(Program,MaxCount) ->
  call(5457, <<Program:?GLuint,MaxCount:?GLsizei>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocation.xml">external</a> documentation.
-spec getAttribLocation(integer(),string()) -> integer().
getAttribLocation(Program,Name) ->
  call(5458, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgram.xml">external</a> documentation.
-spec getProgramiv(integer(),enum()) -> integer().
getProgramiv(Program,Pname) ->
  call(5459, <<Program:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramInfoLog.xml">external</a> documentation.
-spec getProgramInfoLog(integer(),integer()) -> string().
getProgramInfoLog(Program,BufSize) ->
  call(5460, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @spec (Shader::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShader.xml">external</a> documentation.
-spec getShaderiv(integer(),enum()) -> integer().
getShaderiv(Shader,Pname) ->
  call(5461, <<Shader:?GLuint,Pname:?GLenum>>).

%% @spec (Shader::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderInfoLog.xml">external</a> documentation.
-spec getShaderInfoLog(integer(),integer()) -> string().
getShaderInfoLog(Shader,BufSize) ->
  call(5462, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @spec (Shader::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSource.xml">external</a> documentation.
-spec getShaderSource(integer(),integer()) -> string().
getShaderSource(Shader,BufSize) ->
  call(5463, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocation.xml">external</a> documentation.
-spec getUniformLocation(integer(),string()) -> integer().
getUniformLocation(Program,Name) ->
  call(5464, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Location::integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
-spec getUniformfv(integer(),integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}.
getUniformfv(Program,Location) ->
  call(5465, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Program::integer(),Location::integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
-spec getUniformiv(integer(),integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}.
getUniformiv(Program,Location) ->
  call(5466, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Index::integer(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
-spec getVertexAttribdv(integer(),enum()) -> {float(),float(),float(),float()}.
getVertexAttribdv(Index,Pname) ->
  call(5467, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
-spec getVertexAttribfv(integer(),enum()) -> {float(),float(),float(),float()}.
getVertexAttribfv(Index,Pname) ->
  call(5468, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
-spec getVertexAttribiv(integer(),enum()) -> {integer(),integer(),integer(),integer()}.
getVertexAttribiv(Index,Pname) ->
  call(5469, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsProgram.xml">external</a> documentation.
-spec isProgram(integer()) -> 0|1.
isProgram(Program) ->
  call(5470, <<Program:?GLuint>>).

%% @spec (Shader::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsShader.xml">external</a> documentation.
-spec isShader(integer()) -> 0|1.
isShader(Shader) ->
  call(5471, <<Shader:?GLuint>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgram.xml">external</a> documentation.
-spec linkProgram(integer()) -> ok.
linkProgram(Program) ->
  cast(5472, <<Program:?GLuint>>).

%% @spec (Shader::integer(),String::[string()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSource.xml">external</a> documentation.
-spec shaderSource(integer(),[string()]) -> ok.
shaderSource(Shader,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  cast(5473, <<Shader:?GLuint,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+0) rem 8)) rem 8)>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgram.xml">external</a> documentation.
-spec useProgram(integer()) -> ok.
useProgram(Program) ->
  cast(5474, <<Program:?GLuint>>).

%% @spec (Location::integer(),V0::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1f(integer(),float()) -> ok.
uniform1f(Location,V0) ->
  cast(5475, <<Location:?GLint,V0:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2f(integer(),float(),float()) -> ok.
uniform2f(Location,V0,V1) ->
  cast(5476, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3f(integer(),float(),float(),float()) -> ok.
uniform3f(Location,V0,V1,V2) ->
  cast(5477, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float(),V2::float(),V3::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4f(integer(),float(),float(),float(),float()) -> ok.
uniform4f(Location,V0,V1,V2,V3) ->
  cast(5478, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @spec (Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1i(integer(),integer()) -> ok.
uniform1i(Location,V0) ->
  cast(5479, <<Location:?GLint,V0:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2i(integer(),integer(),integer()) -> ok.
uniform2i(Location,V0,V1) ->
  cast(5480, <<Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3i(integer(),integer(),integer(),integer()) -> ok.
uniform3i(Location,V0,V1,V2) ->
  cast(5481, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4i(integer(),integer(),integer(),integer(),integer()) -> ok.
uniform4i(Location,V0,V1,V2,V3) ->
  cast(5482, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @spec (Location::integer(),Value::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1fv(integer(),[float()]) -> ok.
uniform1fv(Location,Value) ->
  cast(5483, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2fv(integer(),[{float(),float()}]) -> ok.
uniform2fv(Location,Value) ->
  cast(5484, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3fv(integer(),[{float(),float(),float()}]) -> ok.
uniform3fv(Location,Value) ->
  cast(5485, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4fv(integer(),[{float(),float(),float(),float()}]) -> ok.
uniform4fv(Location,Value) ->
  cast(5486, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1iv(integer(),[integer()]) -> ok.
uniform1iv(Location,Value) ->
  cast(5487, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2iv(integer(),[{integer(),integer()}]) -> ok.
uniform2iv(Location,Value) ->
  cast(5488, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3iv(integer(),[{integer(),integer(),integer()}]) -> ok.
uniform3iv(Location,Value) ->
  cast(5489, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer(),integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4iv(integer(),[{integer(),integer(),integer(),integer()}]) -> ok.
uniform4iv(Location,Value) ->
  cast(5490, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix2fv(integer(),0|1,[{float(),float(),float(),float()}]) -> ok.
uniformMatrix2fv(Location,Transpose,Value) ->
  cast(5491, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix3fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3fv(Location,Transpose,Value) ->
  cast(5492, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix4fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4fv(Location,Transpose,Value) ->
  cast(5493, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgram.xml">external</a> documentation.
-spec validateProgram(integer()) -> ok.
validateProgram(Program) ->
  cast(5494, <<Program:?GLuint>>).

%% @spec (Index::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib1d(integer(),float()) -> ok.
vertexAttrib1d(Index,X) ->
  cast(5495, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1d(Index,X)
-spec vertexAttrib1dv(integer(),{float()}) -> ok.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).

%% @spec (Index::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib1f(integer(),float()) -> ok.
vertexAttrib1f(Index,X) ->
  cast(5496, <<Index:?GLuint,X:?GLfloat>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1f(Index,X)
-spec vertexAttrib1fv(integer(),{float()}) -> ok.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib1s(integer(),integer()) -> ok.
vertexAttrib1s(Index,X) ->
  cast(5497, <<Index:?GLuint,X:?GLshort>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1s(Index,X)
-spec vertexAttrib1sv(integer(),{integer()}) -> ok.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).

%% @spec (Index::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib2d(integer(),float(),float()) -> ok.
vertexAttrib2d(Index,X,Y) ->
  cast(5498, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2d(Index,X,Y)
-spec vertexAttrib2dv(integer(),{float(),float()}) -> ok.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).

%% @spec (Index::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib2f(integer(),float(),float()) -> ok.
vertexAttrib2f(Index,X,Y) ->
  cast(5499, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2f(Index,X,Y)
-spec vertexAttrib2fv(integer(),{float(),float()}) -> ok.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib2s(integer(),integer(),integer()) -> ok.
vertexAttrib2s(Index,X,Y) ->
  cast(5500, <<Index:?GLuint,X:?GLshort,Y:?GLshort>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2s(Index,X,Y)
-spec vertexAttrib2sv(integer(),{integer(),integer()}) -> ok.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib3d(integer(),float(),float(),float()) -> ok.
vertexAttrib3d(Index,X,Y,Z) ->
  cast(5501, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3d(Index,X,Y,Z)
-spec vertexAttrib3dv(integer(),{float(),float(),float()}) -> ok.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib3f(integer(),float(),float(),float()) -> ok.
vertexAttrib3f(Index,X,Y,Z) ->
  cast(5502, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3f(Index,X,Y,Z)
-spec vertexAttrib3fv(integer(),{float(),float(),float()}) -> ok.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib3s(integer(),integer(),integer(),integer()) -> ok.
vertexAttrib3s(Index,X,Y,Z) ->
  cast(5503, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3s(Index,X,Y,Z)
-spec vertexAttrib3sv(integer(),{integer(),integer(),integer()}) -> ok.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Nbv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Nbv(Index,{V1,V2,V3,V4}) ->
  cast(5504, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Niv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Niv(Index,{V1,V2,V3,V4}) ->
  cast(5505, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Nsv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Nsv(Index,{V1,V2,V3,V4}) ->
  cast(5506, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Nub(integer(),integer(),integer(),integer(),integer()) -> ok.
vertexAttrib4Nub(Index,X,Y,Z,W) ->
  cast(5507, <<Index:?GLuint,X:?GLubyte,Y:?GLubyte,Z:?GLubyte,W:?GLubyte>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
-spec vertexAttrib4Nubv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Nuiv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Nuiv(Index,{V1,V2,V3,V4}) ->
  cast(5508, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4Nusv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4Nusv(Index,{V1,V2,V3,V4}) ->
  cast(5509, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4bv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4bv(Index,{V1,V2,V3,V4}) ->
  cast(5510, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4d(integer(),float(),float(),float(),float()) -> ok.
vertexAttrib4d(Index,X,Y,Z,W) ->
  cast(5511, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
-spec vertexAttrib4dv(integer(),{float(),float(),float(),float()}) -> ok.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4f(integer(),float(),float(),float(),float()) -> ok.
vertexAttrib4f(Index,X,Y,Z,W) ->
  cast(5512, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
-spec vertexAttrib4fv(integer(),{float(),float(),float(),float()}) -> ok.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4iv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4iv(Index,{V1,V2,V3,V4}) ->
  cast(5513, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4s(integer(),integer(),integer(),integer(),integer()) -> ok.
vertexAttrib4s(Index,X,Y,Z,W) ->
  cast(5514, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
-spec vertexAttrib4sv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4ubv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5515, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4uiv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4uiv(Index,{V1,V2,V3,V4}) ->
  cast(5516, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib4usv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttrib4usv(Index,{V1,V2,V3,V4}) ->
  cast(5517, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Index::integer(),Size::integer(),Type::enum(),Normalized::0|1,Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml">external</a> documentation.
-spec vertexAttribPointer(integer(),integer(),enum(),0|1,integer(),offset()|mem()) -> ok.
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5518, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5519, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
-spec uniformMatrix2x3fv(integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix2x3fv(Location,Transpose,Value) ->
  cast(5520, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
-spec uniformMatrix3x2fv(integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3x2fv(Location,Transpose,Value) ->
  cast(5521, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
-spec uniformMatrix2x4fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix2x4fv(Location,Transpose,Value) ->
  cast(5522, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
-spec uniformMatrix4x2fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4x2fv(Location,Transpose,Value) ->
  cast(5523, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
-spec uniformMatrix3x4fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3x4fv(Location,Transpose,Value) ->
  cast(5524, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
-spec uniformMatrix4x3fv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4x3fv(Location,Transpose,Value) ->
  cast(5525, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Index::integer(),R::0|1,G::0|1,B::0|1,A::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaski.xml">external</a> documentation.
-spec colorMaski(integer(),0|1,0|1,0|1,0|1) -> ok.
colorMaski(Index,R,G,B,A) ->
  cast(5526, <<Index:?GLuint,R:?GLboolean,G:?GLboolean,B:?GLboolean,A:?GLboolean>>).

%% @spec (Target::enum(),Index::integer()) -> [0|1]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBooleani_v.xml">external</a> documentation.
-spec getBooleani_v(enum(),integer()) -> [0|1].
getBooleani_v(Target,Index) ->
  call(5527, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetIntegeri_v.xml">external</a> documentation.
-spec getIntegeri_v(enum(),integer()) -> [integer()].
getIntegeri_v(Target,Index) ->
  call(5528, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml">external</a> documentation.
-spec enablei(enum(),integer()) -> ok.
enablei(Target,Index) ->
  cast(5529, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisable.xml">external</a> documentation.
-spec disablei(enum(),integer()) -> ok.
disablei(Target,Index) ->
  cast(5530, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabledi.xml">external</a> documentation.
-spec isEnabledi(enum(),integer()) -> 0|1.
isEnabledi(Target,Index) ->
  call(5531, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (PrimitiveMode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginTransformFeedback.xml">external</a> documentation.
-spec beginTransformFeedback(enum()) -> ok.
beginTransformFeedback(PrimitiveMode) ->
  cast(5532, <<PrimitiveMode:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndTransformFeedback.xml">external</a> documentation.
-spec endTransformFeedback() -> ok.
endTransformFeedback() ->
  cast(5533, <<>>).

%% @spec (Target::enum(),Index::integer(),Buffer::integer(),Offset::integer(),Size::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferRange.xml">external</a> documentation.
-spec bindBufferRange(enum(),integer(),integer(),integer(),integer()) -> ok.
bindBufferRange(Target,Index,Buffer,Offset,Size) ->
  cast(5534, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Index::integer(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferBase.xml">external</a> documentation.
-spec bindBufferBase(enum(),integer(),integer()) -> ok.
bindBufferBase(Target,Index,Buffer) ->
  cast(5535, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint>>).

%% @spec (Program::integer(),Varyings::[string()],BufferMode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTransformFeedbackVaryings.xml">external</a> documentation.
-spec transformFeedbackVaryings(integer(),[string()],enum()) -> ok.
transformFeedbackVaryings(Program,Varyings,BufferMode) ->
 VaryingsTemp = list_to_binary([[Str|[0]] || Str <- Varyings ]),
  cast(5536, <<Program:?GLuint,(length(Varyings)):?GLuint,(size(VaryingsTemp)):?GLuint,(VaryingsTemp)/binary,0:((8-((size(VaryingsTemp)+0) rem 8)) rem 8),BufferMode:?GLenum>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTransformFeedbackVarying.xml">external</a> documentation.
-spec getTransformFeedbackVarying(integer(),integer(),integer()) -> {integer(),enum(),string()}.
getTransformFeedbackVarying(Program,Index,BufSize) ->
  call(5537, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Target::enum(),Clamp::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClampColor.xml">external</a> documentation.
-spec clampColor(enum(),enum()) -> ok.
clampColor(Target,Clamp) ->
  cast(5538, <<Target:?GLenum,Clamp:?GLenum>>).

%% @spec (Id::integer(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginConditionalRender.xml">external</a> documentation.
-spec beginConditionalRender(integer(),enum()) -> ok.
beginConditionalRender(Id,Mode) ->
  cast(5539, <<Id:?GLuint,Mode:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndConditionalRender.xml">external</a> documentation.
-spec endConditionalRender() -> ok.
endConditionalRender() ->
  cast(5540, <<>>).

%% @spec (Index::integer(),Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribIPointer.xml">external</a> documentation.
-spec vertexAttribIPointer(integer(),integer(),enum(),integer(),offset()|mem()) -> ok.
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5541, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5542, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribI.xml">external</a> documentation.
-spec getVertexAttribIiv(integer(),enum()) -> {integer(),integer(),integer(),integer()}.
getVertexAttribIiv(Index,Pname) ->
  call(5543, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribI.xml">external</a> documentation.
-spec getVertexAttribIuiv(integer(),enum()) -> {integer(),integer(),integer(),integer()}.
getVertexAttribIuiv(Index,Pname) ->
  call(5544, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI1i(integer(),integer()) -> ok.
vertexAttribI1i(Index,X) ->
  cast(5545, <<Index:?GLuint,X:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI2i(integer(),integer(),integer()) -> ok.
vertexAttribI2i(Index,X,Y) ->
  cast(5546, <<Index:?GLuint,X:?GLint,Y:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI3i(integer(),integer(),integer(),integer()) -> ok.
vertexAttribI3i(Index,X,Y,Z) ->
  cast(5547, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4i(integer(),integer(),integer(),integer(),integer()) -> ok.
vertexAttribI4i(Index,X,Y,Z,W) ->
  cast(5548, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI1ui(integer(),integer()) -> ok.
vertexAttribI1ui(Index,X) ->
  cast(5549, <<Index:?GLuint,X:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI2ui(integer(),integer(),integer()) -> ok.
vertexAttribI2ui(Index,X,Y) ->
  cast(5550, <<Index:?GLuint,X:?GLuint,Y:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI3ui(integer(),integer(),integer(),integer()) -> ok.
vertexAttribI3ui(Index,X,Y,Z) ->
  cast(5551, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4ui(integer(),integer(),integer(),integer(),integer()) -> ok.
vertexAttribI4ui(Index,X,Y,Z,W) ->
  cast(5552, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint,W:?GLuint>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttribI1i(Index,X)
-spec vertexAttribI1iv(integer(),{integer()}) -> ok.
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttribI2i(Index,X,Y)
-spec vertexAttribI2iv(integer(),{integer(),integer()}) -> ok.
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttribI3i(Index,X,Y,Z)
-spec vertexAttribI3iv(integer(),{integer(),integer(),integer()}) -> ok.
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttribI4i(Index,X,Y,Z,W)
-spec vertexAttribI4iv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttribI1ui(Index,X)
-spec vertexAttribI1uiv(integer(),{integer()}) -> ok.
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttribI2ui(Index,X,Y)
-spec vertexAttribI2uiv(integer(),{integer(),integer()}) -> ok.
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttribI3ui(Index,X,Y,Z)
-spec vertexAttribI3uiv(integer(),{integer(),integer(),integer()}) -> ok.
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttribI4ui(Index,X,Y,Z,W)
-spec vertexAttribI4uiv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4bv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4bv(Index,{V1,V2,V3,V4}) ->
  cast(5553, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4sv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4sv(Index,{V1,V2,V3,V4}) ->
  cast(5554, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4ubv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5555, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
-spec vertexAttribI4usv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
vertexAttribI4usv(Index,{V1,V2,V3,V4}) ->
  cast(5556, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Program::integer(),Location::integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
-spec getUniformuiv(integer(),integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}.
getUniformuiv(Program,Location) ->
  call(5557, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Program::integer(),Color::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFragDataLocation.xml">external</a> documentation.
-spec bindFragDataLocation(integer(),integer(),string()) -> ok.
bindFragDataLocation(Program,Color,Name) ->
  cast(5558, <<Program:?GLuint,Color:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFragDataLocation.xml">external</a> documentation.
-spec getFragDataLocation(integer(),string()) -> integer().
getFragDataLocation(Program,Name) ->
  call(5559, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1ui(integer(),integer()) -> ok.
uniform1ui(Location,V0) ->
  cast(5560, <<Location:?GLint,V0:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2ui(integer(),integer(),integer()) -> ok.
uniform2ui(Location,V0,V1) ->
  cast(5561, <<Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3ui(integer(),integer(),integer(),integer()) -> ok.
uniform3ui(Location,V0,V1,V2) ->
  cast(5562, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4ui(integer(),integer(),integer(),integer(),integer()) -> ok.
uniform4ui(Location,V0,V1,V2,V3) ->
  cast(5563, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @spec (Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1uiv(integer(),[integer()]) -> ok.
uniform1uiv(Location,Value) ->
  cast(5564, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2uiv(integer(),[{integer(),integer()}]) -> ok.
uniform2uiv(Location,Value) ->
  cast(5565, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3uiv(integer(),[{integer(),integer(),integer()}]) -> ok.
uniform3uiv(Location,Value) ->
  cast(5566, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer(),integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4uiv(integer(),[{integer(),integer(),integer(),integer()}]) -> ok.
uniform4uiv(Location,Value) ->
  cast(5567, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameterI.xml">external</a> documentation.
-spec texParameterIiv(enum(),enum(),{integer()}) -> ok.
texParameterIiv(Target,Pname,Params) ->
  cast(5568, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameterI.xml">external</a> documentation.
-spec texParameterIuiv(enum(),enum(),{integer()}) -> ok.
texParameterIuiv(Target,Pname,Params) ->
  cast(5569, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameterI.xml">external</a> documentation.
-spec getTexParameterIiv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getTexParameterIiv(Target,Pname) ->
  call(5570, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameterI.xml">external</a> documentation.
-spec getTexParameterIuiv(enum(),enum()) -> {integer(),integer(),integer(),integer()}.
getTexParameterIuiv(Target,Pname) ->
  call(5571, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
-spec clearBufferiv(enum(),integer(),{integer()}) -> ok.
clearBufferiv(Buffer,Drawbuffer,Value) ->
  cast(5572, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
-spec clearBufferuiv(enum(),integer(),{integer()}) -> ok.
clearBufferuiv(Buffer,Drawbuffer,Value) ->
  cast(5573, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
-spec clearBufferfv(enum(),integer(),{float()}) -> ok.
clearBufferfv(Buffer,Drawbuffer,Value) ->
  cast(5574, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Depth::float(),Stencil::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBufferfi.xml">external</a> documentation.
-spec clearBufferfi(enum(),integer(),float(),integer()) -> ok.
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) ->
  cast(5575, <<Buffer:?GLenum,Drawbuffer:?GLint,Depth:?GLfloat,Stencil:?GLint>>).

%% @spec (Name::enum(),Index::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml">external</a> documentation.
-spec getStringi(enum(),integer()) -> string().
getStringi(Name,Index) ->
  call(5576, <<Name:?GLenum,Index:?GLuint>>).

%% @spec (Mode::enum(),First::integer(),Count::integer(),Primcount::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysInstance.xml">external</a> documentation.
-spec drawArraysInstanced(enum(),integer(),integer(),integer()) -> ok.
drawArraysInstanced(Mode,First,Count,Primcount) ->
  cast(5577, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|mem(),Primcount::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstance.xml">external</a> documentation.
-spec drawElementsInstanced(enum(),integer(),enum(),offset()|mem(),integer()) -> ok.
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) when  is_integer(Indices) ->
  cast(5578, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei>>);
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) ->
  send_bin(Indices),
  cast(5579, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexBuffer.xml">external</a> documentation.
-spec texBuffer(enum(),enum(),integer()) -> ok.
texBuffer(Target,Internalformat,Buffer) ->
  cast(5580, <<Target:?GLenum,Internalformat:?GLenum,Buffer:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrimitiveRestartIndex.xml">external</a> documentation.
-spec primitiveRestartIndex(integer()) -> ok.
primitiveRestartIndex(Index) ->
  cast(5581, <<Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInteger64i_v.xml">external</a> documentation.
-spec getInteger64i_v(enum(),integer()) -> [integer()].
getInteger64i_v(Target,Index) ->
  call(5582, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameteri64v.xml">external</a> documentation.
-spec getBufferParameteri64v(enum(),enum()) -> [integer()].
getBufferParameteri64v(Target,Pname) ->
  call(5583, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture.xml">external</a> documentation.
-spec framebufferTexture(enum(),enum(),integer(),integer()) -> ok.
framebufferTexture(Target,Attachment,Texture,Level) ->
  cast(5584, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Index::integer(),Divisor::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribDivisor.xml">external</a> documentation.
-spec vertexAttribDivisor(integer(),integer()) -> ok.
vertexAttribDivisor(Index,Divisor) ->
  cast(5585, <<Index:?GLuint,Divisor:?GLuint>>).

%% @spec (Value::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMinSampleShading.xml">external</a> documentation.
-spec minSampleShading(clamp()) -> ok.
minSampleShading(Value) ->
  cast(5586, <<Value:?GLclampf>>).

%% @spec (Buf::integer(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquation.xml">external</a> documentation.
-spec blendEquationi(integer(),enum()) -> ok.
blendEquationi(Buf,Mode) ->
  cast(5587, <<Buf:?GLuint,Mode:?GLenum>>).

%% @spec (Buf::integer(),ModeRGB::enum(),ModeAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquationSeparate.xml">external</a> documentation.
-spec blendEquationSeparatei(integer(),enum(),enum()) -> ok.
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) ->
  cast(5588, <<Buf:?GLuint,ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @spec (Buf::integer(),Src::enum(),Dst::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunci.xml">external</a> documentation.
-spec blendFunci(integer(),enum(),enum()) -> ok.
blendFunci(Buf,Src,Dst) ->
  cast(5589, <<Buf:?GLuint,Src:?GLenum,Dst:?GLenum>>).

%% @spec (Buf::integer(),SrcRGB::enum(),DstRGB::enum(),SrcAlpha::enum(),DstAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFuncSeparate.xml">external</a> documentation.
-spec blendFuncSeparatei(integer(),enum(),enum(),enum(),enum()) -> ok.
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) ->
  cast(5590, <<Buf:?GLuint,SrcRGB:?GLenum,DstRGB:?GLenum,SrcAlpha:?GLenum,DstAlpha:?GLenum>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
-spec loadTransposeMatrixfARB({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5591, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5591, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
-spec loadTransposeMatrixdARB({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5592, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5592, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
-spec multTransposeMatrixfARB({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5593, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5593, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
-spec multTransposeMatrixdARB({float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}) -> ok.
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5594, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5594, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightbvARB([integer()]) -> ok.
weightbvARB(Weights) ->
  cast(5595, <<(length(Weights)):?GLuint,
        (<< <<C:?GLbyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightsvARB([integer()]) -> ok.
weightsvARB(Weights) ->
  cast(5596, <<(length(Weights)):?GLuint,
        (<< <<C:?GLshort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightivARB([integer()]) -> ok.
weightivARB(Weights) ->
  cast(5597, <<(length(Weights)):?GLuint,
        (<< <<C:?GLint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Weights::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightfvARB([float()]) -> ok.
weightfvARB(Weights) ->
  cast(5598, <<(length(Weights)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Weights::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightdvARB([float()]) -> ok.
weightdvARB(Weights) ->
  cast(5599, <<(length(Weights)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Weights>>)/binary>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightubvARB([integer()]) -> ok.
weightubvARB(Weights) ->
  cast(5600, <<(length(Weights)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightusvARB([integer()]) -> ok.
weightusvARB(Weights) ->
  cast(5601, <<(length(Weights)):?GLuint,
        (<< <<C:?GLushort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightuivARB([integer()]) -> ok.
weightuivARB(Weights) ->
  cast(5602, <<(length(Weights)):?GLuint,
        (<< <<C:?GLuint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Count::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexBlenARB.xml">external</a> documentation.
-spec vertexBlendARB(integer()) -> ok.
vertexBlendARB(Count) ->
  cast(5603, <<Count:?GLint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCurrentPaletteMatrixARB.xml">external</a> documentation.
-spec currentPaletteMatrixARB(integer()) -> ok.
currentPaletteMatrixARB(Index) ->
  cast(5604, <<Index:?GLint>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexubvARB([integer()]) -> ok.
matrixIndexubvARB(Indices) ->
  cast(5605, <<(length(Indices)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Indices>>)/binary,0:((8-((length(Indices)+ 4) rem 8)) rem 8)>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexusvARB([integer()]) -> ok.
matrixIndexusvARB(Indices) ->
  cast(5606, <<(length(Indices)):?GLuint,
        (<< <<C:?GLushort>> || C <- Indices>>)/binary,0:((8-((length(Indices)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexuivARB([integer()]) -> ok.
matrixIndexuivARB(Indices) ->
  cast(5607, <<(length(Indices)):?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((1+length(Indices)) rem 2)*32)>>).

%% @spec (Target::enum(),Format::enum(),String::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramStringARB.xml">external</a> documentation.
-spec programStringARB(enum(),enum(),string()) -> ok.
programStringARB(Target,Format,String) ->
  cast(5608, <<Target:?GLenum,Format:?GLenum,(list_to_binary([String|[0]]))/binary,0:((8-((length(String)+ 1) rem 8)) rem 8)>>).

%% @spec (Target::enum(),Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindProgramARB.xml">external</a> documentation.
-spec bindProgramARB(enum(),integer()) -> ok.
bindProgramARB(Target,Program) ->
  cast(5609, <<Target:?GLenum,Program:?GLuint>>).

%% @spec (Programs::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgramsARB.xml">external</a> documentation.
-spec deleteProgramsARB([integer()]) -> ok.
deleteProgramsARB(Programs) ->
  cast(5610, <<(length(Programs)):?GLuint,
        (<< <<C:?GLuint>> || C <- Programs>>)/binary,0:(((1+length(Programs)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenProgramsARB.xml">external</a> documentation.
-spec genProgramsARB(integer()) -> [integer()].
genProgramsARB(N) ->
  call(5611, <<N:?GLsizei>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4dARB(enum(),integer(),float(),float(),float(),float()) -> ok.
programEnvParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5612, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),Params::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4dvARB(enum(),integer(),{float(),float(),float(),float()}) -> ok.
programEnvParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5613, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4fARB(enum(),integer(),float(),float(),float(),float()) -> ok.
programEnvParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5614, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),Params::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4fvARB(enum(),integer(),{float(),float(),float(),float()}) -> ok.
programEnvParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5615, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4dARB(enum(),integer(),float(),float(),float(),float()) -> ok.
programLocalParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5616, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),Params::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4dvARB(enum(),integer(),{float(),float(),float(),float()}) -> ok.
programLocalParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5617, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4fARB(enum(),integer(),float(),float(),float(),float()) -> ok.
programLocalParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5618, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),Params::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4fvARB(enum(),integer(),{float(),float(),float(),float()}) -> ok.
programLocalParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5619, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Index::integer()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
-spec getProgramEnvParameterdvARB(enum(),integer()) -> {float(),float(),float(),float()}.
getProgramEnvParameterdvARB(Target,Index) ->
  call(5620, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
-spec getProgramEnvParameterfvARB(enum(),integer()) -> {float(),float(),float(),float()}.
getProgramEnvParameterfvARB(Target,Index) ->
  call(5621, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
-spec getProgramLocalParameterdvARB(enum(),integer()) -> {float(),float(),float(),float()}.
getProgramLocalParameterdvARB(Target,Index) ->
  call(5622, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
-spec getProgramLocalParameterfvARB(enum(),integer()) -> {float(),float(),float(),float()}.
getProgramLocalParameterfvARB(Target,Index) ->
  call(5623, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Pname::enum(),String::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramStringARB.xml">external</a> documentation.
-spec getProgramStringARB(enum(),enum(),mem()) -> ok.
getProgramStringARB(Target,Pname,String) ->
  send_bin(String),
  call(5624, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameterARB.xml">external</a> documentation.
-spec getBufferParameterivARB(enum(),enum()) -> [integer()].
getBufferParameterivARB(Target,Pname) ->
  call(5625, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Obj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteObjectARB.xml">external</a> documentation.
-spec deleteObjectARB(integer()) -> ok.
deleteObjectARB(Obj) ->
  cast(5626, <<Obj:?GLhandleARB>>).

%% @spec (Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHandleARB.xml">external</a> documentation.
-spec getHandleARB(enum()) -> integer().
getHandleARB(Pname) ->
  call(5627, <<Pname:?GLenum>>).

%% @spec (ContainerObj::integer(),AttachedObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachObjectARB.xml">external</a> documentation.
-spec detachObjectARB(integer(),integer()) -> ok.
detachObjectARB(ContainerObj,AttachedObj) ->
  cast(5628, <<ContainerObj:?GLhandleARB,AttachedObj:?GLhandleARB>>).

%% @spec (ShaderType::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShaderObjectARB.xml">external</a> documentation.
-spec createShaderObjectARB(enum()) -> integer().
createShaderObjectARB(ShaderType) ->
  call(5629, <<ShaderType:?GLenum>>).

%% @spec (ShaderObj::integer(),String::[string()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSourceARB.xml">external</a> documentation.
-spec shaderSourceARB(integer(),[string()]) -> ok.
shaderSourceARB(ShaderObj,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  cast(5630, <<ShaderObj:?GLhandleARB,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+4) rem 8)) rem 8)>>).

%% @spec (ShaderObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShaderARB.xml">external</a> documentation.
-spec compileShaderARB(integer()) -> ok.
compileShaderARB(ShaderObj) ->
  cast(5631, <<ShaderObj:?GLhandleARB>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgramObjectARB.xml">external</a> documentation.
-spec createProgramObjectARB() -> integer().
createProgramObjectARB() ->
  call(5632, <<>>).

%% @spec (ContainerObj::integer(),Obj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachObjectARB.xml">external</a> documentation.
-spec attachObjectARB(integer(),integer()) -> ok.
attachObjectARB(ContainerObj,Obj) ->
  cast(5633, <<ContainerObj:?GLhandleARB,Obj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgramARB.xml">external</a> documentation.
-spec linkProgramARB(integer()) -> ok.
linkProgramARB(ProgramObj) ->
  cast(5634, <<ProgramObj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgramObjectARB.xml">external</a> documentation.
-spec useProgramObjectARB(integer()) -> ok.
useProgramObjectARB(ProgramObj) ->
  cast(5635, <<ProgramObj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgramARB.xml">external</a> documentation.
-spec validateProgramARB(integer()) -> ok.
validateProgramARB(ProgramObj) ->
  cast(5636, <<ProgramObj:?GLhandleARB>>).

%% @spec (Obj::integer(),Pname::enum()) -> float()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
-spec getObjectParameterfvARB(integer(),enum()) -> float().
getObjectParameterfvARB(Obj,Pname) ->
  call(5637, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @spec (Obj::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
-spec getObjectParameterivARB(integer(),enum()) -> integer().
getObjectParameterivARB(Obj,Pname) ->
  call(5638, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @spec (Obj::integer(),MaxLength::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInfoLogARB.xml">external</a> documentation.
-spec getInfoLogARB(integer(),integer()) -> string().
getInfoLogARB(Obj,MaxLength) ->
  call(5639, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @spec (ContainerObj::integer(),MaxCount::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedObjectsARB.xml">external</a> documentation.
-spec getAttachedObjectsARB(integer(),integer()) -> [integer()].
getAttachedObjectsARB(ContainerObj,MaxCount) ->
  call(5640, <<ContainerObj:?GLhandleARB,MaxCount:?GLsizei>>).

%% @spec (ProgramObj::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocationARB.xml">external</a> documentation.
-spec getUniformLocationARB(integer(),string()) -> integer().
getUniformLocationARB(ProgramObj,Name) ->
  call(5641, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (ProgramObj::integer(),Index::integer(),MaxLength::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformARB.xml">external</a> documentation.
-spec getActiveUniformARB(integer(),integer(),integer()) -> {integer(),enum(),string()}.
getActiveUniformARB(ProgramObj,Index,MaxLength) ->
  call(5642, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Location::integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
-spec getUniformfvARB(integer(),integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}.
getUniformfvARB(ProgramObj,Location) ->
  call(5643, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @spec (ProgramObj::integer(),Location::integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
-spec getUniformivARB(integer(),integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}.
getUniformivARB(ProgramObj,Location) ->
  call(5644, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @spec (Obj::integer(),MaxLength::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSourceARB.xml">external</a> documentation.
-spec getShaderSourceARB(integer(),integer()) -> string().
getShaderSourceARB(Obj,MaxLength) ->
  call(5645, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Index::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocationARB.xml">external</a> documentation.
-spec bindAttribLocationARB(integer(),integer(),string()) -> ok.
bindAttribLocationARB(ProgramObj,Index,Name) ->
  cast(5646, <<ProgramObj:?GLhandleARB,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (ProgramObj::integer(),Index::integer(),MaxLength::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttribARB.xml">external</a> documentation.
-spec getActiveAttribARB(integer(),integer(),integer()) -> {integer(),enum(),string()}.
getActiveAttribARB(ProgramObj,Index,MaxLength) ->
  call(5647, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocationARB.xml">external</a> documentation.
-spec getAttribLocationARB(integer(),string()) -> integer().
getAttribLocationARB(ProgramObj,Name) ->
  call(5648, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Renderbuffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsRenderbuffer.xml">external</a> documentation.
-spec isRenderbuffer(integer()) -> 0|1.
isRenderbuffer(Renderbuffer) ->
  call(5649, <<Renderbuffer:?GLuint>>).

%% @spec (Target::enum(),Renderbuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindRenderbuffer.xml">external</a> documentation.
-spec bindRenderbuffer(enum(),integer()) -> ok.
bindRenderbuffer(Target,Renderbuffer) ->
  cast(5650, <<Target:?GLenum,Renderbuffer:?GLuint>>).

%% @spec (Renderbuffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteRenderbuffers.xml">external</a> documentation.
-spec deleteRenderbuffers([integer()]) -> ok.
deleteRenderbuffers(Renderbuffers) ->
  cast(5651, <<(length(Renderbuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Renderbuffers>>)/binary,0:(((1+length(Renderbuffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenRenderbuffers.xml">external</a> documentation.
-spec genRenderbuffers(integer()) -> [integer()].
genRenderbuffers(N) ->
  call(5652, <<N:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorage.xml">external</a> documentation.
-spec renderbufferStorage(enum(),enum(),integer(),integer()) -> ok.
renderbufferStorage(Target,Internalformat,Width,Height) ->
  cast(5653, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetRenderbufferParameter.xml">external</a> documentation.
-spec getRenderbufferParameteriv(enum(),enum()) -> integer().
getRenderbufferParameteriv(Target,Pname) ->
  call(5654, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Framebuffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsFramebuffer.xml">external</a> documentation.
-spec isFramebuffer(integer()) -> 0|1.
isFramebuffer(Framebuffer) ->
  call(5655, <<Framebuffer:?GLuint>>).

%% @spec (Target::enum(),Framebuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFramebuffer.xml">external</a> documentation.
-spec bindFramebuffer(enum(),integer()) -> ok.
bindFramebuffer(Target,Framebuffer) ->
  cast(5656, <<Target:?GLenum,Framebuffer:?GLuint>>).

%% @spec (Framebuffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteFramebuffers.xml">external</a> documentation.
-spec deleteFramebuffers([integer()]) -> ok.
deleteFramebuffers(Framebuffers) ->
  cast(5657, <<(length(Framebuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Framebuffers>>)/binary,0:(((1+length(Framebuffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenFramebuffers.xml">external</a> documentation.
-spec genFramebuffers(integer()) -> [integer()].
genFramebuffers(N) ->
  call(5658, <<N:?GLsizei>>).

%% @spec (Target::enum()) -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCheckFramebufferStatus.xml">external</a> documentation.
-spec checkFramebufferStatus(enum()) -> enum().
checkFramebufferStatus(Target) ->
  call(5659, <<Target:?GLenum>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture1D.xml">external</a> documentation.
-spec framebufferTexture1D(enum(),enum(),enum(),integer(),integer()) -> ok.
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5660, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture2D.xml">external</a> documentation.
-spec framebufferTexture2D(enum(),enum(),enum(),integer(),integer()) -> ok.
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5661, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer(),Zoffset::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture3D.xml">external</a> documentation.
-spec framebufferTexture3D(enum(),enum(),enum(),integer(),integer(),integer()) -> ok.
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) ->
  cast(5662, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint,Zoffset:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Renderbuffertarget::enum(),Renderbuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferRenderbuffer.xml">external</a> documentation.
-spec framebufferRenderbuffer(enum(),enum(),enum(),integer()) -> ok.
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) ->
  cast(5663, <<Target:?GLenum,Attachment:?GLenum,Renderbuffertarget:?GLenum,Renderbuffer:?GLuint>>).

%% @spec (Target::enum(),Attachment::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFramebufferAttachmentParameter.xml">external</a> documentation.
-spec getFramebufferAttachmentParameteriv(enum(),enum(),enum()) -> integer().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) ->
  call(5664, <<Target:?GLenum,Attachment:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenerateMipmap.xml">external</a> documentation.
-spec generateMipmap(enum()) -> ok.
generateMipmap(Target) ->
  cast(5665, <<Target:?GLenum>>).

%% @spec (SrcX0::integer(),SrcY0::integer(),SrcX1::integer(),SrcY1::integer(),DstX0::integer(),DstY0::integer(),DstX1::integer(),DstY1::integer(),Mask::integer(),Filter::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlitFramebuffer.xml">external</a> documentation.
-spec blitFramebuffer(integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),enum()) -> ok.
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) ->
  cast(5666, <<SrcX0:?GLint,SrcY0:?GLint,SrcX1:?GLint,SrcY1:?GLint,DstX0:?GLint,DstY0:?GLint,DstX1:?GLint,DstY1:?GLint,Mask:?GLbitfield,Filter:?GLenum>>).

%% @spec (Target::enum(),Samples::integer(),Internalformat::enum(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorageMultisample.xml">external</a> documentation.
-spec renderbufferStorageMultisample(enum(),integer(),enum(),integer(),integer()) -> ok.
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) ->
  cast(5667, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer(),Layer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTextureLayer.xml">external</a> documentation.
-spec framebufferTextureLayer(enum(),enum(),integer(),integer(),integer()) -> ok.
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) ->
  cast(5668, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Layer:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer(),Face::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTextureFaceARB.xml">external</a> documentation.
-spec framebufferTextureFaceARB(enum(),enum(),integer(),integer(),enum()) -> ok.
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) ->
  cast(5669, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Face:?GLenum>>).

%% @spec (Target::enum(),Offset::integer(),Length::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlushMappedBufferRange.xml">external</a> documentation.
-spec flushMappedBufferRange(enum(),integer(),integer()) -> ok.
flushMappedBufferRange(Target,Offset,Length) ->
  cast(5670, <<Target:?GLenum,0:32,Offset:?GLintptr,Length:?GLsizeiptr>>).

%% @spec (Array::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindVertexArray.xml">external</a> documentation.
-spec bindVertexArray(integer()) -> ok.
bindVertexArray(Array) ->
  cast(5671, <<Array:?GLuint>>).

%% @spec (Arrays::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteVertexArrays.xml">external</a> documentation.
-spec deleteVertexArrays([integer()]) -> ok.
deleteVertexArrays(Arrays) ->
  cast(5672, <<(length(Arrays)):?GLuint,
        (<< <<C:?GLuint>> || C <- Arrays>>)/binary,0:(((1+length(Arrays)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenVertexArrays.xml">external</a> documentation.
-spec genVertexArrays(integer()) -> [integer()].
genVertexArrays(N) ->
  call(5673, <<N:?GLsizei>>).

%% @spec (Array::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsVertexArray.xml">external</a> documentation.
-spec isVertexArray(integer()) -> 0|1.
isVertexArray(Array) ->
  call(5674, <<Array:?GLuint>>).

%% @spec (Program::integer(),UniformNames::[string()]) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformIndices.xml">external</a> documentation.
-spec getUniformIndices(integer(),[string()]) -> [integer()].
getUniformIndices(Program,UniformNames) ->
 UniformNamesTemp = list_to_binary([[Str|[0]] || Str <- UniformNames ]),
  call(5675, <<Program:?GLuint,(length(UniformNames)):?GLuint,(size(UniformNamesTemp)):?GLuint,(UniformNamesTemp)/binary,0:((8-((size(UniformNamesTemp)+0) rem 8)) rem 8)>>).

%% @spec (Program::integer(),UniformIndices::[integer()],Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniforms.xml">external</a> documentation.
-spec getActiveUniformsiv(integer(),[integer()],enum()) -> [integer()].
getActiveUniformsiv(Program,UniformIndices,Pname) ->
  call(5676, <<Program:?GLuint,(length(UniformIndices)):?GLuint,
        (<< <<C:?GLuint>> || C <- UniformIndices>>)/binary,0:(((length(UniformIndices)) rem 2)*32),Pname:?GLenum>>).

%% @spec (Program::integer(),UniformIndex::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformName.xml">external</a> documentation.
-spec getActiveUniformName(integer(),integer(),integer()) -> string().
getActiveUniformName(Program,UniformIndex,BufSize) ->
  call(5677, <<Program:?GLuint,UniformIndex:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),UniformBlockName::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformBlockIndex.xml">external</a> documentation.
-spec getUniformBlockIndex(integer(),string()) -> integer().
getUniformBlockIndex(Program,UniformBlockName) ->
  call(5678, <<Program:?GLuint,(list_to_binary([UniformBlockName|[0]]))/binary,0:((8-((length(UniformBlockName)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),Pname::enum(),Params::mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlock.xml">external</a> documentation.
-spec getActiveUniformBlockiv(integer(),integer(),enum(),mem()) -> ok.
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) ->
  send_bin(Params),
  call(5679, <<Program:?GLuint,UniformBlockIndex:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlockName.xml">external</a> documentation.
-spec getActiveUniformBlockName(integer(),integer(),integer()) -> string().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) ->
  call(5680, <<Program:?GLuint,UniformBlockIndex:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),UniformBlockBinding::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformBlockBinding.xml">external</a> documentation.
-spec uniformBlockBinding(integer(),integer(),integer()) -> ok.
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) ->
  cast(5681, <<Program:?GLuint,UniformBlockIndex:?GLuint,UniformBlockBinding:?GLuint>>).

%% @spec (ReadTarget::enum(),WriteTarget::enum(),ReadOffset::integer(),WriteOffset::integer(),Size::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyBufferSubData.xml">external</a> documentation.
-spec copyBufferSubData(enum(),enum(),integer(),integer(),integer()) -> ok.
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) ->
  cast(5682, <<ReadTarget:?GLenum,WriteTarget:?GLenum,ReadOffset:?GLintptr,WriteOffset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|mem(),Basevertex::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsBaseVertex.xml">external</a> documentation.
-spec drawElementsBaseVertex(enum(),integer(),enum(),offset()|mem(),integer()) -> ok.
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5683, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5684, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

%% @spec (Mode::enum(),Start::integer(),End::integer(),Count::integer(),Type::enum(),Indices::offset()|mem(),Basevertex::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawRangeElementsBaseVertex.xml">external</a> documentation.
-spec drawRangeElementsBaseVertex(enum(),integer(),integer(),integer(),enum(),offset()|mem(),integer()) -> ok.
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5685, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5686, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|mem(),Primcount::integer(),Basevertex::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstancedBaseVertex.xml">external</a> documentation.
-spec drawElementsInstancedBaseVertex(enum(),integer(),enum(),offset()|mem(),integer(),integer()) -> ok.
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) when  is_integer(Indices) ->
  cast(5687, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Basevertex:?GLint>>);
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) ->
  send_bin(Indices),
  cast(5688, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Basevertex:?GLint>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProvokingVertex.xml">external</a> documentation.
-spec provokingVertex(enum()) -> ok.
provokingVertex(Mode) ->
  cast(5689, <<Mode:?GLenum>>).

%% @spec (Condition::enum(),Flags::integer()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFenceSync.xml">external</a> documentation.
-spec fenceSync(enum(),integer()) -> integer().
fenceSync(Condition,Flags) ->
  call(5690, <<Condition:?GLenum,Flags:?GLbitfield>>).

%% @spec (Sync::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsSync.xml">external</a> documentation.
-spec isSync(integer()) -> 0|1.
isSync(Sync) ->
  call(5691, <<Sync:?GLsync>>).

%% @spec (Sync::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteSync.xml">external</a> documentation.
-spec deleteSync(integer()) -> ok.
deleteSync(Sync) ->
  cast(5692, <<Sync:?GLsync>>).

%% @spec (Sync::integer(),Flags::integer(),Timeout::integer()) -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClientWaitSync.xml">external</a> documentation.
-spec clientWaitSync(integer(),integer(),integer()) -> enum().
clientWaitSync(Sync,Flags,Timeout) ->
  call(5693, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @spec (Sync::integer(),Flags::integer(),Timeout::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWaitSync.xml">external</a> documentation.
-spec waitSync(integer(),integer(),integer()) -> ok.
waitSync(Sync,Flags,Timeout) ->
  cast(5694, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @spec (Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInteger64v.xml">external</a> documentation.
-spec getInteger64v(enum()) -> [integer()].
getInteger64v(Pname) ->
  call(5695, <<Pname:?GLenum>>).

%% @spec (Sync::integer(),Pname::enum(),BufSize::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSync.xml">external</a> documentation.
-spec getSynciv(integer(),enum(),integer()) -> [integer()].
getSynciv(Sync,Pname,BufSize) ->
  call(5696, <<Sync:?GLsync,Pname:?GLenum,BufSize:?GLsizei>>).

%% @spec (Target::enum(),Samples::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Fixedsamplelocations::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2DMultisample.xml">external</a> documentation.
-spec texImage2DMultisample(enum(),integer(),integer(),integer(),integer(),0|1) -> ok.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) ->
  cast(5697, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Fixedsamplelocations:?GLboolean>>).

%% @spec (Target::enum(),Samples::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Depth::integer(),Fixedsamplelocations::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3DMultisample.xml">external</a> documentation.
-spec texImage3DMultisample(enum(),integer(),integer(),integer(),integer(),integer(),0|1) -> ok.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) ->
  cast(5698, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Fixedsamplelocations:?GLboolean>>).

%% @spec (Pname::enum(),Index::integer()) -> {float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMultisample.xml">external</a> documentation.
-spec getMultisamplefv(enum(),integer()) -> {float(),float()}.
getMultisamplefv(Pname,Index) ->
  call(5699, <<Pname:?GLenum,Index:?GLuint>>).

%% @spec (Index::integer(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSampleMaski.xml">external</a> documentation.
-spec sampleMaski(integer(),integer()) -> ok.
sampleMaski(Index,Mask) ->
  cast(5700, <<Index:?GLuint,Mask:?GLbitfield>>).

%% @spec (Type::enum(),Name::string(),String::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNamedStringARB.xml">external</a> documentation.
-spec namedStringARB(enum(),string(),string()) -> ok.
namedStringARB(Type,Name,String) ->
  cast(5701, <<Type:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8),(list_to_binary([String|[0]]))/binary,0:((8-((length(String)+ 1) rem 8)) rem 8)>>).

%% @spec (Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteNamedStringARB.xml">external</a> documentation.
-spec deleteNamedStringARB(string()) -> ok.
deleteNamedStringARB(Name) ->
  cast(5702, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Shader::integer(),Path::[string()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShaderIncludeARB.xml">external</a> documentation.
-spec compileShaderIncludeARB(integer(),[string()]) -> ok.
compileShaderIncludeARB(Shader,Path) ->
 PathTemp = list_to_binary([[Str|[0]] || Str <- Path ]),
  cast(5703, <<Shader:?GLuint,(length(Path)):?GLuint,(size(PathTemp)):?GLuint,(PathTemp)/binary,0:((8-((size(PathTemp)+0) rem 8)) rem 8)>>).

%% @spec (Name::string()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsNamedStringARB.xml">external</a> documentation.
-spec isNamedStringARB(string()) -> 0|1.
isNamedStringARB(Name) ->
  call(5704, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Name::string(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetNamedStringARB.xml">external</a> documentation.
-spec getNamedStringARB(string(),integer()) -> string().
getNamedStringARB(Name,BufSize) ->
  call(5705, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8),BufSize:?GLsizei>>).

%% @spec (Name::string(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetNamedStringARB.xml">external</a> documentation.
-spec getNamedStringivARB(string(),enum()) -> integer().
getNamedStringivARB(Name,Pname) ->
  call(5706, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8),Pname:?GLenum>>).

%% @spec (Program::integer(),ColorNumber::integer(),Index::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFragDataLocationIndexe.xml">external</a> documentation.
-spec bindFragDataLocationIndexed(integer(),integer(),integer(),string()) -> ok.
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) ->
  cast(5707, <<Program:?GLuint,ColorNumber:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFragDataIndex.xml">external</a> documentation.
-spec getFragDataIndex(integer(),string()) -> integer().
getFragDataIndex(Program,Name) ->
  call(5708, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Count::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenSamplers.xml">external</a> documentation.
-spec genSamplers(integer()) -> [integer()].
genSamplers(Count) ->
  call(5709, <<Count:?GLsizei>>).

%% @spec (Samplers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteSamplers.xml">external</a> documentation.
-spec deleteSamplers([integer()]) -> ok.
deleteSamplers(Samplers) ->
  cast(5710, <<(length(Samplers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Samplers>>)/binary,0:(((1+length(Samplers)) rem 2)*32)>>).

%% @spec (Sampler::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsSampler.xml">external</a> documentation.
-spec isSampler(integer()) -> 0|1.
isSampler(Sampler) ->
  call(5711, <<Sampler:?GLuint>>).

%% @spec (Unit::integer(),Sampler::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindSampler.xml">external</a> documentation.
-spec bindSampler(integer(),integer()) -> ok.
bindSampler(Unit,Sampler) ->
  cast(5712, <<Unit:?GLuint,Sampler:?GLuint>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameter.xml">external</a> documentation.
-spec samplerParameteri(integer(),enum(),integer()) -> ok.
samplerParameteri(Sampler,Pname,Param) ->
  cast(5713, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLint>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameter.xml">external</a> documentation.
-spec samplerParameteriv(integer(),enum(),[integer()]) -> ok.
samplerParameteriv(Sampler,Pname,Param) ->
  cast(5714, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameter.xml">external</a> documentation.
-spec samplerParameterf(integer(),enum(),float()) -> ok.
samplerParameterf(Sampler,Pname,Param) ->
  cast(5715, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameter.xml">external</a> documentation.
-spec samplerParameterfv(integer(),enum(),[float()]) -> ok.
samplerParameterfv(Sampler,Pname,Param) ->
  cast(5716, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameterI.xml">external</a> documentation.
-spec samplerParameterIiv(integer(),enum(),[integer()]) -> ok.
samplerParameterIiv(Sampler,Pname,Param) ->
  cast(5717, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @spec (Sampler::integer(),Pname::enum(),Param::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameterI.xml">external</a> documentation.
-spec samplerParameterIuiv(integer(),enum(),[integer()]) -> ok.
samplerParameterIuiv(Sampler,Pname,Param) ->
  cast(5718, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLuint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @spec (Sampler::integer(),Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameter.xml">external</a> documentation.
-spec getSamplerParameteriv(integer(),enum()) -> [integer()].
getSamplerParameteriv(Sampler,Pname) ->
  call(5719, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @spec (Sampler::integer(),Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameterI.xml">external</a> documentation.
-spec getSamplerParameterIiv(integer(),enum()) -> [integer()].
getSamplerParameterIiv(Sampler,Pname) ->
  call(5720, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @spec (Sampler::integer(),Pname::enum()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameter.xml">external</a> documentation.
-spec getSamplerParameterfv(integer(),enum()) -> [float()].
getSamplerParameterfv(Sampler,Pname) ->
  call(5721, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @spec (Sampler::integer(),Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameterI.xml">external</a> documentation.
-spec getSamplerParameterIuiv(integer(),enum()) -> [integer()].
getSamplerParameterIuiv(Sampler,Pname) ->
  call(5722, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @spec (Id::integer(),Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glQueryCounter.xml">external</a> documentation.
-spec queryCounter(integer(),enum()) -> ok.
queryCounter(Id,Target) ->
  cast(5723, <<Id:?GLuint,Target:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObjecti64v.xml">external</a> documentation.
-spec getQueryObjecti64v(integer(),enum()) -> integer().
getQueryObjecti64v(Id,Pname) ->
  call(5724, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObjectui64v.xml">external</a> documentation.
-spec getQueryObjectui64v(integer(),enum()) -> integer().
getQueryObjectui64v(Id,Pname) ->
  call(5725, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Mode::enum(),Indirect::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysIndirect.xml">external</a> documentation.
-spec drawArraysIndirect(enum(),offset()|mem()) -> ok.
drawArraysIndirect(Mode,Indirect) when  is_integer(Indirect) ->
  cast(5726, <<Mode:?GLenum,Indirect:?GLuint>>);
drawArraysIndirect(Mode,Indirect) ->
  send_bin(Indirect),
  cast(5727, <<Mode:?GLenum>>).

%% @spec (Mode::enum(),Type::enum(),Indirect::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsIndirect.xml">external</a> documentation.
-spec drawElementsIndirect(enum(),enum(),offset()|mem()) -> ok.
drawElementsIndirect(Mode,Type,Indirect) when  is_integer(Indirect) ->
  cast(5728, <<Mode:?GLenum,Type:?GLenum,Indirect:?GLuint>>);
drawElementsIndirect(Mode,Type,Indirect) ->
  send_bin(Indirect),
  cast(5729, <<Mode:?GLenum,Type:?GLenum>>).

%% @spec (Location::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1d(integer(),float()) -> ok.
uniform1d(Location,X) ->
  cast(5730, <<Location:?GLint,0:32,X:?GLdouble>>).

%% @spec (Location::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2d(integer(),float(),float()) -> ok.
uniform2d(Location,X,Y) ->
  cast(5731, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @spec (Location::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3d(integer(),float(),float(),float()) -> ok.
uniform3d(Location,X,Y,Z) ->
  cast(5732, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Location::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4d(integer(),float(),float(),float(),float()) -> ok.
uniform4d(Location,X,Y,Z,W) ->
  cast(5733, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Location::integer(),Value::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1dv(integer(),[float()]) -> ok.
uniform1dv(Location,Value) ->
  cast(5734, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform2dv(integer(),[{float(),float()}]) -> ok.
uniform2dv(Location,Value) ->
  cast(5735, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform3dv(integer(),[{float(),float(),float()}]) -> ok.
uniform3dv(Location,Value) ->
  cast(5736, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform4dv(integer(),[{float(),float(),float(),float()}]) -> ok.
uniform4dv(Location,Value) ->
  cast(5737, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix2dv(integer(),0|1,[{float(),float(),float(),float()}]) -> ok.
uniformMatrix2dv(Location,Transpose,Value) ->
  cast(5738, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix3dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3dv(Location,Transpose,Value) ->
  cast(5739, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
-spec uniformMatrix4dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4dv(Location,Transpose,Value) ->
  cast(5740, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
-spec uniformMatrix2x3dv(integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix2x3dv(Location,Transpose,Value) ->
  cast(5741, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
-spec uniformMatrix2x4dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix2x4dv(Location,Transpose,Value) ->
  cast(5742, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
-spec uniformMatrix3x2dv(integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3x2dv(Location,Transpose,Value) ->
  cast(5743, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
-spec uniformMatrix3x4dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix3x4dv(Location,Transpose,Value) ->
  cast(5744, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
-spec uniformMatrix4x2dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4x2dv(Location,Transpose,Value) ->
  cast(5745, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
-spec uniformMatrix4x3dv(integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
uniformMatrix4x3dv(Location,Transpose,Value) ->
  cast(5746, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
-spec getUniformdv(integer(),integer()) -> {float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}.
getUniformdv(Program,Location) ->
  call(5747, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Program::integer(),Shadertype::enum(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSubroutineUniformLocation.xml">external</a> documentation.
-spec getSubroutineUniformLocation(integer(),enum(),string()) -> integer().
getSubroutineUniformLocation(Program,Shadertype,Name) ->
  call(5748, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Shadertype::enum(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSubroutineIndex.xml">external</a> documentation.
-spec getSubroutineIndex(integer(),enum(),string()) -> integer().
getSubroutineIndex(Program,Shadertype,Name) ->
  call(5749, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Shadertype::enum(),Index::integer(),Bufsize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveSubroutineUniformName.xml">external</a> documentation.
-spec getActiveSubroutineUniformName(integer(),enum(),integer(),integer()) -> string().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) ->
  call(5750, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @spec (Program::integer(),Shadertype::enum(),Index::integer(),Bufsize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveSubroutineName.xml">external</a> documentation.
-spec getActiveSubroutineName(integer(),enum(),integer(),integer()) -> string().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) ->
  call(5751, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @spec (Shadertype::enum(),Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformSubroutines.xml">external</a> documentation.
-spec uniformSubroutinesuiv(enum(),[integer()]) -> ok.
uniformSubroutinesuiv(Shadertype,Indices) ->
  cast(5752, <<Shadertype:?GLenum,(length(Indices)):?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((length(Indices)) rem 2)*32)>>).

%% @spec (Shadertype::enum(),Location::integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformSubroutine.xml">external</a> documentation.
-spec getUniformSubroutineuiv(enum(),integer()) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()}.
getUniformSubroutineuiv(Shadertype,Location) ->
  call(5753, <<Shadertype:?GLenum,Location:?GLint>>).

%% @spec (Program::integer(),Shadertype::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramStage.xml">external</a> documentation.
-spec getProgramStageiv(integer(),enum(),enum()) -> integer().
getProgramStageiv(Program,Shadertype,Pname) ->
  call(5754, <<Program:?GLuint,Shadertype:?GLenum,Pname:?GLenum>>).

%% @spec (Pname::enum(),Value::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPatchParameter.xml">external</a> documentation.
-spec patchParameteri(enum(),integer()) -> ok.
patchParameteri(Pname,Value) ->
  cast(5755, <<Pname:?GLenum,Value:?GLint>>).

%% @spec (Pname::enum(),Values::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPatchParameter.xml">external</a> documentation.
-spec patchParameterfv(enum(),[float()]) -> ok.
patchParameterfv(Pname,Values) ->
  cast(5756, <<Pname:?GLenum,(length(Values)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Values>>)/binary,0:(((length(Values)) rem 2)*32)>>).

%% @spec (Target::enum(),Id::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindTransformFeedback.xml">external</a> documentation.
-spec bindTransformFeedback(enum(),integer()) -> ok.
bindTransformFeedback(Target,Id) ->
  cast(5757, <<Target:?GLenum,Id:?GLuint>>).

%% @spec (Ids::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTransformFeedbacks.xml">external</a> documentation.
-spec deleteTransformFeedbacks([integer()]) -> ok.
deleteTransformFeedbacks(Ids) ->
  cast(5758, <<(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+length(Ids)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenTransformFeedbacks.xml">external</a> documentation.
-spec genTransformFeedbacks(integer()) -> [integer()].
genTransformFeedbacks(N) ->
  call(5759, <<N:?GLsizei>>).

%% @spec (Id::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsTransformFeedback.xml">external</a> documentation.
-spec isTransformFeedback(integer()) -> 0|1.
isTransformFeedback(Id) ->
  call(5760, <<Id:?GLuint>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPauseTransformFeedback.xml">external</a> documentation.
-spec pauseTransformFeedback() -> ok.
pauseTransformFeedback() ->
  cast(5761, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResumeTransformFeedback.xml">external</a> documentation.
-spec resumeTransformFeedback() -> ok.
resumeTransformFeedback() ->
  cast(5762, <<>>).

%% @spec (Mode::enum(),Id::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedback.xml">external</a> documentation.
-spec drawTransformFeedback(enum(),integer()) -> ok.
drawTransformFeedback(Mode,Id) ->
  cast(5763, <<Mode:?GLenum,Id:?GLuint>>).

%% @spec (Mode::enum(),Id::integer(),Stream::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedbackStream.xml">external</a> documentation.
-spec drawTransformFeedbackStream(enum(),integer(),integer()) -> ok.
drawTransformFeedbackStream(Mode,Id,Stream) ->
  cast(5764, <<Mode:?GLenum,Id:?GLuint,Stream:?GLuint>>).

%% @spec (Target::enum(),Index::integer(),Id::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQueryIndexe.xml">external</a> documentation.
-spec beginQueryIndexed(enum(),integer(),integer()) -> ok.
beginQueryIndexed(Target,Index,Id) ->
  cast(5765, <<Target:?GLenum,Index:?GLuint,Id:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndQueryIndexe.xml">external</a> documentation.
-spec endQueryIndexed(enum(),integer()) -> ok.
endQueryIndexed(Target,Index) ->
  cast(5766, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryIndexed.xml">external</a> documentation.
-spec getQueryIndexediv(enum(),integer(),enum()) -> integer().
getQueryIndexediv(Target,Index,Pname) ->
  call(5767, <<Target:?GLenum,Index:?GLuint,Pname:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReleaseShaderCompiler.xml">external</a> documentation.
-spec releaseShaderCompiler() -> ok.
releaseShaderCompiler() ->
  cast(5768, <<>>).

%% @spec (Shaders::[integer()],Binaryformat::enum(),Binary::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderBinary.xml">external</a> documentation.
-spec shaderBinary([integer()],enum(),binary()) -> ok.
shaderBinary(Shaders,Binaryformat,Binary) ->
  send_bin(Binary),
  cast(5769, <<(length(Shaders)):?GLuint,
        (<< <<C:?GLuint>> || C <- Shaders>>)/binary,0:(((1+length(Shaders)) rem 2)*32),Binaryformat:?GLenum>>).

%% @spec (Shadertype::enum(),Precisiontype::enum()) -> {Range::{integer(),integer()},Precision::integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderPrecisionFormat.xml">external</a> documentation.
-spec getShaderPrecisionFormat(enum(),enum()) -> {{integer(),integer()},integer()}.
getShaderPrecisionFormat(Shadertype,Precisiontype) ->
  call(5770, <<Shadertype:?GLenum,Precisiontype:?GLenum>>).

%% @spec (N::clamp(),F::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRange.xml">external</a> documentation.
-spec depthRangef(clamp(),clamp()) -> ok.
depthRangef(N,F) ->
  cast(5771, <<N:?GLclampf,F:?GLclampf>>).

%% @spec (D::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearDepthf.xml">external</a> documentation.
-spec clearDepthf(clamp()) -> ok.
clearDepthf(D) ->
  cast(5772, <<D:?GLclampf>>).

%% @spec (Program::integer(),BufSize::integer()) -> {BinaryFormat::enum(),Binary::binary()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramBinary.xml">external</a> documentation.
-spec getProgramBinary(integer(),integer()) -> {enum(),binary()}.
getProgramBinary(Program,BufSize) ->
  call(5773, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),BinaryFormat::enum(),Binary::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramBinary.xml">external</a> documentation.
-spec programBinary(integer(),enum(),binary()) -> ok.
programBinary(Program,BinaryFormat,Binary) ->
  send_bin(Binary),
  cast(5774, <<Program:?GLuint,BinaryFormat:?GLenum>>).

%% @spec (Program::integer(),Pname::enum(),Value::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramParameter.xml">external</a> documentation.
-spec programParameteri(integer(),enum(),integer()) -> ok.
programParameteri(Program,Pname,Value) ->
  cast(5775, <<Program:?GLuint,Pname:?GLenum,Value:?GLint>>).

%% @spec (Pipeline::integer(),Stages::integer(),Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgramStages.xml">external</a> documentation.
-spec useProgramStages(integer(),integer(),integer()) -> ok.
useProgramStages(Pipeline,Stages,Program) ->
  cast(5776, <<Pipeline:?GLuint,Stages:?GLbitfield,Program:?GLuint>>).

%% @spec (Pipeline::integer(),Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glActiveShaderProgram.xml">external</a> documentation.
-spec activeShaderProgram(integer(),integer()) -> ok.
activeShaderProgram(Pipeline,Program) ->
  cast(5777, <<Pipeline:?GLuint,Program:?GLuint>>).

%% @spec (Type::enum(),Strings::[string()]) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShaderProgramv.xml">external</a> documentation.
-spec createShaderProgramv(enum(),[string()]) -> integer().
createShaderProgramv(Type,Strings) ->
 StringsTemp = list_to_binary([[Str|[0]] || Str <- Strings ]),
  call(5778, <<Type:?GLenum,(length(Strings)):?GLuint,(size(StringsTemp)):?GLuint,(StringsTemp)/binary,0:((8-((size(StringsTemp)+0) rem 8)) rem 8)>>).

%% @spec (Pipeline::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindProgramPipeline.xml">external</a> documentation.
-spec bindProgramPipeline(integer()) -> ok.
bindProgramPipeline(Pipeline) ->
  cast(5779, <<Pipeline:?GLuint>>).

%% @spec (Pipelines::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgramPipelines.xml">external</a> documentation.
-spec deleteProgramPipelines([integer()]) -> ok.
deleteProgramPipelines(Pipelines) ->
  cast(5780, <<(length(Pipelines)):?GLuint,
        (<< <<C:?GLuint>> || C <- Pipelines>>)/binary,0:(((1+length(Pipelines)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenProgramPipelines.xml">external</a> documentation.
-spec genProgramPipelines(integer()) -> [integer()].
genProgramPipelines(N) ->
  call(5781, <<N:?GLsizei>>).

%% @spec (Pipeline::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsProgramPipeline.xml">external</a> documentation.
-spec isProgramPipeline(integer()) -> 0|1.
isProgramPipeline(Pipeline) ->
  call(5782, <<Pipeline:?GLuint>>).

%% @spec (Pipeline::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramPipeline.xml">external</a> documentation.
-spec getProgramPipelineiv(integer(),enum()) -> integer().
getProgramPipelineiv(Pipeline,Pname) ->
  call(5783, <<Pipeline:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1i(integer(),integer(),integer()) -> ok.
programUniform1i(Program,Location,V0) ->
  cast(5784, <<Program:?GLuint,Location:?GLint,V0:?GLint>>).

%% @spec (Program::integer(),Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1iv(integer(),integer(),[integer()]) -> ok.
programUniform1iv(Program,Location,Value) ->
  cast(5785, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @spec (Program::integer(),Location::integer(),V0::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1f(integer(),integer(),float()) -> ok.
programUniform1f(Program,Location,V0) ->
  cast(5786, <<Program:?GLuint,Location:?GLint,V0:?GLfloat>>).

%% @spec (Program::integer(),Location::integer(),Value::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1fv(integer(),integer(),[float()]) -> ok.
programUniform1fv(Program,Location,Value) ->
  cast(5787, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @spec (Program::integer(),Location::integer(),V0::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1d(integer(),integer(),float()) -> ok.
programUniform1d(Program,Location,V0) ->
  cast(5788, <<Program:?GLuint,Location:?GLint,V0:?GLdouble>>).

%% @spec (Program::integer(),Location::integer(),Value::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1dv(integer(),integer(),[float()]) -> ok.
programUniform1dv(Program,Location,Value) ->
  cast(5789, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1ui(integer(),integer(),integer()) -> ok.
programUniform1ui(Program,Location,V0) ->
  cast(5790, <<Program:?GLuint,Location:?GLint,V0:?GLuint>>).

%% @spec (Program::integer(),Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1uiv(integer(),integer(),[integer()]) -> ok.
programUniform1uiv(Program,Location,Value) ->
  cast(5791, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2i(integer(),integer(),integer(),integer()) -> ok.
programUniform2i(Program,Location,V0,V1) ->
  cast(5792, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2iv(integer(),integer(),[{integer(),integer()}]) -> ok.
programUniform2iv(Program,Location,Value) ->
  cast(5793, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2f(integer(),integer(),float(),float()) -> ok.
programUniform2f(Program,Location,V0,V1) ->
  cast(5794, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2fv(integer(),integer(),[{float(),float()}]) -> ok.
programUniform2fv(Program,Location,Value) ->
  cast(5795, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2d(integer(),integer(),float(),float()) -> ok.
programUniform2d(Program,Location,V0,V1) ->
  cast(5796, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2dv(integer(),integer(),[{float(),float()}]) -> ok.
programUniform2dv(Program,Location,Value) ->
  cast(5797, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2ui(integer(),integer(),integer(),integer()) -> ok.
programUniform2ui(Program,Location,V0,V1) ->
  cast(5798, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform2uiv(integer(),integer(),[{integer(),integer()}]) -> ok.
programUniform2uiv(Program,Location,Value) ->
  cast(5799, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3i(integer(),integer(),integer(),integer(),integer()) -> ok.
programUniform3i(Program,Location,V0,V1,V2) ->
  cast(5800, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3iv(integer(),integer(),[{integer(),integer(),integer()}]) -> ok.
programUniform3iv(Program,Location,Value) ->
  cast(5801, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3f(integer(),integer(),float(),float(),float()) -> ok.
programUniform3f(Program,Location,V0,V1,V2) ->
  cast(5802, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3fv(integer(),integer(),[{float(),float(),float()}]) -> ok.
programUniform3fv(Program,Location,Value) ->
  cast(5803, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3d(integer(),integer(),float(),float(),float()) -> ok.
programUniform3d(Program,Location,V0,V1,V2) ->
  cast(5804, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3dv(integer(),integer(),[{float(),float(),float()}]) -> ok.
programUniform3dv(Program,Location,Value) ->
  cast(5805, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3ui(integer(),integer(),integer(),integer(),integer()) -> ok.
programUniform3ui(Program,Location,V0,V1,V2) ->
  cast(5806, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform3uiv(integer(),integer(),[{integer(),integer(),integer()}]) -> ok.
programUniform3uiv(Program,Location,Value) ->
  cast(5807, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4i(integer(),integer(),integer(),integer(),integer(),integer()) -> ok.
programUniform4i(Program,Location,V0,V1,V2,V3) ->
  cast(5808, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4iv(integer(),integer(),[{integer(),integer(),integer(),integer()}]) -> ok.
programUniform4iv(Program,Location,Value) ->
  cast(5809, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float(),V2::float(),V3::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4f(integer(),integer(),float(),float(),float(),float()) -> ok.
programUniform4f(Program,Location,V0,V1,V2,V3) ->
  cast(5810, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4fv(integer(),integer(),[{float(),float(),float(),float()}]) -> ok.
programUniform4fv(Program,Location,Value) ->
  cast(5811, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::float(),V1::float(),V2::float(),V3::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4d(integer(),integer(),float(),float(),float(),float()) -> ok.
programUniform4d(Program,Location,V0,V1,V2,V3) ->
  cast(5812, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>>).

%% @spec (Program::integer(),Location::integer(),Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4dv(integer(),integer(),[{float(),float(),float(),float()}]) -> ok.
programUniform4dv(Program,Location,Value) ->
  cast(5813, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4ui(integer(),integer(),integer(),integer(),integer(),integer()) -> ok.
programUniform4ui(Program,Location,V0,V1,V2,V3) ->
  cast(5814, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @spec (Program::integer(),Location::integer(),Value::[{integer(),integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform4uiv(integer(),integer(),[{integer(),integer(),integer(),integer()}]) -> ok.
programUniform4uiv(Program,Location,Value) ->
  cast(5815, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix2fv(integer(),integer(),0|1,[{float(),float(),float(),float()}]) -> ok.
programUniformMatrix2fv(Program,Location,Transpose,Value) ->
  cast(5816, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix3fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3fv(Program,Location,Transpose,Value) ->
  cast(5817, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix4fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4fv(Program,Location,Transpose,Value) ->
  cast(5818, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix2dv(integer(),integer(),0|1,[{float(),float(),float(),float()}]) -> ok.
programUniformMatrix2dv(Program,Location,Transpose,Value) ->
  cast(5819, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix3dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3dv(Program,Location,Transpose,Value) ->
  cast(5820, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix.xml">external</a> documentation.
-spec programUniformMatrix4dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4dv(Program,Location,Transpose,Value) ->
  cast(5821, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix2x.xml">external</a> documentation.
-spec programUniformMatrix2x3fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix2x3fv(Program,Location,Transpose,Value) ->
  cast(5822, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix3x.xml">external</a> documentation.
-spec programUniformMatrix3x2fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3x2fv(Program,Location,Transpose,Value) ->
  cast(5823, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix2x.xml">external</a> documentation.
-spec programUniformMatrix2x4fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix2x4fv(Program,Location,Transpose,Value) ->
  cast(5824, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix4x.xml">external</a> documentation.
-spec programUniformMatrix4x2fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4x2fv(Program,Location,Transpose,Value) ->
  cast(5825, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix3x.xml">external</a> documentation.
-spec programUniformMatrix3x4fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3x4fv(Program,Location,Transpose,Value) ->
  cast(5826, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix4x.xml">external</a> documentation.
-spec programUniformMatrix4x3fv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4x3fv(Program,Location,Transpose,Value) ->
  cast(5827, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix2x.xml">external</a> documentation.
-spec programUniformMatrix2x3dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix2x3dv(Program,Location,Transpose,Value) ->
  cast(5828, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix3x.xml">external</a> documentation.
-spec programUniformMatrix3x2dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3x2dv(Program,Location,Transpose,Value) ->
  cast(5829, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix2x.xml">external</a> documentation.
-spec programUniformMatrix2x4dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix2x4dv(Program,Location,Transpose,Value) ->
  cast(5830, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix4x.xml">external</a> documentation.
-spec programUniformMatrix4x2dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4x2dv(Program,Location,Transpose,Value) ->
  cast(5831, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix3x.xml">external</a> documentation.
-spec programUniformMatrix3x4dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix3x4dv(Program,Location,Transpose,Value) ->
  cast(5832, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Program::integer(),Location::integer(),Transpose::0|1,Value::[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniformMatrix4x.xml">external</a> documentation.
-spec programUniformMatrix4x3dv(integer(),integer(),0|1,[{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}]) -> ok.
programUniformMatrix4x3dv(Program,Location,Transpose,Value) ->
  cast(5833, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Pipeline::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgramPipeline.xml">external</a> documentation.
-spec validateProgramPipeline(integer()) -> ok.
validateProgramPipeline(Pipeline) ->
  cast(5834, <<Pipeline:?GLuint>>).

%% @spec (Pipeline::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramPipelineInfoLog.xml">external</a> documentation.
-spec getProgramPipelineInfoLog(integer(),integer()) -> string().
getProgramPipelineInfoLog(Pipeline,BufSize) ->
  call(5835, <<Pipeline:?GLuint,BufSize:?GLsizei>>).

%% @spec (Index::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL1d(integer(),float()) -> ok.
vertexAttribL1d(Index,X) ->
  cast(5836, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @spec (Index::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL2d(integer(),float(),float()) -> ok.
vertexAttribL2d(Index,X,Y) ->
  cast(5837, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL3d(integer(),float(),float(),float()) -> ok.
vertexAttribL3d(Index,X,Y,Z) ->
  cast(5838, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL4d(integer(),float(),float(),float(),float()) -> ok.
vertexAttribL4d(Index,X,Y,Z,W) ->
  cast(5839, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttribL1d(Index,X)
-spec vertexAttribL1dv(integer(),{float()}) -> ok.
vertexAttribL1dv(Index,{X}) ->  vertexAttribL1d(Index,X).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttribL2d(Index,X,Y)
-spec vertexAttribL2dv(integer(),{float(),float()}) -> ok.
vertexAttribL2dv(Index,{X,Y}) ->  vertexAttribL2d(Index,X,Y).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttribL3d(Index,X,Y,Z)
-spec vertexAttribL3dv(integer(),{float(),float(),float()}) -> ok.
vertexAttribL3dv(Index,{X,Y,Z}) ->  vertexAttribL3d(Index,X,Y,Z).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttribL4d(Index,X,Y,Z,W)
-spec vertexAttribL4dv(integer(),{float(),float(),float(),float()}) -> ok.
vertexAttribL4dv(Index,{X,Y,Z,W}) ->  vertexAttribL4d(Index,X,Y,Z,W).

%% @spec (Index::integer(),Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribLPointer.xml">external</a> documentation.
-spec vertexAttribLPointer(integer(),integer(),enum(),integer(),offset()|mem()) -> ok.
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5840, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5841, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Index::integer(),Pname::enum()) -> {float(),float(),float(),float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribL.xml">external</a> documentation.
-spec getVertexAttribLdv(integer(),enum()) -> {float(),float(),float(),float()}.
getVertexAttribLdv(Index,Pname) ->
  call(5842, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (First::integer(),V::[{float(),float(),float(),float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewportArrayv.xml">external</a> documentation.
-spec viewportArrayv(integer(),[{float(),float(),float(),float()}]) -> ok.
viewportArrayv(First,V) ->
  cast(5843, <<First:?GLuint,(length(V)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- V>>)/binary>>).

%% @spec (Index::integer(),X::float(),Y::float(),W::float(),H::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewportIndexed.xml">external</a> documentation.
-spec viewportIndexedf(integer(),float(),float(),float(),float()) -> ok.
viewportIndexedf(Index,X,Y,W,H) ->
  cast(5844, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,W:?GLfloat,H:?GLfloat>>).

%% @spec (Index::integer(),V::{float(),float(),float(),float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewportIndexed.xml">external</a> documentation.
-spec viewportIndexedfv(integer(),{float(),float(),float(),float()}) -> ok.
viewportIndexedfv(Index,{V1,V2,V3,V4}) ->
  cast(5845, <<Index:?GLuint,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>>).

%% @spec (First::integer(),V::[{integer(),integer(),integer(),integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorArrayv.xml">external</a> documentation.
-spec scissorArrayv(integer(),[{integer(),integer(),integer(),integer()}]) -> ok.
scissorArrayv(First,V) ->
  cast(5846, <<First:?GLuint,(length(V)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- V>>)/binary>>).

%% @spec (Index::integer(),Left::integer(),Bottom::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorIndexe.xml">external</a> documentation.
-spec scissorIndexed(integer(),integer(),integer(),integer(),integer()) -> ok.
scissorIndexed(Index,Left,Bottom,Width,Height) ->
  cast(5847, <<Index:?GLuint,Left:?GLint,Bottom:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Index::integer(),V::{integer(),integer(),integer(),integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorIndexe.xml">external</a> documentation.
-spec scissorIndexedv(integer(),{integer(),integer(),integer(),integer()}) -> ok.
scissorIndexedv(Index,{V1,V2,V3,V4}) ->
  cast(5848, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (First::integer(),V::[{clamp(),clamp()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRangeArrayv.xml">external</a> documentation.
-spec depthRangeArrayv(integer(),[{clamp(),clamp()}]) -> ok.
depthRangeArrayv(First,V) ->
  cast(5849, <<First:?GLuint,0:32,(length(V)):?GLuint,0:32,
        (<< <<V1:?GLclampd,V2:?GLclampd>> || {V1,V2} <- V>>)/binary>>).

%% @spec (Index::integer(),N::clamp(),F::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRangeIndexe.xml">external</a> documentation.
-spec depthRangeIndexed(integer(),clamp(),clamp()) -> ok.
depthRangeIndexed(Index,N,F) ->
  cast(5850, <<Index:?GLuint,0:32,N:?GLclampd,F:?GLclampd>>).

%% @spec (Target::enum(),Index::integer()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFloati_v.xml">external</a> documentation.
-spec getFloati_v(enum(),integer()) -> [float()].
getFloati_v(Target,Index) ->
  call(5851, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetDoublei_v.xml">external</a> documentation.
-spec getDoublei_v(enum(),integer()) -> [float()].
getDoublei_v(Target,Index) ->
  call(5852, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Source::enum(),Type::enum(),Severity::enum(),Ids::[integer()],Enabled::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDebugMessageControlARB.xml">external</a> documentation.
-spec debugMessageControlARB(enum(),enum(),enum(),[integer()],0|1) -> ok.
debugMessageControlARB(Source,Type,Severity,Ids,Enabled) ->
  cast(5853, <<Source:?GLenum,Type:?GLenum,Severity:?GLenum,(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((length(Ids)) rem 2)*32),Enabled:?GLboolean>>).

%% @spec (Source::enum(),Type::enum(),Id::integer(),Severity::enum(),Buf::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDebugMessageInsertARB.xml">external</a> documentation.
-spec debugMessageInsertARB(enum(),enum(),integer(),enum(),string()) -> ok.
debugMessageInsertARB(Source,Type,Id,Severity,Buf) ->
  cast(5854, <<Source:?GLenum,Type:?GLenum,Id:?GLuint,Severity:?GLenum,(list_to_binary([Buf|[0]]))/binary,0:((8-((length(Buf)+ 1) rem 8)) rem 8)>>).

%% @spec (Count::integer(),Bufsize::integer()) -> {integer(),Sources::[enum()],Types::[enum()],Ids::[integer()],Severities::[enum()],MessageLog::[string()]}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetDebugMessageLogARB.xml">external</a> documentation.
-spec getDebugMessageLogARB(integer(),integer()) -> {integer(),[enum()],[enum()],[integer()],[enum()],[string()]}.
getDebugMessageLogARB(Count,Bufsize) ->
  call(5855, <<Count:?GLuint,Bufsize:?GLsizei>>).

%% @spec () -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetGraphicsResetStatusARB.xml">external</a> documentation.
-spec getGraphicsResetStatusARB() -> enum().
getGraphicsResetStatusARB() ->
  call(5856, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResizeBuffersMESA.xml">external</a> documentation.
-spec resizeBuffersMESA() -> ok.
resizeBuffersMESA() ->
  cast(5857, <<>>).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4dMESA.xml">external</a> documentation.
-spec windowPos4dMESA(float(),float(),float(),float()) -> ok.
windowPos4dMESA(X,Y,Z,W) ->
  cast(5858, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4dMESA(X,Y,Z,W)
-spec windowPos4dvMESA({float(),float(),float(),float()}) -> ok.
windowPos4dvMESA({X,Y,Z,W}) ->  windowPos4dMESA(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4fMESA.xml">external</a> documentation.
-spec windowPos4fMESA(float(),float(),float(),float()) -> ok.
windowPos4fMESA(X,Y,Z,W) ->
  cast(5859, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4fMESA(X,Y,Z,W)
-spec windowPos4fvMESA({float(),float(),float(),float()}) -> ok.
windowPos4fvMESA({X,Y,Z,W}) ->  windowPos4fMESA(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4iMESA.xml">external</a> documentation.
-spec windowPos4iMESA(integer(),integer(),integer(),integer()) -> ok.
windowPos4iMESA(X,Y,Z,W) ->
  cast(5860, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4iMESA(X,Y,Z,W)
-spec windowPos4ivMESA({integer(),integer(),integer(),integer()}) -> ok.
windowPos4ivMESA({X,Y,Z,W}) ->  windowPos4iMESA(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4sMESA.xml">external</a> documentation.
-spec windowPos4sMESA(integer(),integer(),integer(),integer()) -> ok.
windowPos4sMESA(X,Y,Z,W) ->
  cast(5861, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4sMESA(X,Y,Z,W)
-spec windowPos4svMESA({integer(),integer(),integer(),integer()}) -> ok.
windowPos4svMESA({X,Y,Z,W}) ->  windowPos4sMESA(X,Y,Z,W).

%% @spec (Zmin::clamp(),Zmax::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthBoundsEXT.xml">external</a> documentation.
-spec depthBoundsEXT(clamp(),clamp()) -> ok.
depthBoundsEXT(Zmin,Zmax) ->
  cast(5862, <<Zmin:?GLclampd,Zmax:?GLclampd>>).

%% @spec (StencilTagBits::integer(),StencilClearTag::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilClearTagEXT.xml">external</a> documentation.
-spec stencilClearTagEXT(integer(),integer()) -> ok.
stencilClearTagEXT(StencilTagBits,StencilClearTag) ->
  cast(5863, <<StencilTagBits:?GLsizei,StencilClearTag:?GLuint>>).

