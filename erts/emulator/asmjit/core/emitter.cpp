// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#include "../core/api-build_p.h"
#include "../core/emitterutils_p.h"
#include "../core/errorhandler.h"
#include "../core/logger.h"
#include "../core/support.h"

#ifdef ASMJIT_BUILD_X86
  #include "../x86/x86internal_p.h"
  #include "../x86/x86instdb_p.h"
#endif // ASMJIT_BUILD_X86

#ifdef ASMJIT_BUILD_ARM
  #include "../arm/arminternal_p.h"
  #include "../arm/arminstdb.h"
#endif // ASMJIT_BUILD_ARM

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::BaseEmitter - Construction / Destruction]
// ============================================================================

BaseEmitter::BaseEmitter(uint32_t emitterType) noexcept
  : _emitterType(uint8_t(emitterType)),
    _emitterFlags(0),
    _validationFlags(0),
    _validationOptions(0),
    _encodingOptions(0),
    _forcedInstOptions(BaseInst::kOptionReserved),
    _privateData(0),
    _code(nullptr),
    _logger(nullptr),
    _errorHandler(nullptr),
    _environment(),
    _gpRegInfo(),
    _instOptions(0),
    _extraReg(),
    _inlineComment(nullptr) {}

BaseEmitter::~BaseEmitter() noexcept {
  if (_code) {
    _addEmitterFlags(kFlagDestroyed);
    _code->detach(this);
  }
}

// ============================================================================
// [asmjit::BaseEmitter - Finalize]
// ============================================================================

Error BaseEmitter::finalize() {
  // Does nothing by default, overridden by `BaseBuilder` and `BaseCompiler`.
  return kErrorOk;
}

// ============================================================================
// [asmjit::BaseEmitter - Internals]
// ============================================================================

static constexpr uint32_t kEmitterPreservedFlags =
    BaseEmitter::kFlagOwnLogger |
    BaseEmitter::kFlagOwnErrorHandler ;

static ASMJIT_NOINLINE void BaseEmitter_updateForcedOptions(BaseEmitter* self) noexcept {
  bool hasLogger = self->_logger != nullptr;
  bool hasValidationOptions;

  if (self->emitterType() == BaseEmitter::kTypeAssembler)
    hasValidationOptions = self->hasValidationOption(BaseEmitter::kValidationOptionAssembler);
  else
    hasValidationOptions = self->hasValidationOption(BaseEmitter::kValidationOptionIntermediate);

  self->_forcedInstOptions &= ~BaseInst::kOptionReserved;
  if (hasLogger || hasValidationOptions)
    self->_forcedInstOptions |= BaseInst::kOptionReserved;
}

// ============================================================================
// [asmjit::BaseEmitter - Validation Options]
// ============================================================================

void BaseEmitter::addValidationOptions(uint32_t options) noexcept {
  _validationOptions = uint8_t(_validationOptions | options);
  BaseEmitter_updateForcedOptions(this);
}

void BaseEmitter::clearValidationOptions(uint32_t options) noexcept {
  _validationOptions = uint8_t(_validationOptions | options);
  BaseEmitter_updateForcedOptions(this);
}

// ============================================================================
// [asmjit::BaseEmitter - Logging]
// ============================================================================

void BaseEmitter::setLogger(Logger* logger) noexcept {
#ifndef ASMJIT_NO_LOGGING
  if (logger) {
    _logger = logger;
    _addEmitterFlags(kFlagOwnLogger);
  }
  else {
    _logger = nullptr;
    _clearEmitterFlags(kFlagOwnLogger);
    if (_code)
      _logger = _code->logger();
  }
  BaseEmitter_updateForcedOptions(this);
#else
  DebugUtils::unused(logger);
#endif
}

// ============================================================================
// [asmjit::BaseEmitter - Error Handling]
// ============================================================================

void BaseEmitter::setErrorHandler(ErrorHandler* errorHandler) noexcept {
  if (errorHandler) {
    _errorHandler = errorHandler;
    _addEmitterFlags(kFlagOwnErrorHandler);
  }
  else {
    _errorHandler = nullptr;
    _clearEmitterFlags(kFlagOwnErrorHandler);
    if (_code)
      _errorHandler = _code->errorHandler();
  }
}

Error BaseEmitter::reportError(Error err, const char* message) {
  ErrorHandler* eh = _errorHandler;
  if (eh) {
    if (!message)
      message = DebugUtils::errorAsString(err);
    eh->handleError(err, message, this);
  }
  return err;
}

// ============================================================================
// [asmjit::BaseEmitter - Labels]
// ============================================================================

Label BaseEmitter::labelByName(const char* name, size_t nameSize, uint32_t parentId) noexcept {
  return Label(_code ? _code->labelIdByName(name, nameSize, parentId) : uint32_t(Globals::kInvalidId));
}

bool BaseEmitter::isLabelValid(uint32_t labelId) const noexcept {
  return _code && labelId < _code->labelCount();
}

// ============================================================================
// [asmjit::BaseEmitter - Emit (Low-Level)]
// ============================================================================

using EmitterUtils::noExt;

Error BaseEmitter::_emitI(uint32_t instId) {
  return _emit(instId, noExt[0], noExt[1], noExt[2], noExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0) {
  return _emit(instId, o0, noExt[1], noExt[2], noExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0, const Operand_& o1) {
  return _emit(instId, o0, o1, noExt[2], noExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0, const Operand_& o1, const Operand_& o2) {
  return _emit(instId, o0, o1, o2, noExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) {
  Operand_ opExt[3] = { o3 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4) {
  Operand_ opExt[3] = { o3, o4 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitI(uint32_t instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4, const Operand_& o5) {
  Operand_ opExt[3] = { o3, o4, o5 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitOpArray(uint32_t instId, const Operand_* operands, size_t opCount) {
  const Operand_* op = operands;

  Operand_ opExt[3];

  switch (opCount) {
    case 0:
      return _emit(instId, noExt[0], noExt[1], noExt[2], noExt);

    case 1:
      return _emit(instId, op[0], noExt[1], noExt[2], noExt);

    case 2:
      return _emit(instId, op[0], op[1], noExt[2], noExt);

    case 3:
      return _emit(instId, op[0], op[1], op[2], noExt);

    case 4:
      opExt[0] = op[3];
      opExt[1].reset();
      opExt[2].reset();
      return _emit(instId, op[0], op[1], op[2], opExt);

    case 5:
      opExt[0] = op[3];
      opExt[1] = op[4];
      opExt[2].reset();
      return _emit(instId, op[0], op[1], op[2], opExt);

    case 6:
      return _emit(instId, op[0], op[1], op[2], op + 3);

    default:
      return DebugUtils::errored(kErrorInvalidArgument);
  }
}

// ============================================================================
// [asmjit::BaseEmitter - Emit (High-Level)]
// ============================================================================

ASMJIT_FAVOR_SIZE Error BaseEmitter::emitProlog(const FuncFrame& frame) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

#ifdef ASMJIT_BUILD_X86
  if (environment().isFamilyX86())
    return x86::X86Internal::emitProlog(as<x86::Emitter>(), frame);
#endif

#ifdef ASMJIT_BUILD_ARM
  if (environment().isFamilyARM())
    return arm::ArmInternal::emitProlog(as<arm::Emitter>(), frame);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}

ASMJIT_FAVOR_SIZE Error BaseEmitter::emitEpilog(const FuncFrame& frame) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

#ifdef ASMJIT_BUILD_X86
  if (environment().isFamilyX86())
    return x86::X86Internal::emitEpilog(as<x86::Emitter>(), frame);
#endif

#ifdef ASMJIT_BUILD_ARM
  if (environment().isFamilyARM())
    return arm::ArmInternal::emitEpilog(as<arm::Emitter>(), frame);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}

ASMJIT_FAVOR_SIZE Error BaseEmitter::emitArgsAssignment(const FuncFrame& frame, const FuncArgsAssignment& args) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

#ifdef ASMJIT_BUILD_X86
  if (environment().isFamilyX86())
    return x86::X86Internal::emitArgsAssignment(as<x86::Emitter>(), frame, args);
#endif

#ifdef ASMJIT_BUILD_ARM
  if (environment().isFamilyARM())
    return arm::ArmInternal::emitArgsAssignment(as<arm::Emitter>(), frame, args);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}

// ============================================================================
// [asmjit::BaseEmitter - Comment]
// ============================================================================

Error BaseEmitter::commentf(const char* fmt, ...) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

#ifndef ASMJIT_NO_LOGGING
  va_list ap;
  va_start(ap, fmt);
  Error err = commentv(fmt, ap);
  va_end(ap);
  return err;
#else
  DebugUtils::unused(fmt);
  return kErrorOk;
#endif
}

Error BaseEmitter::commentv(const char* fmt, va_list ap) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

#ifndef ASMJIT_NO_LOGGING
  StringTmp<1024> sb;
  Error err = sb.appendVFormat(fmt, ap);

  if (ASMJIT_UNLIKELY(err))
    return err;

  return comment(sb.data(), sb.size());
#else
  DebugUtils::unused(fmt, ap);
  return kErrorOk;
#endif
}

// ============================================================================
// [asmjit::BaseEmitter - Events]
// ============================================================================

Error BaseEmitter::onAttach(CodeHolder* code) noexcept {
  _code = code;
  _environment = code->environment();

  onSettingsUpdated();
  return kErrorOk;
}

Error BaseEmitter::onDetach(CodeHolder* code) noexcept {
  DebugUtils::unused(code);

  _clearEmitterFlags(~kEmitterPreservedFlags);
  _forcedInstOptions = BaseInst::kOptionReserved;
  _privateData = 0;

  if (!hasOwnLogger())
    _logger = nullptr;

  if (!hasOwnErrorHandler())
    _errorHandler = nullptr;

  _environment.reset();
  _gpRegInfo.reset();

  _instOptions = 0;
  _extraReg.reset();
  _inlineComment = nullptr;

  return kErrorOk;
}

void BaseEmitter::onSettingsUpdated() noexcept {
  // Only called when attached to CodeHolder by CodeHolder.
  ASMJIT_ASSERT(_code != nullptr);

  if (!hasOwnLogger())
    _logger = _code->logger();

  if (!hasOwnErrorHandler())
    _errorHandler = _code->errorHandler();

  BaseEmitter_updateForcedOptions(this);
}

ASMJIT_END_NAMESPACE
