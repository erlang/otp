// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/emitterutils_p.h"
#include "../core/errorhandler.h"
#include "../core/logger.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

// BaseEmitter - Construction & Destruction
// ========================================

BaseEmitter::BaseEmitter(EmitterType emitterType) noexcept
  : _emitterType(emitterType) {}

BaseEmitter::~BaseEmitter() noexcept {
  if (_code) {
    _addEmitterFlags(EmitterFlags::kDestroyed);
    _code->detach(this);
  }
}

// BaseEmitter - Finalize
// ======================

Error BaseEmitter::finalize() {
  // Does nothing by default, overridden by `BaseBuilder` and `BaseCompiler`.
  return kErrorOk;
}

// BaseEmitter - Internals
// =======================

static constexpr EmitterFlags kEmitterPreservedFlags = EmitterFlags::kOwnLogger | EmitterFlags::kOwnErrorHandler;

static ASMJIT_NOINLINE void BaseEmitter_updateForcedOptions(BaseEmitter* self) noexcept {
  bool emitComments = false;
  bool hasDiagnosticOptions = false;

  if (self->emitterType() == EmitterType::kAssembler) {
    // Assembler: Don't emit comments if logger is not attached.
    emitComments = self->_code != nullptr && self->_logger != nullptr;
    hasDiagnosticOptions = self->hasDiagnosticOption(DiagnosticOptions::kValidateAssembler);
  }
  else {
    // Builder/Compiler: Always emit comments, we cannot assume they won't be used.
    emitComments = self->_code != nullptr;
    hasDiagnosticOptions = self->hasDiagnosticOption(DiagnosticOptions::kValidateIntermediate);
  }

  if (emitComments)
    self->_addEmitterFlags(EmitterFlags::kLogComments);
  else
    self->_clearEmitterFlags(EmitterFlags::kLogComments);

  // The reserved option tells emitter (Assembler/Builder/Compiler) that there may be either a border
  // case (CodeHolder not attached, for example) or that logging or validation is required.
  if (self->_code == nullptr || self->_logger || hasDiagnosticOptions)
    self->_forcedInstOptions |= InstOptions::kReserved;
  else
    self->_forcedInstOptions &= ~InstOptions::kReserved;
}

// BaseEmitter - Diagnostic Options
// ================================

void BaseEmitter::addDiagnosticOptions(DiagnosticOptions options) noexcept {
  _diagnosticOptions |= options;
  BaseEmitter_updateForcedOptions(this);
}

void BaseEmitter::clearDiagnosticOptions(DiagnosticOptions options) noexcept {
  _diagnosticOptions &= ~options;
  BaseEmitter_updateForcedOptions(this);
}

// BaseEmitter - Logging
// =====================

void BaseEmitter::setLogger(Logger* logger) noexcept {
#ifndef ASMJIT_NO_LOGGING
  if (logger) {
    _logger = logger;
    _addEmitterFlags(EmitterFlags::kOwnLogger);
  }
  else {
    _logger = nullptr;
    _clearEmitterFlags(EmitterFlags::kOwnLogger);
    if (_code)
      _logger = _code->logger();
  }
  BaseEmitter_updateForcedOptions(this);
#else
  DebugUtils::unused(logger);
#endif
}

// BaseEmitter - Error Handling
// ============================

void BaseEmitter::setErrorHandler(ErrorHandler* errorHandler) noexcept {
  if (errorHandler) {
    _errorHandler = errorHandler;
    _addEmitterFlags(EmitterFlags::kOwnErrorHandler);
  }
  else {
    _errorHandler = nullptr;
    _clearEmitterFlags(EmitterFlags::kOwnErrorHandler);
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

// BaseEmitter - Labels
// ====================

Label BaseEmitter::labelByName(const char* name, size_t nameSize, uint32_t parentId) noexcept {
  return Label(_code ? _code->labelIdByName(name, nameSize, parentId) : Globals::kInvalidId);
}

bool BaseEmitter::isLabelValid(uint32_t labelId) const noexcept {
  return _code && labelId < _code->labelCount();
}

// BaseEmitter - Emit (Low-Level)
// ==============================

using EmitterUtils::noExt;

Error BaseEmitter::_emitI(InstId instId) {
  return _emit(instId, noExt[0], noExt[1], noExt[2], noExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0) {
  return _emit(instId, o0, noExt[1], noExt[2], noExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0, const Operand_& o1) {
  return _emit(instId, o0, o1, noExt[2], noExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2) {
  return _emit(instId, o0, o1, o2, noExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3) {
  Operand_ opExt[3] = { o3 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4) {
  Operand_ opExt[3] = { o3, o4 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4, const Operand_& o5) {
  Operand_ opExt[3] = { o3, o4, o5 };
  return _emit(instId, o0, o1, o2, opExt);
}

Error BaseEmitter::_emitOpArray(InstId instId, const Operand_* operands, size_t opCount) {
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

// BaseEmitter - Emit Utilities
// ============================

Error BaseEmitter::emitProlog(const FuncFrame& frame) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

  return _funcs.emitProlog(this, frame);
}

Error BaseEmitter::emitEpilog(const FuncFrame& frame) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

  return _funcs.emitEpilog(this, frame);
}

Error BaseEmitter::emitArgsAssignment(const FuncFrame& frame, const FuncArgsAssignment& args) {
  if (ASMJIT_UNLIKELY(!_code))
    return DebugUtils::errored(kErrorNotInitialized);

  return _funcs.emitArgsAssignment(this, frame, args);
}

// BaseEmitter - Comment
// =====================

Error BaseEmitter::commentf(const char* fmt, ...) {
  if (!hasEmitterFlag(EmitterFlags::kLogComments)) {
    if (!hasEmitterFlag(EmitterFlags::kAttached))
      return reportError(DebugUtils::errored(kErrorNotInitialized));
    return kErrorOk;
  }

#ifndef ASMJIT_NO_LOGGING
  StringTmp<1024> sb;

  va_list ap;
  va_start(ap, fmt);
  Error err = sb.appendVFormat(fmt, ap);
  va_end(ap);

  ASMJIT_PROPAGATE(err);
  return comment(sb.data(), sb.size());
#else
  DebugUtils::unused(fmt);
  return kErrorOk;
#endif
}

Error BaseEmitter::commentv(const char* fmt, va_list ap) {
  if (!hasEmitterFlag(EmitterFlags::kLogComments)) {
    if (!hasEmitterFlag(EmitterFlags::kAttached))
      return reportError(DebugUtils::errored(kErrorNotInitialized));
    return kErrorOk;
  }

#ifndef ASMJIT_NO_LOGGING
  StringTmp<1024> sb;
  Error err = sb.appendVFormat(fmt, ap);

  ASMJIT_PROPAGATE(err);
  return comment(sb.data(), sb.size());
#else
  DebugUtils::unused(fmt, ap);
  return kErrorOk;
#endif
}

// BaseEmitter - Events
// ====================

Error BaseEmitter::onAttach(CodeHolder* code) noexcept {
  _code = code;
  _environment = code->environment();
  _addEmitterFlags(EmitterFlags::kAttached);

  const ArchTraits& archTraits = ArchTraits::byArch(code->arch());
  RegType nativeRegType = Environment::is32Bit(code->arch()) ? RegType::kGp32 : RegType::kGp64;
  _gpSignature = archTraits.regTypeToSignature(nativeRegType);

  onSettingsUpdated();
  return kErrorOk;
}

Error BaseEmitter::onDetach(CodeHolder* code) noexcept {
  DebugUtils::unused(code);

  if (!hasOwnLogger())
    _logger = nullptr;

  if (!hasOwnErrorHandler())
    _errorHandler = nullptr;

  _clearEmitterFlags(~kEmitterPreservedFlags);
  _forcedInstOptions = InstOptions::kReserved;
  _privateData = 0;

  _environment.reset();
  _gpSignature.reset();

  _instOptions = InstOptions::kNone;
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
