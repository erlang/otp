// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_EMITTER_H_INCLUDED
#define ASMJIT_CORE_EMITTER_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/codeholder.h"
#include "../core/formatter.h"
#include "../core/inst.h"
#include "../core/operand.h"
#include "../core/type.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

class ConstPool;
class FuncFrame;
class FuncArgsAssignment;

//! Align mode, used by \ref BaseEmitter::align().
enum class AlignMode : uint8_t {
  //! Align executable code.
  kCode = 0,
  //! Align non-executable code.
  kData = 1,
  //! Align by a sequence of zeros.
  kZero = 2,

  //! Maximum value of `AlignMode`.
  kMaxValue = kZero
};

//! Emitter type used by \ref BaseEmitter.
enum class EmitterType : uint8_t {
  //! Unknown or uninitialized.
  kNone = 0,
  //! Emitter inherits from \ref BaseAssembler.
  kAssembler = 1,
  //! Emitter inherits from \ref BaseBuilder.
  kBuilder = 2,
  //! Emitter inherits from \ref BaseCompiler.
  kCompiler = 3,

  //! Maximum value of `EmitterType`.
  kMaxValue = kCompiler
};

//! Emitter flags, used by \ref BaseEmitter.
enum class EmitterFlags : uint8_t {
  //! No flags.
  kNone = 0u,
  //! Emitter is attached to CodeHolder.
  kAttached = 0x01u,
  //! The emitter must emit comments.
  kLogComments = 0x08u,
  //! The emitter has its own \ref Logger (not propagated from \ref CodeHolder).
  kOwnLogger = 0x10u,
  //! The emitter has its own \ref ErrorHandler (not propagated from \ref CodeHolder).
  kOwnErrorHandler = 0x20u,
  //! The emitter was finalized.
  kFinalized = 0x40u,
  //! The emitter was destroyed.
  //!
  //! This flag is used for a very short time when an emitter is being destroyed by
  //! CodeHolder.
  kDestroyed = 0x80u
};
ASMJIT_DEFINE_ENUM_FLAGS(EmitterFlags)

//! Encoding options.
enum class EncodingOptions : uint32_t {
  //! No encoding options.
  kNone = 0,

  //! Emit instructions that are optimized for size, if possible.
  //!
  //! Default: false.
  //!
  //! X86 Specific
  //! ------------
  //!
  //! When this option is set it the assembler will try to fix instructions if possible into operation equivalent
  //! instructions that take less bytes by taking advantage of implicit zero extension. For example instruction
  //! like `mov r64, imm` and `and r64, imm` can be translated to `mov r32, imm` and `and r32, imm` when the
  //! immediate constant is lesser than `2^31`.
  kOptimizeForSize = 0x00000001u,

  //! Emit optimized code-alignment sequences.
  //!
  //! Default: false.
  //!
  //! X86 Specific
  //! ------------
  //!
  //! Default align sequence used by X86 architecture is one-byte (0x90) opcode that is often shown by disassemblers
  //! as NOP. However there are more optimized align sequences for 2-11 bytes that may execute faster on certain CPUs.
  //! If this feature is enabled AsmJit will generate specialized sequences for alignment between 2 to 11 bytes.
  kOptimizedAlign = 0x00000002u,

  //! Emit jump-prediction hints.
  //!
  //! Default: false.
  //!
  //! X86 Specific
  //! ------------
  //!
  //! Jump prediction is usually based on the direction of the jump. If the jump is backward it is usually predicted as
  //! taken; and if the jump is forward it is usually predicted as not-taken. The reason is that loops generally use
  //! backward jumps and conditions usually use forward jumps. However this behavior can be overridden by using
  //! instruction prefixes. If this option is enabled these hints will be emitted.
  //!
  //! This feature is disabled by default, because the only processor that used to take into consideration prediction
  //! hints was P4. Newer processors implement heuristics for branch prediction and ignore static hints. This means
  //! that this feature can be only used for annotation purposes.
  kPredictedJumps = 0x00000010u
};
ASMJIT_DEFINE_ENUM_FLAGS(EncodingOptions)

//! Diagnostic options are used to tell emitters and their passes to perform diagnostics when emitting or processing
//! user code. These options control validation and extra diagnostics that can be performed by higher level emitters.
//!
//! Instruction Validation
//! ----------------------
//!
//! \ref BaseAssembler implementation perform by default only basic checks that are necessary to identify all
//! variations of an instruction so the correct encoding can be selected. This is fine for production-ready code
//! as the assembler doesn't have to perform checks that would slow it down. However, sometimes these checks are
//! beneficial especially when the project that uses AsmJit is in a development phase, in which mistakes happen
//! often. To make the experience of using AsmJit seamless it offers validation features that can be controlled
//! by \ref DiagnosticOptions.
//!
//! Compiler Diagnostics
//! --------------------
//!
//! Diagnostic options work with \ref BaseCompiler passes (precisely with its register allocation pass). These options
//! can be used to enable logging of all operations that the Compiler does.
enum class DiagnosticOptions : uint32_t {
  //! No validation options.
  kNone = 0,

  //! Perform strict validation in \ref BaseAssembler::emit() implementations.
  //!
  //! This flag ensures that each instruction is checked before it's encoded into a binary representation. This flag
  //! is only relevant for \ref BaseAssembler implementations, but can be set in any other emitter type, in that case
  //! if that emitter needs to create an assembler on its own, for the purpose of \ref BaseEmitter::finalize() it
  //! would propagate this flag to such assembler so all instructions passed to it are explicitly validated.
  //!
  //! Default: false.
  kValidateAssembler = 0x00000001u,

  //! Perform strict validation in \ref BaseBuilder::emit() and \ref BaseCompiler::emit() implementations.
  //!
  //! This flag ensures that each instruction is checked before an \ref InstNode representing the instruction is
  //! created by \ref BaseBuilder or \ref BaseCompiler. This option could be more useful than \ref kValidateAssembler
  //! in cases in which there is an invalid instruction passed to an assembler, which was invalid much earlier, most
  //! likely when such instruction was passed to Builder/Compiler.
  //!
  //! This is a separate option that was introduced, because it's possible to manipulate the instruction stream
  //! emitted by \ref BaseBuilder and \ref BaseCompiler - this means that it's allowed to emit invalid instructions
  //! (for example with missing operands) that will be fixed later before finalizing it.
  //!
  //! Default: false.
  kValidateIntermediate = 0x00000002u,

  //! Annotate all nodes processed by register allocator (Compiler/RA).
  //!
  //! \note Annotations don't need debug options, however, some debug options like `kRADebugLiveness` may influence
  //! their output (for example the mentioned option would add liveness information to per-instruction annotation).
  kRAAnnotate = 0x00000080u,

  //! Debug CFG generation and other related algorithms / operations (Compiler/RA).
  kRADebugCFG = 0x00000100u,

  //! Debug liveness analysis (Compiler/RA).
  kRADebugLiveness = 0x00000200u,

  //! Debug register allocation assignment (Compiler/RA).
  kRADebugAssignment = 0x00000400u,

  //! Debug the removal of code part of unreachable blocks.
  kRADebugUnreachable = 0x00000800u,

  //! Enable all debug options (Compiler/RA).
  kRADebugAll = 0x0000FF00u,
};
ASMJIT_DEFINE_ENUM_FLAGS(DiagnosticOptions)

//! Provides a base foundation to emitting code - specialized by \ref BaseAssembler and \ref BaseBuilder.
class ASMJIT_VIRTAPI BaseEmitter {
public:
  ASMJIT_BASE_CLASS(BaseEmitter)

  //! \name Members
  //! \{

  //! See \ref EmitterType.
  EmitterType _emitterType = EmitterType::kNone;
  //! See \ref EmitterFlags.
  EmitterFlags _emitterFlags = EmitterFlags::kNone;
  //! Validation flags in case validation is used.
  //!
  //! \note Validation flags are specific to the emitter and they are setup at construction time and then never
  //! changed.
  ValidationFlags _validationFlags = ValidationFlags::kNone;
  //! Validation options.
  DiagnosticOptions _diagnosticOptions = DiagnosticOptions::kNone;

  //! All supported architectures in a bit-mask, where LSB is the bit with a zero index.
  uint64_t _archMask = 0;

  //! Encoding options.
  EncodingOptions _encodingOptions = EncodingOptions::kNone;

  //! Forced instruction options, combined with \ref _instOptions by \ref emit().
  InstOptions _forcedInstOptions = InstOptions::kReserved;
  //! Internal private data used freely by any emitter.
  uint32_t _privateData = 0;

  //! CodeHolder the emitter is attached to.
  CodeHolder* _code = nullptr;
  //! Attached \ref Logger.
  Logger* _logger = nullptr;
  //! Attached \ref ErrorHandler.
  ErrorHandler* _errorHandler = nullptr;

  //! Describes the target environment, matches \ref CodeHolder::environment().
  Environment _environment {};
  //! Native GP register signature and signature related information.
  OperandSignature _gpSignature {};

  //! Next instruction options (affects the next instruction).
  InstOptions _instOptions = InstOptions::kNone;
  //! Extra register (op-mask {k} on AVX-512) (affects the next instruction).
  RegOnly _extraReg {};
  //! Inline comment of the next instruction (affects the next instruction).
  const char* _inlineComment = nullptr;

  //! Function callbacks used by emitter implementation.
  //!
  //! These are typically shared between Assembler/Builder/Compiler of a single backend.
  struct Funcs {
    typedef Error (ASMJIT_CDECL* EmitProlog)(BaseEmitter* emitter, const FuncFrame& frame);
    typedef Error (ASMJIT_CDECL* EmitEpilog)(BaseEmitter* emitter, const FuncFrame& frame);
    typedef Error (ASMJIT_CDECL* EmitArgsAssignment)(BaseEmitter* emitter, const FuncFrame& frame, const FuncArgsAssignment& args);

    typedef Error (ASMJIT_CDECL* FormatInstruction)(
      String& sb,
      FormatFlags formatFlags,
      const BaseEmitter* emitter,
      Arch arch,
      const BaseInst& inst, const Operand_* operands, size_t opCount) ASMJIT_NOEXCEPT_TYPE;

    typedef Error (ASMJIT_CDECL* ValidateFunc)(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags) ASMJIT_NOEXCEPT_TYPE;

    //! Emit prolog implementation.
    EmitProlog emitProlog;
    //! Emit epilog implementation.
    EmitEpilog emitEpilog;
    //! Emit arguments assignment implementation.
    EmitArgsAssignment emitArgsAssignment;
    //! Instruction formatter implementation.
    FormatInstruction formatInstruction;
    //! Instruction validation implementation.
    ValidateFunc validate;

    //! Resets all functions to nullptr.
    inline void reset() noexcept {
      emitProlog = nullptr;
      emitEpilog = nullptr;
      emitArgsAssignment = nullptr;
      validate = nullptr;
    }
  };

  Funcs _funcs {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_API explicit BaseEmitter(EmitterType emitterType) noexcept;
  ASMJIT_API virtual ~BaseEmitter() noexcept;

  //! \}

  //! \name Cast
  //! \{

  template<typename T>
  inline T* as() noexcept { return reinterpret_cast<T*>(this); }

  template<typename T>
  inline const T* as() const noexcept { return reinterpret_cast<const T*>(this); }

  //! \}

  //! \name Emitter Type & Flags
  //! \{

  //! Returns the type of this emitter, see `EmitterType`.
  inline EmitterType emitterType() const noexcept { return _emitterType; }
  //! Returns emitter flags , see `Flags`.
  inline EmitterFlags emitterFlags() const noexcept { return _emitterFlags; }

  //! Tests whether the emitter inherits from `BaseAssembler`.
  inline bool isAssembler() const noexcept { return _emitterType == EmitterType::kAssembler; }
  //! Tests whether the emitter inherits from `BaseBuilder`.
  //!
  //! \note Both Builder and Compiler emitters would return `true`.
  inline bool isBuilder() const noexcept { return uint32_t(_emitterType) >= uint32_t(EmitterType::kBuilder); }
  //! Tests whether the emitter inherits from `BaseCompiler`.
  inline bool isCompiler() const noexcept { return _emitterType == EmitterType::kCompiler; }

  //! Tests whether the emitter has the given `flag` enabled.
  inline bool hasEmitterFlag(EmitterFlags flag) const noexcept { return Support::test(_emitterFlags, flag); }
  //! Tests whether the emitter is finalized.
  inline bool isFinalized() const noexcept { return hasEmitterFlag(EmitterFlags::kFinalized); }
  //! Tests whether the emitter is destroyed (only used during destruction).
  inline bool isDestroyed() const noexcept { return hasEmitterFlag(EmitterFlags::kDestroyed); }

  inline void _addEmitterFlags(EmitterFlags flags) noexcept { _emitterFlags |= flags; }
  inline void _clearEmitterFlags(EmitterFlags flags) noexcept { _emitterFlags &= _emitterFlags & ~flags; }

  //! \}

  //! \name Target Information
  //! \{

  //! Returns the CodeHolder this emitter is attached to.
  inline CodeHolder* code() const noexcept { return _code; }

  //! Returns the target environment.
  //!
  //! The returned \ref Environment reference matches \ref CodeHolder::environment().
  inline const Environment& environment() const noexcept { return _environment; }

  //! Tests whether the target architecture is 32-bit.
  inline bool is32Bit() const noexcept { return environment().is32Bit(); }
  //! Tests whether the target architecture is 64-bit.
  inline bool is64Bit() const noexcept { return environment().is64Bit(); }

  //! Returns the target architecture type.
  inline Arch arch() const noexcept { return environment().arch(); }
  //! Returns the target architecture sub-type.
  inline SubArch subArch() const noexcept { return environment().subArch(); }

  //! Returns the target architecture's GP register size (4 or 8 bytes).
  inline uint32_t registerSize() const noexcept { return environment().registerSize(); }

  //! \}

  //! \name Initialization & Finalization
  //! \{

  //! Tests whether the emitter is initialized (i.e. attached to \ref CodeHolder).
  inline bool isInitialized() const noexcept { return _code != nullptr; }

  //! Finalizes this emitter.
  //!
  //! Materializes the content of the emitter by serializing it to the attached \ref CodeHolder through an architecture
  //! specific \ref BaseAssembler. This function won't do anything if the emitter inherits from \ref BaseAssembler as
  //! assemblers emit directly to a \ref CodeBuffer held by \ref CodeHolder. However, if this is an emitter that
  //! inherits from \ref BaseBuilder or \ref BaseCompiler then these emitters need the materialization phase as they
  //! store their content in a representation not visible to \ref CodeHolder.
  ASMJIT_API virtual Error finalize();

  //! \}

  //! \name Logging
  //! \{

  //! Tests whether the emitter has a logger.
  inline bool hasLogger() const noexcept { return _logger != nullptr; }

  //! Tests whether the emitter has its own logger.
  //!
  //! Own logger means that it overrides the possible logger that may be used by \ref CodeHolder this emitter is
  //! attached to.
  inline bool hasOwnLogger() const noexcept { return hasEmitterFlag(EmitterFlags::kOwnLogger); }

  //! Returns the logger this emitter uses.
  //!
  //! The returned logger is either the emitter's own logger or it's logger used by \ref CodeHolder this emitter
  //! is attached to.
  inline Logger* logger() const noexcept { return _logger; }

  //! Sets or resets the logger of the emitter.
  //!
  //! If the `logger` argument is non-null then the logger will be considered emitter's own logger, see \ref
  //! hasOwnLogger() for more details. If the given `logger` is null then the emitter will automatically use logger
  //! that is attached to the \ref CodeHolder this emitter is attached to.
  ASMJIT_API void setLogger(Logger* logger) noexcept;

  //! Resets the logger of this emitter.
  //!
  //! The emitter will bail to using a logger attached to \ref CodeHolder this emitter is attached to, or no logger
  //! at all if \ref CodeHolder doesn't have one.
  inline void resetLogger() noexcept { return setLogger(nullptr); }

  //! \}

  //! \name Error Handling
  //! \{

  //! Tests whether the emitter has an error handler attached.
  inline bool hasErrorHandler() const noexcept { return _errorHandler != nullptr; }

  //! Tests whether the emitter has its own error handler.
  //!
  //! Own error handler means that it overrides the possible error handler that may be used by \ref CodeHolder this
  //! emitter is attached to.
  inline bool hasOwnErrorHandler() const noexcept { return hasEmitterFlag(EmitterFlags::kOwnErrorHandler); }

  //! Returns the error handler this emitter uses.
  //!
  //! The returned error handler is either the emitter's own error handler or it's error handler used by
  //! \ref CodeHolder this emitter is attached to.
  inline ErrorHandler* errorHandler() const noexcept { return _errorHandler; }

  //! Sets or resets the error handler of the emitter.
  ASMJIT_API void setErrorHandler(ErrorHandler* errorHandler) noexcept;

  //! Resets the error handler.
  inline void resetErrorHandler() noexcept { setErrorHandler(nullptr); }

  //! Handles the given error in the following way:
  //!   1. If the emitter has \ref ErrorHandler attached, it calls its \ref ErrorHandler::handleError() member function
  //!      first, and then returns the error. The `handleError()` function may throw.
  //!   2. if the emitter doesn't have \ref ErrorHandler, the error is simply returned.
  ASMJIT_API Error reportError(Error err, const char* message = nullptr);

  //! \}

  //! \name Encoding Options
  //! \{

  //! Returns encoding options.
  inline EncodingOptions encodingOptions() const noexcept { return _encodingOptions; }
  //! Tests whether the encoding `option` is set.
  inline bool hasEncodingOption(EncodingOptions option) const noexcept { return Support::test(_encodingOptions, option); }

  //! Enables the given encoding `options`.
  inline void addEncodingOptions(EncodingOptions options) noexcept { _encodingOptions |= options; }
  //! Disables the given encoding `options`.
  inline void clearEncodingOptions(EncodingOptions options) noexcept { _encodingOptions &= ~options; }

  //! \}

  //! \name Diagnostic Options
  //! \{

  //! Returns the emitter's diagnostic options.
  inline DiagnosticOptions diagnosticOptions() const noexcept { return _diagnosticOptions; }

  //! Tests whether the given `option` is present in the emitter's diagnostic options.
  inline bool hasDiagnosticOption(DiagnosticOptions option) const noexcept { return Support::test(_diagnosticOptions, option); }

  //! Activates the given diagnostic `options`.
  //!
  //! This function is used to activate explicit validation options that will be then used by all emitter
  //! implementations. There are in general two possibilities:
  //!
  //!   - Architecture specific assembler is used. In this case a \ref DiagnosticOptions::kValidateAssembler can be
  //!     used to turn on explicit validation that will be used before an instruction is emitted. This means that
  //!     internally an extra step will be performed to make sure that the instruction is correct. This is needed,
  //!     because by default assemblers prefer speed over strictness.
  //!
  //!     This option should be used in debug builds as it's pretty expensive.
  //!
  //!   - Architecture specific builder or compiler is used. In this case the user can turn on
  //!     \ref DiagnosticOptions::kValidateIntermediate option that adds explicit validation step before the Builder
  //!     or Compiler creates an \ref InstNode to represent an emitted instruction. Error will be returned if the
  //!     instruction is ill-formed. In addition, also \ref DiagnosticOptions::kValidateAssembler can be used, which
  //!     would not be consumed by Builder / Compiler directly, but it would be propagated to an architecture specific
  //!     \ref BaseAssembler implementation it creates during \ref BaseEmitter::finalize().
  ASMJIT_API void addDiagnosticOptions(DiagnosticOptions options) noexcept;

  //! Deactivates the given validation `options`.
  //!
  //! See \ref addDiagnosticOptions() and \ref DiagnosticOptions for more details.
  ASMJIT_API void clearDiagnosticOptions(DiagnosticOptions options) noexcept;

  //! \}

  //! \name Instruction Options
  //! \{

  //! Returns forced instruction options.
  //!
  //! Forced instruction options are merged with next instruction options before the instruction is encoded. These
  //! options have some bits reserved that are used by error handling, logging, and instruction validation purposes.
  //! Other options are globals that affect each instruction.
  inline InstOptions forcedInstOptions() const noexcept { return _forcedInstOptions; }

  //! Returns options of the next instruction.
  inline InstOptions instOptions() const noexcept { return _instOptions; }
  //! Returns options of the next instruction.
  inline void setInstOptions(InstOptions options) noexcept { _instOptions = options; }
  //! Adds options of the next instruction.
  inline void addInstOptions(InstOptions options) noexcept { _instOptions |= options; }
  //! Resets options of the next instruction.
  inline void resetInstOptions() noexcept { _instOptions = InstOptions::kNone; }

  //! Tests whether the extra register operand is valid.
  inline bool hasExtraReg() const noexcept { return _extraReg.isReg(); }
  //! Returns an extra operand that will be used by the next instruction (architecture specific).
  inline const RegOnly& extraReg() const noexcept { return _extraReg; }
  //! Sets an extra operand that will be used by the next instruction (architecture specific).
  inline void setExtraReg(const BaseReg& reg) noexcept { _extraReg.init(reg); }
  //! Sets an extra operand that will be used by the next instruction (architecture specific).
  inline void setExtraReg(const RegOnly& reg) noexcept { _extraReg.init(reg); }
  //! Resets an extra operand that will be used by the next instruction (architecture specific).
  inline void resetExtraReg() noexcept { _extraReg.reset(); }

  //! Returns comment/annotation of the next instruction.
  inline const char* inlineComment() const noexcept { return _inlineComment; }
  //! Sets comment/annotation of the next instruction.
  //!
  //! \note This string is set back to null by `_emit()`, but until that it has to remain valid as the Emitter is not
  //! required to make a copy of it (and it would be slow to do that for each instruction).
  inline void setInlineComment(const char* s) noexcept { _inlineComment = s; }
  //! Resets the comment/annotation to nullptr.
  inline void resetInlineComment() noexcept { _inlineComment = nullptr; }

  //! \}

  //! \name Sections
  //! \{

  virtual Error section(Section* section) = 0;

  //! \}

  //! \name Labels
  //! \{

  //! Creates a new label.
  virtual Label newLabel() = 0;
  //! Creates a new named label.
  virtual Label newNamedLabel(const char* name, size_t nameSize = SIZE_MAX, LabelType type = LabelType::kGlobal, uint32_t parentId = Globals::kInvalidId) = 0;

  //! Creates a new anonymous label with a name, which can only be used for debugging purposes.
  inline Label newAnonymousLabel(const char* name, size_t nameSize = SIZE_MAX) { return newNamedLabel(name, nameSize, LabelType::kAnonymous); }
  //! Creates a new external label.
  inline Label newExternalLabel(const char* name, size_t nameSize = SIZE_MAX) { return newNamedLabel(name, nameSize, LabelType::kExternal); }

  //! Returns `Label` by `name`.
  //!
  //! Returns invalid Label in case that the name is invalid or label was not found.
  //!
  //! \note This function doesn't trigger ErrorHandler in case the name is invalid or no such label exist. You must
  //! always check the validity of the `Label` returned.
  ASMJIT_API Label labelByName(const char* name, size_t nameSize = SIZE_MAX, uint32_t parentId = Globals::kInvalidId) noexcept;

  //! Binds the `label` to the current position of the current section.
  //!
  //! \note Attempt to bind the same label multiple times will return an error.
  virtual Error bind(const Label& label) = 0;

  //! Tests whether the label `id` is valid (i.e. registered).
  ASMJIT_API bool isLabelValid(uint32_t labelId) const noexcept;
  //! Tests whether the `label` is valid (i.e. registered).
  inline bool isLabelValid(const Label& label) const noexcept { return isLabelValid(label.id()); }

  //! \}

  //! \name Emit
  //! \{

  // NOTE: These `emit()` helpers are designed to address a code-bloat generated by C++ compilers to call a function
  // having many arguments. Each parameter to `_emit()` requires some code to pass it, which means that if we default
  // to 5 arguments in `_emit()` and instId the C++ compiler would have to generate a virtual function call having 5
  // parameters and additional `this` argument, which is quite a lot. Since by default most instructions have 2 to 3
  // operands it's better to introduce helpers that pass from 0 to 6 operands that help to reduce the size of emit(...)
  // function call.

  //! Emits an instruction (internal).
  ASMJIT_API Error _emitI(InstId instId);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0, const Operand_& o1);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4);
  //! \overload
  ASMJIT_API Error _emitI(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_& o3, const Operand_& o4, const Operand_& o5);

  //! Emits an instruction `instId` with the given `operands`.
  template<typename... Args>
  ASMJIT_FORCE_INLINE Error emit(InstId instId, Args&&... operands) {
    return _emitI(instId, Support::ForwardOp<Args>::forward(operands)...);
  }

  ASMJIT_FORCE_INLINE Error emitOpArray(InstId instId, const Operand_* operands, size_t opCount) {
    return _emitOpArray(instId, operands, opCount);
  }

  ASMJIT_FORCE_INLINE Error emitInst(const BaseInst& inst, const Operand_* operands, size_t opCount) {
    setInstOptions(inst.options());
    setExtraReg(inst.extraReg());
    return _emitOpArray(inst.id(), operands, opCount);
  }

  //! \cond INTERNAL
  //! Emits an instruction - all 6 operands must be defined.
  virtual Error _emit(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* oExt) = 0;
  //! Emits instruction having operands stored in array.
  ASMJIT_API virtual Error _emitOpArray(InstId instId, const Operand_* operands, size_t opCount);
  //! \endcond

  //! \}

  //! \name Emit Utilities
  //! \{

  ASMJIT_API Error emitProlog(const FuncFrame& frame);
  ASMJIT_API Error emitEpilog(const FuncFrame& frame);
  ASMJIT_API Error emitArgsAssignment(const FuncFrame& frame, const FuncArgsAssignment& args);

  //! \}

  //! \name Align
  //! \{

  //! Aligns the current CodeBuffer position to the `alignment` specified.
  //!
  //! The sequence that is used to fill the gap between the aligned location and the current location depends on the
  //! align `mode`, see \ref AlignMode. The `alignment` argument specifies alignment in bytes, so for example when
  //! it's `32` it means that the code buffer will be aligned to `32` bytes.
  virtual Error align(AlignMode alignMode, uint32_t alignment) = 0;

  //! \}

  //! \name Embed
  //! \{

  //! Embeds raw data into the \ref CodeBuffer.
  virtual Error embed(const void* data, size_t dataSize) = 0;

  //! Embeds a typed data array.
  //!
  //! This is the most flexible function for embedding data as it allows to:
  //!
  //!   - Assign a `typeId` to the data, so the emitter knows the type of items stored in `data`. Binary data should
  //!     use \ref TypeId::kUInt8.
  //!
  //!   - Repeat the given data `repeatCount` times, so the data can be used as a fill pattern for example, or as a
  //!     pattern used by SIMD instructions.
  virtual Error embedDataArray(TypeId typeId, const void* data, size_t itemCount, size_t repeatCount = 1) = 0;

  //! Embeds int8_t `value` repeated by `repeatCount`.
  inline Error embedInt8(int8_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kInt8, &value, 1, repeatCount); }
  //! Embeds uint8_t `value` repeated by `repeatCount`.
  inline Error embedUInt8(uint8_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kUInt8, &value, 1, repeatCount); }
  //! Embeds int16_t `value` repeated by `repeatCount`.
  inline Error embedInt16(int16_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kInt16, &value, 1, repeatCount); }
  //! Embeds uint16_t `value` repeated by `repeatCount`.
  inline Error embedUInt16(uint16_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kUInt16, &value, 1, repeatCount); }
  //! Embeds int32_t `value` repeated by `repeatCount`.
  inline Error embedInt32(int32_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kInt32, &value, 1, repeatCount); }
  //! Embeds uint32_t `value` repeated by `repeatCount`.
  inline Error embedUInt32(uint32_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kUInt32, &value, 1, repeatCount); }
  //! Embeds int64_t `value` repeated by `repeatCount`.
  inline Error embedInt64(int64_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kInt64, &value, 1, repeatCount); }
  //! Embeds uint64_t `value` repeated by `repeatCount`.
  inline Error embedUInt64(uint64_t value, size_t repeatCount = 1) { return embedDataArray(TypeId::kUInt64, &value, 1, repeatCount); }
  //! Embeds a floating point `value` repeated by `repeatCount`.
  inline Error embedFloat(float value, size_t repeatCount = 1) { return embedDataArray(TypeId(TypeUtils::TypeIdOfT<float>::kTypeId), &value, 1, repeatCount); }
  //! Embeds a floating point `value` repeated by `repeatCount`.
  inline Error embedDouble(double value, size_t repeatCount = 1) { return embedDataArray(TypeId(TypeUtils::TypeIdOfT<double>::kTypeId), &value, 1, repeatCount); }

  //! Embeds a constant pool at the current offset by performing the following:
  //!   1. Aligns by using AlignMode::kData to the minimum `pool` alignment.
  //!   2. Binds the ConstPool label so it's bound to an aligned location.
  //!   3. Emits ConstPool content.
  virtual Error embedConstPool(const Label& label, const ConstPool& pool) = 0;

  //! Embeds an absolute `label` address as data.
  //!
  //! The `dataSize` is an optional argument that can be used to specify the size of the address data. If it's zero
  //! (default) the address size is deduced from the target architecture (either 4 or 8 bytes).
  virtual Error embedLabel(const Label& label, size_t dataSize = 0) = 0;

  //! Embeds a delta (distance) between the `label` and `base` calculating it as `label - base`. This function was
  //! designed to make it easier to embed lookup tables where each index is a relative distance of two labels.
  virtual Error embedLabelDelta(const Label& label, const Label& base, size_t dataSize = 0) = 0;

  //! \}

  //! \name Comment
  //! \{

  //! Emits a comment stored in `data` with an optional `size` parameter.
  virtual Error comment(const char* data, size_t size = SIZE_MAX) = 0;

  //! Emits a formatted comment specified by `fmt` and variable number of arguments.
  ASMJIT_API Error commentf(const char* fmt, ...);
  //! Emits a formatted comment specified by `fmt` and `ap`.
  ASMJIT_API Error commentv(const char* fmt, va_list ap);

  //! \}

  //! \name Events
  //! \{

  //! Called after the emitter was attached to `CodeHolder`.
  virtual Error onAttach(CodeHolder* ASMJIT_NONNULL(code)) noexcept = 0;
  //! Called after the emitter was detached from `CodeHolder`.
  virtual Error onDetach(CodeHolder* ASMJIT_NONNULL(code)) noexcept = 0;

  //! Called when \ref CodeHolder has updated an important setting, which involves the following:
  //!
  //!   - \ref Logger has been changed (\ref CodeHolder::setLogger() has been called).
  //!
  //!   - \ref ErrorHandler has been changed (\ref CodeHolder::setErrorHandler() has been called).
  //!
  //! This function ensures that the settings are properly propagated from \ref CodeHolder to the emitter.
  //!
  //! \note This function is virtual and can be overridden, however, if you do so, always call \ref
  //! BaseEmitter::onSettingsUpdated() within your own implementation to ensure that the emitter is
  //! in a consistent state.
  ASMJIT_API virtual void onSettingsUpdated() noexcept;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_EMITTER_H_INCLUDED
