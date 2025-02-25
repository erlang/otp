// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_FUNC_H_INCLUDED
#define ASMJIT_CORE_FUNC_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/environment.h"
#include "../core/operand.h"
#include "../core/type.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_function
//! \{

//! Calling convention id.
//!
//! Calling conventions can be divided into the following groups:
//!
//!   - Universal - calling conventions are applicable to any target. They will be converted to a target dependent
//!     calling convention at runtime by \ref CallConv::init() with some help from \ref Environment. The purpose of
//!     these calling conventions is to make using functions less target dependent and closer to C and C++.
//!
//!   - Target specific - calling conventions that are used by a particular architecture and ABI. For example
//!     Windows 64-bit calling convention and AMD64 SystemV calling convention.
enum class CallConvId : uint8_t {
  // Universal Calling Conventions
  // -----------------------------

  //! Standard function call or explicit `__cdecl` where it can be specified.
  //!
  //! This is a universal calling convention, which is used to initialize specific calling conventions based on
  //! architecture, platform, and its ABI.
  kCDecl = 0,

  //! `__stdcall` on targets that support this calling convention (X86).
  //!
  //! \note This calling convention is only supported on 32-bit X86. If used on environment that doesn't support
  //! this calling convention it will be replaced by \ref CallConvId::kCDecl.
  kStdCall = 1,

  //! `__fastcall` on targets that support this calling convention (X86).
  //!
  //! \note This calling convention is only supported on 32-bit X86. If used on environment that doesn't support
  //! this calling convention it will be replaced by \ref CallConvId::kCDecl.
  kFastCall = 2,

  //! `__vectorcall` on targets that support this calling convention (X86/X64).
  //!
  //! \note This calling convention is only supported on 32-bit and 64-bit X86 architecture on Windows platform.
  //! If used on environment that doesn't support this calling it will be replaced by \ref CallConvId::kCDecl.
  kVectorCall = 3,

  //! `__thiscall` on targets that support this calling convention (X86).
  //!
  //! \note This calling convention is only supported on 32-bit X86 Windows platform. If used on environment that
  //! doesn't support this calling convention it will be replaced by \ref CallConvId::kCDecl.
  kThisCall = 4,

  //! `__attribute__((regparm(1)))` convention (GCC and Clang).
  kRegParm1 = 5,
  //! `__attribute__((regparm(2)))` convention (GCC and Clang).
  kRegParm2 = 6,
  //! `__attribute__((regparm(3)))` convention (GCC and Clang).
  kRegParm3 = 7,

  //! AsmJit specific calling convention designed for calling functions inside a multimedia code that don't use many
  //! registers internally, but are long enough to be called and not inlined. These functions are usually used to
  //! calculate trigonometric functions, logarithms, etc...
  kLightCall2 = 16,
  kLightCall3 = 17,
  kLightCall4 = 18,

  // ABI-Specific Calling Conventions
  // --------------------------------

  //! Soft-float calling convention (AArch32).
  //!
  //! Floating point arguments are passed via general purpose registers.
  kSoftFloat = 30,

  //! Hard-float calling convention (AArch32).
  //!
  //! Floating point arguments are passed via SIMD registers.
  kHardFloat = 31,

  //! X64 System-V calling convention.
  kX64SystemV = 32,
  //! X64 Windows calling convention.
  kX64Windows = 33,

  //! Maximum value of `CallConvId`.
  kMaxValue = kX64Windows

  // Deprecated Aliases
  // ------------------

#if !defined(ASMJIT_NO_DEPRECATED)
  ,
  kNone = kCDecl,
  kHost = kCDecl
#endif // !ASMJIT_NO_DEPRECATED
};

//! Strategy used by calling conventions to assign registers to function arguments.
//!
//! Calling convention strategy describes how AsmJit should convert function arguments used by \ref FuncSignature
//! into register identifiers and stack offsets. The \ref CallConvStrategy::kDefault strategy assigns registers
//! and then stack whereas \ref CallConvStrategy::kX64Windows strategy does register shadowing as defined by WIN64
//! calling convention, which is only used by 64-bit Windows.
enum class CallConvStrategy : uint8_t {
  //! Default register assignment strategy.
  kDefault = 0,
  //! Windows 64-bit ABI register assignment strategy.
  kX64Windows = 1,
  //! Windows 64-bit __vectorcall register assignment strategy.
  kX64VectorCall = 2,
  //! Apple's AArch64 calling convention (differs compared to AArch64 calling convention used by Linux).
  kAArch64Apple = 3,

  //! Maximum value of `CallConvStrategy`.
  kMaxValue = kX64VectorCall
};

//! Calling convention flags.
enum class CallConvFlags : uint32_t {
  //! No flags.
  kNone = 0,
  //! Callee is responsible for cleaning up the stack.
  kCalleePopsStack = 0x0001u,
  //! Pass vector arguments indirectly (as a pointer).
  kIndirectVecArgs = 0x0002u,
  //! Pass F32 and F64 arguments via VEC128 register.
  kPassFloatsByVec = 0x0004u,
  //! Pass MMX and vector arguments via stack if the function has variable arguments.
  kPassVecByStackIfVA = 0x0008u,
  //! MMX registers are passed and returned via GP registers.
  kPassMmxByGp = 0x0010u,
  //! MMX registers are passed and returned via XMM registers.
  kPassMmxByXmm = 0x0020u,
  //! Calling convention can be used with variable arguments.
  kVarArgCompatible = 0x0080u
};
ASMJIT_DEFINE_ENUM_FLAGS(CallConvFlags)

//! Function calling convention.
//!
//! Function calling convention is a scheme that defines how function parameters are passed and how function
//! returns its result. AsmJit defines a variety of architecture and OS specific calling conventions and also
//! provides a compile time detection to make the code-generation easier.
struct CallConv {
  //! \name Constants
  //! \{

  //! Maximum number of register arguments per register group.
  //!
  //! \note This is not really AsmJit's limitation, it's just the number that makes sense considering all common
  //! calling conventions. Usually even conventions that use registers to pass function arguments are limited to 8
  //! and less arguments passed via registers per group.
  static constexpr uint32_t kMaxRegArgsPerGroup = 16;

  //! \}

  //! \name Members
  //! \{

  //! Target architecture.
  Arch _arch;
  //! Calling convention id.
  CallConvId _id;
  //! Register assignment strategy.
  CallConvStrategy _strategy;

  //! Red zone size (AMD64 == 128 bytes).
  uint8_t _redZoneSize;
  //! Spill zone size (WIN-X64 == 32 bytes).
  uint8_t _spillZoneSize;
  //! Natural stack alignment as defined by OS/ABI.
  uint8_t _naturalStackAlignment;

  //! \cond INTERNAL
  //! Reserved for future use.
  uint8_t _reserved[2];
  //! \endcond

  //! Calling convention flags.
  CallConvFlags _flags;

  //! Size to save/restore per register group.
  Support::Array<uint8_t, Globals::kNumVirtGroups> _saveRestoreRegSize;
  //! Alignment of save/restore groups.
  Support::Array<uint8_t, Globals::kNumVirtGroups> _saveRestoreAlignment;

  //! Mask of all passed registers, per group.
  Support::Array<RegMask, Globals::kNumVirtGroups> _passedRegs;
  //! Mask of all preserved registers, per group.
  Support::Array<RegMask, Globals::kNumVirtGroups> _preservedRegs;

  //! Passed registers' order.
  union RegOrder {
    //! Passed registers, ordered.
    uint8_t id[kMaxRegArgsPerGroup];
    //! Packed IDs in `uint32_t` array.
    uint32_t packed[(kMaxRegArgsPerGroup + 3) / 4];
  };

  //! Passed registers' order, per register group.
  Support::Array<RegOrder, Globals::kNumVirtGroups> _passedOrder;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Initializes this calling convention to the given `ccId` based on the `environment`.
  //!
  //! See \ref CallConvId and \ref Environment for more details.
  ASMJIT_API Error init(CallConvId ccId, const Environment& environment) noexcept;

  //! Resets this CallConv struct into a defined state.
  //!
  //! It's recommended to reset the \ref CallConv struct in case you would like create a custom calling convention
  //! as it prevents from using an uninitialized data (CallConv doesn't have a constructor that would initialize it,
  //! it's just a struct).
  ASMJIT_INLINE_NODEBUG void reset() noexcept {
    *this = CallConv{};
    memset(_passedOrder.data(), 0xFF, sizeof(_passedOrder));
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the target architecture of this calling convention.
  ASMJIT_INLINE_NODEBUG Arch arch() const noexcept { return _arch; }
  //! Sets the target architecture of this calling convention.
  ASMJIT_INLINE_NODEBUG void setArch(Arch arch) noexcept { _arch = arch; }

  //! Returns the calling convention id.
  ASMJIT_INLINE_NODEBUG CallConvId id() const noexcept { return _id; }
  //! Sets the calling convention id.
  ASMJIT_INLINE_NODEBUG void setId(CallConvId ccId) noexcept { _id = ccId; }

  //! Returns the strategy used to assign registers to arguments.
  ASMJIT_INLINE_NODEBUG CallConvStrategy strategy() const noexcept { return _strategy; }
  //! Sets the strategy used to assign registers to arguments.
  ASMJIT_INLINE_NODEBUG void setStrategy(CallConvStrategy ccStrategy) noexcept { _strategy = ccStrategy; }

  //! Tests whether the calling convention has the given `flag` set.
  ASMJIT_INLINE_NODEBUG bool hasFlag(CallConvFlags flag) const noexcept { return Support::test(_flags, flag); }
  //! Returns the calling convention flags, see `Flags`.
  ASMJIT_INLINE_NODEBUG CallConvFlags flags() const noexcept { return _flags; }
  //! Adds the calling convention flags, see `Flags`.
  ASMJIT_INLINE_NODEBUG void setFlags(CallConvFlags flag) noexcept { _flags = flag; };
  //! Adds the calling convention flags, see `Flags`.
  ASMJIT_INLINE_NODEBUG void addFlags(CallConvFlags flags) noexcept { _flags |= flags; };

  //! Tests whether this calling convention specifies 'RedZone'.
  ASMJIT_INLINE_NODEBUG bool hasRedZone() const noexcept { return _redZoneSize != 0; }
  //! Tests whether this calling convention specifies 'SpillZone'.
  ASMJIT_INLINE_NODEBUG bool hasSpillZone() const noexcept { return _spillZoneSize != 0; }

  //! Returns size of 'RedZone'.
  ASMJIT_INLINE_NODEBUG uint32_t redZoneSize() const noexcept { return _redZoneSize; }
  //! Returns size of 'SpillZone'.
  ASMJIT_INLINE_NODEBUG uint32_t spillZoneSize() const noexcept { return _spillZoneSize; }

  //! Sets size of 'RedZone'.
  ASMJIT_INLINE_NODEBUG void setRedZoneSize(uint32_t size) noexcept { _redZoneSize = uint8_t(size); }
  //! Sets size of 'SpillZone'.
  ASMJIT_INLINE_NODEBUG void setSpillZoneSize(uint32_t size) noexcept { _spillZoneSize = uint8_t(size); }

  //! Returns a natural stack alignment.
  ASMJIT_INLINE_NODEBUG uint32_t naturalStackAlignment() const noexcept { return _naturalStackAlignment; }
  //! Sets a natural stack alignment.
  //!
  //! This function can be used to override the default stack alignment in case that you know that it's alignment is
  //! different. For example it allows to implement custom calling conventions that guarantee higher stack alignment.
  ASMJIT_INLINE_NODEBUG void setNaturalStackAlignment(uint32_t value) noexcept { _naturalStackAlignment = uint8_t(value); }

  //! Returns the size of a register (or its part) to be saved and restored of the given `group`.
  ASMJIT_INLINE_NODEBUG uint32_t saveRestoreRegSize(RegGroup group) const noexcept { return _saveRestoreRegSize[group]; }
  //! Sets the size of a vector register (or its part) to be saved and restored.
  ASMJIT_INLINE_NODEBUG void setSaveRestoreRegSize(RegGroup group, uint32_t size) noexcept { _saveRestoreRegSize[group] = uint8_t(size); }

  //! Returns the alignment of a save-restore area of the given `group`.
  ASMJIT_INLINE_NODEBUG uint32_t saveRestoreAlignment(RegGroup group) const noexcept { return _saveRestoreAlignment[group]; }
  //! Sets the alignment of a save-restore area of the given `group`.
  ASMJIT_INLINE_NODEBUG void setSaveRestoreAlignment(RegGroup group, uint32_t alignment) noexcept { _saveRestoreAlignment[group] = uint8_t(alignment); }

  //! Returns the order of passed registers of the given `group`.
  inline const uint8_t* passedOrder(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _passedOrder[size_t(group)].id;
  }

  //! Returns the mask of passed registers of the given `group`.
  inline RegMask passedRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _passedRegs[size_t(group)];
  }

  inline void _setPassedPacked(RegGroup group, uint32_t p0, uint32_t p1, uint32_t p2, uint32_t p3) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);

    _passedOrder[group].packed[0] = p0;
    _passedOrder[group].packed[1] = p1;
    _passedOrder[group].packed[2] = p2;
    _passedOrder[group].packed[3] = p3;
  }

  //! Resets the order and mask of passed registers.
  inline void setPassedToNone(RegGroup group) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);

    _setPassedPacked(group, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);
    _passedRegs[size_t(group)] = 0u;
  }

  //! Sets the order and mask of passed registers.
  inline void setPassedOrder(RegGroup group, uint32_t a0, uint32_t a1 = 0xFF, uint32_t a2 = 0xFF, uint32_t a3 = 0xFF, uint32_t a4 = 0xFF, uint32_t a5 = 0xFF, uint32_t a6 = 0xFF, uint32_t a7 = 0xFF) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);

    // NOTE: This should always be called with all arguments known at compile time, so even if it looks scary it
    // should be translated into few instructions.
    _setPassedPacked(group, Support::bytepack32_4x8(a0, a1, a2, a3),
                            Support::bytepack32_4x8(a4, a5, a6, a7),
                            0xFFFFFFFFu,
                            0xFFFFFFFFu);

    _passedRegs[group] = (a0 != 0xFF ? 1u << a0 : 0u) |
                         (a1 != 0xFF ? 1u << a1 : 0u) |
                         (a2 != 0xFF ? 1u << a2 : 0u) |
                         (a3 != 0xFF ? 1u << a3 : 0u) |
                         (a4 != 0xFF ? 1u << a4 : 0u) |
                         (a5 != 0xFF ? 1u << a5 : 0u) |
                         (a6 != 0xFF ? 1u << a6 : 0u) |
                         (a7 != 0xFF ? 1u << a7 : 0u) ;
  }

  //! Returns preserved register mask of the given `group`.
  inline RegMask preservedRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _preservedRegs[group];
  }

  //! Sets preserved register mask of the given `group`.
  inline void setPreservedRegs(RegGroup group, RegMask regs) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    _preservedRegs[group] = regs;
  }

  //! \}
};

//! Function signature.
//!
//! Contains information about a function return type, count of arguments, and their TypeIds. Function signature
//! is a low level structure which doesn't contain platform specific or calling convention specific information.
//! It's typically used to describe function arguments in a C-API like form, which is then used to calculate a
//! \ref FuncDetail instance, which then maps function signature into a platform and calling convention specific
//! format.
//!
//! Function signature can be built either dynamically by using \ref addArg() and \ref addArgT() functionality,
//! or dynamically by using a template-based \ref FuncSignature::build() function, which maps template types
//! into a function signature.
struct FuncSignature {
  //! \name Constants
  //! \{

  //! Doesn't have variable number of arguments (`...`).
  static constexpr uint8_t kNoVarArgs = 0xFFu;

  //! \}

  //! \name Members
  //! \{

  //! Calling convention id.
  CallConvId _ccId = CallConvId::kCDecl;
  //! Count of arguments.
  uint8_t _argCount = 0;
  //! Index of a first VA or `kNoVarArgs`.
  uint8_t _vaIndex = kNoVarArgs;
  //! Return value TypeId.
  TypeId _ret = TypeId::kVoid;
  //! Reserved for future use.
  uint8_t _reserved[4] {};
  //! Function argument TypeIds.
  TypeId _args[Globals::kMaxFuncArgs] {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Default constructed function signature, initialized to \ref CallConvId::kCDecl, having no return value and no arguments.
  ASMJIT_FORCE_INLINE constexpr FuncSignature() = default;

  //! Copy constructor, which is initialized to the same function signature as `other`.
  ASMJIT_FORCE_INLINE constexpr FuncSignature(const FuncSignature& other) = default;

  //! Initializes the function signature with calling convention id `ccId` and variable argument's index `vaIndex`.
  ASMJIT_FORCE_INLINE constexpr FuncSignature(CallConvId ccId, uint32_t vaIndex = kNoVarArgs) noexcept
    : _ccId(ccId),
      _vaIndex(uint8_t(vaIndex)) {}

  //! Initializes the function signature with calling convention id `ccId`, `vaIndex`, return value, and function arguments.
  template<typename... Args>
  ASMJIT_FORCE_INLINE constexpr FuncSignature(CallConvId ccId, uint32_t vaIndex, TypeId ret, Args&&...args) noexcept
    : _ccId(ccId),
      _argCount(uint8_t(sizeof...(args))),
      _vaIndex(uint8_t(vaIndex)),
      _ret(ret),
      _args{std::forward<Args>(args)...} {}

  //! Builds a function signature based on `RetValueAndArgs`. The first template argument is a function return type,
  //! and function arguments follow.
  //!
  //! \note This function returns a new function signature, which can be passed to functions where it's required. It's
  //! a convenience function that allows to build function signature statically based on types known at compile time,
  //! which is common in JIT code generation.
  template<typename... RetValueAndArgs>
  static ASMJIT_INLINE_NODEBUG constexpr FuncSignature build(CallConvId ccId = CallConvId::kCDecl, uint32_t vaIndex = kNoVarArgs) noexcept {
    return FuncSignature(ccId, vaIndex, (TypeId(TypeUtils::TypeIdOfT<RetValueAndArgs>::kTypeId))... );
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Copy assignment - function signature can be copied by value.
  ASMJIT_FORCE_INLINE FuncSignature& operator=(const FuncSignature& other) noexcept = default;

  //! Compares this function signature with `other` for equality..
  ASMJIT_FORCE_INLINE bool operator==(const FuncSignature& other) const noexcept { return equals(other); }
  //! Compares this function signature with `other` for inequality..
  ASMJIT_FORCE_INLINE bool operator!=(const FuncSignature& other) const noexcept { return !equals(other); }

  //! \}

  //! \name Initialization & Reset
  //! \{

  //! Resets this function signature to a default constructed state.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { *this = FuncSignature{}; }

  //! \}

  //! \name Equality & Comparison
  //! \{

  //! Compares this function signature with `other` for equality..
  ASMJIT_INLINE_NODEBUG bool equals(const FuncSignature& other) const noexcept {
    return _ccId == other._ccId &&
           _argCount == other._argCount &&
           _vaIndex == other._vaIndex &&
           _ret == other._ret &&
           memcmp(_args, other._args, sizeof(_args)) == 0;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the calling convention.
  ASMJIT_INLINE_NODEBUG CallConvId callConvId() const noexcept { return _ccId; }
  //! Sets the calling convention to `ccId`;
  ASMJIT_INLINE_NODEBUG void setCallConvId(CallConvId ccId) noexcept { _ccId = ccId; }

  //! Tests whether the function signature has a return value.
  ASMJIT_INLINE_NODEBUG bool hasRet() const noexcept { return _ret != TypeId::kVoid; }
  //! Returns the type of the return value.
  ASMJIT_INLINE_NODEBUG TypeId ret() const noexcept { return _ret; }
  //! Sets the return type to `retType`.
  ASMJIT_INLINE_NODEBUG void setRet(TypeId retType) noexcept { _ret = retType; }
  //! Sets the return type based on `T`.
  template<typename T>
  ASMJIT_INLINE_NODEBUG void setRetT() noexcept { setRet(TypeId(TypeUtils::TypeIdOfT<T>::kTypeId)); }


  //! Returns the array of function arguments' types.
  ASMJIT_INLINE_NODEBUG const TypeId* args() const noexcept { return _args; }
  //! Returns the number of function arguments.
  ASMJIT_INLINE_NODEBUG uint32_t argCount() const noexcept { return _argCount; }

  //! Returns the type of the argument at index `i`.
  inline TypeId arg(uint32_t i) const noexcept {
    ASMJIT_ASSERT(i < _argCount);
    return _args[i];
  }

  //! Sets the argument at index `index` to `argType`.
  inline void setArg(uint32_t index, TypeId argType) noexcept {
    ASMJIT_ASSERT(index < _argCount);
    _args[index] = argType;
  }
  //! Sets the argument at index `i` to the type based on `T`.
  template<typename T>
  inline void setArgT(uint32_t index) noexcept { setArg(index, TypeId(TypeUtils::TypeIdOfT<T>::kTypeId)); }

  //! Tests whether an argument can be added to the signature, use before calling \ref addArg() and \ref addArgT().
  //!
  //! \note If you know that you are not adding more arguments than \ref Globals::kMaxFuncArgs then it's not necessary
  //! to use this function. However, if you are adding arguments based on user input, for example, then either check
  //! the number of arguments before using function signature or use \ref canAddArg() before actually adding them to
  //! the function signature.
  inline bool canAddArg() const noexcept { return _argCount < Globals::kMaxFuncArgs; }

  //! Appends an argument of `type` to the function prototype.
  inline void addArg(TypeId type) noexcept {
    ASMJIT_ASSERT(_argCount < Globals::kMaxFuncArgs);
    _args[_argCount++] = type;
  }

  //! Appends an argument of type based on `T` to the function prototype.
  template<typename T>
  inline void addArgT() noexcept { addArg(TypeId(TypeUtils::TypeIdOfT<T>::kTypeId)); }

  //! Tests whether the function has variable number of arguments (...).
  ASMJIT_INLINE_NODEBUG bool hasVarArgs() const noexcept { return _vaIndex != kNoVarArgs; }
  //! Returns the variable arguments (...) index, `kNoVarArgs` if none.
  ASMJIT_INLINE_NODEBUG uint32_t vaIndex() const noexcept { return _vaIndex; }
  //! Sets the variable arguments (...) index to `index`.
  ASMJIT_INLINE_NODEBUG void setVaIndex(uint32_t index) noexcept { _vaIndex = uint8_t(index); }
  //! Resets the variable arguments index (making it a non-va function).
  ASMJIT_INLINE_NODEBUG void resetVaIndex() noexcept { _vaIndex = kNoVarArgs; }

  //! \}
};

#if !defined(ASMJIT_NO_DEPRECATED)
template<typename... RetValueAndArgs>
class FuncSignatureT : public FuncSignature {
public:
  ASMJIT_DEPRECATED("Use FuncSignature::build<RetValueAndArgs>() instead")
  ASMJIT_INLINE_NODEBUG constexpr FuncSignatureT(CallConvId ccId = CallConvId::kCDecl, uint32_t vaIndex = kNoVarArgs) noexcept
    : FuncSignature(ccId, vaIndex, (TypeId(TypeUtils::TypeIdOfT<RetValueAndArgs>::kTypeId))... ) {}
};

ASMJIT_DEPRECATED("Use FuncSignature instead of FuncSignatureBuilder")
typedef FuncSignature FuncSignatureBuilder;
#endif // !ASMJIT_NO_DEPRECATED

//! Argument or return value (or its part) as defined by `FuncSignature`, but with register or stack address
//! (and other metadata) assigned.
struct FuncValue {
  //! \name Constants
  //! \{

  enum Bits : uint32_t {
    kTypeIdShift      = 0,             //!< TypeId shift.
    kTypeIdMask       = 0x000000FFu,   //!< TypeId mask.

    kFlagIsReg        = 0x00000100u,   //!< Passed by register.
    kFlagIsStack      = 0x00000200u,   //!< Passed by stack.
    kFlagIsIndirect   = 0x00000400u,   //!< Passed indirectly by reference (internally a pointer).
    kFlagIsDone       = 0x00000800u,   //!< Used internally by arguments allocator.

    kStackOffsetShift = 12,            //!< Stack offset shift.
    kStackOffsetMask  = 0xFFFFF000u,   //!< Stack offset mask (must occupy MSB bits).

    kRegIdShift       = 16,            //!< RegId shift.
    kRegIdMask        = 0x00FF0000u,   //!< RegId mask.

    kRegTypeShift     = 24,            //!< RegType shift.
    kRegTypeMask      = 0xFF000000u    //!< RegType mask.
  };

  //! \}

  //! \name Members
  //! \{

  uint32_t _data;

  //! \}

  //! \name Initialization & Reset
  //!
  //! These initialize the whole `FuncValue` to either register or stack. Useful when you know all of these
  //! properties and wanna just set it up.
  //!
  //! \{

  //! Initializes this `FuncValue` only to the `typeId` provided - the rest of the values will be cleared.
  ASMJIT_INLINE_NODEBUG void initTypeId(TypeId typeId) noexcept {
    _data = uint32_t(typeId) << kTypeIdShift;
  }

  //! Initializes this `FuncValue` to a register of `regType`, `regId`, and assigns its `typeId` and `flags`.
  ASMJIT_INLINE_NODEBUG void initReg(RegType regType, uint32_t regId, TypeId typeId, uint32_t flags = 0) noexcept {
    _data = (uint32_t(regType) << kRegTypeShift) | (regId << kRegIdShift) | (uint32_t(typeId) << kTypeIdShift) | kFlagIsReg | flags;
  }

  //! Initializes this `FuncValue` to a stack at the given `offset` and assigns its `typeId`.
  ASMJIT_INLINE_NODEBUG void initStack(int32_t offset, TypeId typeId) noexcept {
    _data = (uint32_t(offset) << kStackOffsetShift) | (uint32_t(typeId) << kTypeIdShift) | kFlagIsStack;
  }

  //! Resets the value to its unassigned state.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { _data = 0; }

  //! \}

  //! \name Assign
  //!
  //! These initialize only part of `FuncValue`, useful when building `FuncValue` incrementally. The caller
  //! should first init the type-id by calling `initTypeId` and then continue building either register or stack.
  //!
  //! \{

  //! Assigns a register of `regType` and `regId`.
  inline void assignRegData(RegType regType, uint32_t regId) noexcept {
    ASMJIT_ASSERT((_data & (kRegTypeMask | kRegIdMask)) == 0);
    _data |= (uint32_t(regType) << kRegTypeShift) | (regId << kRegIdShift) | kFlagIsReg;
  }

  //! Assigns a stack location at `offset`.
  inline void assignStackOffset(int32_t offset) noexcept {
    ASMJIT_ASSERT((_data & kStackOffsetMask) == 0);
    _data |= (uint32_t(offset) << kStackOffsetShift) | kFlagIsStack;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns true if the value is initialized (explicit bool cast).
  ASMJIT_INLINE_NODEBUG explicit operator bool() const noexcept { return _data != 0; }

  //! \cond INTERNAL
  ASMJIT_INLINE_NODEBUG void _replaceValue(uint32_t mask, uint32_t value) noexcept { _data = (_data & ~mask) | value; }
  //! \endcond

  //! Tests whether the `FuncValue` has a flag `flag` set.
  ASMJIT_INLINE_NODEBUG bool hasFlag(uint32_t flag) const noexcept { return Support::test(_data, flag); }
  //! Adds `flags` to `FuncValue`.
  ASMJIT_INLINE_NODEBUG void addFlags(uint32_t flags) noexcept { _data |= flags; }
  //! Clears `flags` of `FuncValue`.
  ASMJIT_INLINE_NODEBUG void clearFlags(uint32_t flags) noexcept { _data &= ~flags; }

  //! Tests whether the value is initialized (i.e. contains a valid data).
  ASMJIT_INLINE_NODEBUG bool isInitialized() const noexcept { return _data != 0; }
  //! Tests whether the argument is passed by register.
  ASMJIT_INLINE_NODEBUG bool isReg() const noexcept { return hasFlag(kFlagIsReg); }
  //! Tests whether the argument is passed by stack.
  ASMJIT_INLINE_NODEBUG bool isStack() const noexcept { return hasFlag(kFlagIsStack); }
  //! Tests whether the argument is passed by register.
  ASMJIT_INLINE_NODEBUG bool isAssigned() const noexcept { return hasFlag(kFlagIsReg | kFlagIsStack); }
  //! Tests whether the argument is passed through a pointer (used by WIN64 to pass XMM|YMM|ZMM).
  ASMJIT_INLINE_NODEBUG bool isIndirect() const noexcept { return hasFlag(kFlagIsIndirect); }

  //! Tests whether the argument was already processed (used internally).
  ASMJIT_INLINE_NODEBUG bool isDone() const noexcept { return hasFlag(kFlagIsDone); }

  //! Returns a register type of the register used to pass function argument or return value.
  ASMJIT_INLINE_NODEBUG RegType regType() const noexcept { return RegType((_data & kRegTypeMask) >> kRegTypeShift); }
  //! Sets a register type of the register used to pass function argument or return value.
  ASMJIT_INLINE_NODEBUG void setRegType(RegType regType) noexcept { _replaceValue(kRegTypeMask, uint32_t(regType) << kRegTypeShift); }

  //! Returns a physical id of the register used to pass function argument or return value.
  ASMJIT_INLINE_NODEBUG uint32_t regId() const noexcept { return (_data & kRegIdMask) >> kRegIdShift; }
  //! Sets a physical id of the register used to pass function argument or return value.
  ASMJIT_INLINE_NODEBUG void setRegId(uint32_t regId) noexcept { _replaceValue(kRegIdMask, regId << kRegIdShift); }

  //! Returns a stack offset of this argument.
  ASMJIT_INLINE_NODEBUG int32_t stackOffset() const noexcept { return int32_t(_data & kStackOffsetMask) >> kStackOffsetShift; }
  //! Sets a stack offset of this argument.
  ASMJIT_INLINE_NODEBUG void setStackOffset(int32_t offset) noexcept { _replaceValue(kStackOffsetMask, uint32_t(offset) << kStackOffsetShift); }

  //! Tests whether the argument or return value has associated `TypeId`.
  ASMJIT_INLINE_NODEBUG bool hasTypeId() const noexcept { return Support::test(_data, kTypeIdMask); }
  //! Returns a TypeId of this argument or return value.
  ASMJIT_INLINE_NODEBUG TypeId typeId() const noexcept { return TypeId((_data & kTypeIdMask) >> kTypeIdShift); }
  //! Sets a TypeId of this argument or return value.
  ASMJIT_INLINE_NODEBUG void setTypeId(TypeId typeId) noexcept { _replaceValue(kTypeIdMask, uint32_t(typeId) << kTypeIdShift); }

  //! \}
};

//! Contains multiple `FuncValue` instances in an array so functions that use multiple registers for arguments or
//! return values can represent all inputs and outputs.
struct FuncValuePack {
public:
  //! \name Members
  //! \{

  //! Values of the pack.
  FuncValue _values[Globals::kMaxValuePack];

  //! \}

  //! \name Initialization & Reset
  //! \{

  //! Resets all values in the pack.
  inline void reset() noexcept {
    for (size_t i = 0; i < Globals::kMaxValuePack; i++)
      _values[i].reset();
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Calculates how many values are in the pack, checking for non-values from the end.
  inline uint32_t count() const noexcept {
    uint32_t n = Globals::kMaxValuePack;
    while (n && !_values[n - 1])
      n--;
    return n;
  }

  //! Returns values in this value in the pack.
  //!
  //! \note The returned array has exactly \ref Globals::kMaxValuePack elements.
  ASMJIT_INLINE_NODEBUG FuncValue* values() noexcept { return _values; }
  //! \overload
  ASMJIT_INLINE_NODEBUG const FuncValue* values() const noexcept { return _values; }

  //! Resets a value at the given `index` in the pack, which makes it unassigned.
  inline void resetValue(size_t index) noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    _values[index].reset();
  }

  //! Tests whether the value at the given `index` in the pack is assigned.
  inline bool hasValue(size_t index) noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    return _values[index].isInitialized();
  }

  //! Assigns a register at the given `index` to `reg` and an optional `typeId`.
  inline void assignReg(size_t index, const BaseReg& reg, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    ASMJIT_ASSERT(reg.isPhysReg());
    _values[index].initReg(reg.type(), reg.id(), typeId);
  }

  //! Assigns a register at the given `index` to `regType`, `regId`, and an optional `typeId`.
  inline void assignReg(size_t index, RegType regType, uint32_t regId, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    _values[index].initReg(regType, regId, typeId);
  }

  //! Assigns a stack location at the given `index` to `offset` and an optional `typeId`.
  inline void assignStack(size_t index, int32_t offset, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    _values[index].initStack(offset, typeId);
  }

  //! Accesses the value in the pack at the given `index`.
  //!
  //! \note The maximum index value is `Globals::kMaxValuePack - 1`.
  inline FuncValue& operator[](size_t index) {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    return _values[index];
  }
  //! \overload
  inline const FuncValue& operator[](size_t index) const {
    ASMJIT_ASSERT(index < Globals::kMaxValuePack);
    return _values[index];
  }

  //! \}
};

//! Attributes are designed in a way that all are initially false, and user or \ref FuncFrame finalizer adds
//! them when necessary.
enum class FuncAttributes : uint32_t {
  //! No attributes.
  kNoAttributes = 0,

  //! Function has variable number of arguments.
  kHasVarArgs = 0x00000001u,
  //! Preserve frame pointer (don't omit FP).
  kHasPreservedFP = 0x00000010u,
  //! Function calls other functions (is not leaf).
  kHasFuncCalls = 0x00000020u,
  //! Function has aligned save/restore of vector registers.
  kAlignedVecSR = 0x00000040u,
  //! Function must begin with an instruction that marks a start of a branch or function.
  //!
  //!   * `ENDBR32/ENDBR64` instruction is inserted at the beginning of the function (X86, X86_64).
  //!   * `BTI` instruction is inserted at the beginning of the function (AArch64)
  kIndirectBranchProtection = 0x00000080u,
  //! FuncFrame is finalized and can be used by prolog/epilog inserter (PEI).
  kIsFinalized = 0x00000800u,

  // X86 Specific Attributes
  // -----------------------

  //! Enables the use of AVX within the function's body, prolog, and epilog (X86).
  //!
  //! This flag instructs prolog and epilog emitter to use AVX instead of SSE for manipulating XMM registers.
  kX86_AVXEnabled = 0x00010000u,

  //! Enables the use of AVX-512 within the function's body, prolog, and epilog (X86).
  //!
  //! This flag instructs Compiler register allocator to use additional 16 registers introduced by AVX-512.
  //! Additionally, if the functions saves full width of ZMM registers (custom calling conventions only) then
  //! the prolog/epilog inserter would use AVX-512 move instructions to emit the save and restore sequence.
  kX86_AVX512Enabled = 0x00020000u,

  //! This flag instructs the epilog writer to emit EMMS instruction before RET (X86).
  kX86_MMXCleanup = 0x00040000u,

  //! This flag instructs the epilog writer to emit VZEROUPPER instruction before RET (X86).
  kX86_AVXCleanup = 0x00080000u
};
ASMJIT_DEFINE_ENUM_FLAGS(FuncAttributes)

//! Function detail - \ref CallConv and expanded \ref FuncSignature.
//!
//! Function detail is architecture and OS dependent representation of a function. It contains a materialized
//! calling convention and expanded function signature so all arguments have assigned either register type/id
//! or stack address.
class FuncDetail {
public:
  //! \name Constants
  //! \{

  //! Function doesn't have a variable number of arguments (`...`).
  static constexpr uint8_t kNoVarArgs = 0xFFu;

  //! \}

  //! \name Members
  //! \{

  //! Calling convention.
  CallConv _callConv {};
  //! Number of function arguments.
  uint8_t _argCount = 0;
  //! Variable arguments index of `kNoVarArgs`.
  uint8_t _vaIndex = 0;
  //! Reserved for future use.
  uint16_t _reserved = 0;
  //! Registers that contain arguments.
  Support::Array<RegMask, Globals::kNumVirtGroups> _usedRegs {};
  //! Size of arguments passed by stack.
  uint32_t _argStackSize = 0;
  //! Function return value(s).
  FuncValuePack _rets {};
  //! Function arguments.
  FuncValuePack _args[Globals::kMaxFuncArgs] {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a default constructed \ref FuncDetail.
  ASMJIT_INLINE_NODEBUG FuncDetail() noexcept {}

  //! Copy constructor.
  //!
  //! Function details are copyable.
  ASMJIT_INLINE_NODEBUG FuncDetail(const FuncDetail& other) noexcept = default;

  //! Initializes this `FuncDetail` to the given signature.
  ASMJIT_API Error init(const FuncSignature& signature, const Environment& environment) noexcept;

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Assignment operator, copies `other` to this \ref FuncDetail.
  ASMJIT_INLINE_NODEBUG FuncDetail& operator=(const FuncDetail& other) noexcept = default;

  //! \}

  //! \name Reset
  //! \{

  //! Resets the function detail to its default constructed state.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { *this = FuncDetail{}; }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the function's calling convention, see `CallConv`.
  ASMJIT_INLINE_NODEBUG const CallConv& callConv() const noexcept { return _callConv; }

  //! Returns the associated calling convention flags, see `CallConv::Flags`.
  ASMJIT_INLINE_NODEBUG CallConvFlags flags() const noexcept { return _callConv.flags(); }
  //! Checks whether a CallConv `flag` is set, see `CallConv::Flags`.
  ASMJIT_INLINE_NODEBUG bool hasFlag(CallConvFlags ccFlag) const noexcept { return _callConv.hasFlag(ccFlag); }

  //! Tests whether the function has a return value.
  ASMJIT_INLINE_NODEBUG bool hasRet() const noexcept { return bool(_rets[0]); }
  //! Returns the number of function arguments.
  ASMJIT_INLINE_NODEBUG uint32_t argCount() const noexcept { return _argCount; }

  //! Returns function return values.
  ASMJIT_INLINE_NODEBUG FuncValuePack& retPack() noexcept { return _rets; }
  //! Returns function return values.
  ASMJIT_INLINE_NODEBUG const FuncValuePack& retPack() const noexcept { return _rets; }

  //! Returns a function return value associated with the given `valueIndex`.
  ASMJIT_INLINE_NODEBUG FuncValue& ret(size_t valueIndex = 0) noexcept { return _rets[valueIndex]; }
  //! Returns a function return value associated with the given `valueIndex` (const).
  ASMJIT_INLINE_NODEBUG const FuncValue& ret(size_t valueIndex = 0) const noexcept { return _rets[valueIndex]; }

  //! Returns function argument packs array.
  ASMJIT_INLINE_NODEBUG FuncValuePack* argPacks() noexcept { return _args; }
  //! Returns function argument packs array (const).
  ASMJIT_INLINE_NODEBUG const FuncValuePack* argPacks() const noexcept { return _args; }

  //! Returns function argument pack at the given `argIndex`.
  inline FuncValuePack& argPack(size_t argIndex) noexcept {
    ASMJIT_ASSERT(argIndex < Globals::kMaxFuncArgs);
    return _args[argIndex];
  }

  //! Returns function argument pack at the given `argIndex` (const).
  inline const FuncValuePack& argPack(size_t argIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < Globals::kMaxFuncArgs);
    return _args[argIndex];
  }

  //! Returns an argument at `valueIndex` from the argument pack at the given `argIndex`.
  inline FuncValue& arg(size_t argIndex, size_t valueIndex = 0) noexcept {
    ASMJIT_ASSERT(argIndex < Globals::kMaxFuncArgs);
    return _args[argIndex][valueIndex];
  }

  //! Returns an argument at `valueIndex` from the argument pack at the given `argIndex` (const).
  inline const FuncValue& arg(size_t argIndex, size_t valueIndex = 0) const noexcept {
    ASMJIT_ASSERT(argIndex < Globals::kMaxFuncArgs);
    return _args[argIndex][valueIndex];
  }

  //! Resets an argument at the given `argIndex`.
  //!
  //! If the argument is a parameter pack (has multiple values) all values are reset.
  inline void resetArg(size_t argIndex) noexcept {
    ASMJIT_ASSERT(argIndex < Globals::kMaxFuncArgs);
    _args[argIndex].reset();
  }

  //! Tests whether the function has variable arguments.
  ASMJIT_INLINE_NODEBUG bool hasVarArgs() const noexcept { return _vaIndex != kNoVarArgs; }
  //! Returns an index of a first variable argument.
  ASMJIT_INLINE_NODEBUG uint32_t vaIndex() const noexcept { return _vaIndex; }

  //! Tests whether the function passes one or more argument by stack.
  ASMJIT_INLINE_NODEBUG bool hasStackArgs() const noexcept { return _argStackSize != 0; }
  //! Returns stack size needed for function arguments passed on the stack.
  ASMJIT_INLINE_NODEBUG uint32_t argStackSize() const noexcept { return _argStackSize; }

  //! Returns red zone size.
  ASMJIT_INLINE_NODEBUG uint32_t redZoneSize() const noexcept { return _callConv.redZoneSize(); }
  //! Returns spill zone size.
  ASMJIT_INLINE_NODEBUG uint32_t spillZoneSize() const noexcept { return _callConv.spillZoneSize(); }
  //! Returns natural stack alignment.
  ASMJIT_INLINE_NODEBUG uint32_t naturalStackAlignment() const noexcept { return _callConv.naturalStackAlignment(); }

  //! Returns a mask of all passed registers of the given register `group`.
  ASMJIT_INLINE_NODEBUG RegMask passedRegs(RegGroup group) const noexcept { return _callConv.passedRegs(group); }
  //! Returns a mask of all preserved registers of the given register `group`.
  ASMJIT_INLINE_NODEBUG RegMask preservedRegs(RegGroup group) const noexcept { return _callConv.preservedRegs(group); }

  //! Returns a mask of all used registers of the given register `group`.
  inline RegMask usedRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _usedRegs[size_t(group)];
  }

  //! Adds `regs` to the mask of used registers of the given register `group`.
  inline void addUsedRegs(RegGroup group, RegMask regs) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    _usedRegs[size_t(group)] |= regs;
  }

  //! \}
};

//! Function frame.
//!
//! Function frame is used directly by prolog and epilog insertion (PEI) utils. It provides information necessary to
//! insert a proper and ABI conforming prolog and epilog. Function frame calculation is based on `CallConv` and
//! other function attributes.
//!
//! SSE vs AVX vs AVX-512
//! ---------------------
//!
//! Function frame provides a way to tell prolog/epilog inserter to use AVX instructions instead of SSE. Use
//! `setAvxEnabled()` and `setAvx512Enabled()`  to enable AVX and/or AVX-512, respectively. Enabling AVX-512
//! is mostly for Compiler as it would use 32 SIMD registers instead of 16 when enabled.
//!
//! \note If your code uses AVX instructions and AVX is not enabled there would be a performance hit in case that
//! some registers had to be saved/restored in function's prolog/epilog, respectively. Thus, it's recommended to
//! always let the function frame know about the use of AVX.
//!
//! Function Frame Structure
//! ------------------------
//!
//! Various properties can contribute to the size and structure of the function frame. The function frame in most
//! cases won't use all of the properties illustrated (for example Spill Zone and Red Zone are never used together).
//!
//! ```
//!   +-----------------------------+
//!   | Arguments Passed by Stack   |
//!   +-----------------------------+
//!   | Spill Zone                  |
//!   +-----------------------------+ <- Stack offset (args) starts from here.
//!   | Return Address, if Pushed   |
//!   +-----------------------------+ <- Stack pointer (SP) upon entry.
//!   | Save/Restore Stack.         |
//!   +-----------------------------+-----------------------------+
//!   | Local Stack                 |                             |
//!   +-----------------------------+          Final Stack        |
//!   | Call Stack                  |                             |
//!   +-----------------------------+-----------------------------+ <- SP after prolog.
//!   | Red Zone                    |
//!   +-----------------------------+
//! ```
class FuncFrame {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    //! Tag used to inform that some offset is invalid.
    kTagInvalidOffset = 0xFFFFFFFFu
  };

  //! \}

  //! \name Members
  //! \{

  //! Function attributes.
  FuncAttributes _attributes {};

  //! Target architecture.
  Arch _arch {};
  //! SP register ID (to access call stack and local stack).
  uint8_t _spRegId = uint8_t(BaseReg::kIdBad);
  //! SA register ID (to access stack arguments).
  uint8_t _saRegId = uint8_t(BaseReg::kIdBad);

  //! Red zone size (copied from CallConv).
  uint8_t _redZoneSize = 0;
  //! Spill zone size (copied from CallConv).
  uint8_t _spillZoneSize = 0;
  //! Natural stack alignment (copied from CallConv).
  uint8_t _naturalStackAlignment = 0;
  //! Minimum stack alignment to turn on dynamic alignment.
  uint8_t _minDynamicAlignment = 0;

  //! Call stack alignment.
  uint8_t _callStackAlignment = 0;
  //! Local stack alignment.
  uint8_t _localStackAlignment = 0;
  //! Final stack alignment.
  uint8_t _finalStackAlignment = 0;

  //! Adjustment of the stack before returning (X86-STDCALL).
  uint16_t _calleeStackCleanup = 0;

  //! Call stack size.
  uint32_t _callStackSize = 0;
  //! Local stack size.
  uint32_t _localStackSize = 0;
  //! Final stack size (sum of call stack and local stack).
  uint32_t _finalStackSize = 0;

  //! Local stack offset (non-zero only if call stack is used).
  uint32_t _localStackOffset = 0;
  //! Offset relative to SP that contains previous SP (before alignment).
  uint32_t _daOffset = 0;
  //! Offset of the first stack argument relative to SP.
  uint32_t _saOffsetFromSP = 0;
  //! Offset of the first stack argument relative to SA (_saRegId or FP).
  uint32_t _saOffsetFromSA = 0;

  //! Local stack adjustment in prolog/epilog.
  uint32_t _stackAdjustment = 0;

  //! Registers that are dirty.
  Support::Array<RegMask, Globals::kNumVirtGroups> _dirtyRegs {};
  //! Registers that must be preserved (copied from CallConv).
  Support::Array<RegMask, Globals::kNumVirtGroups> _preservedRegs {};
  //! Size to save/restore per register group.
  Support::Array<uint8_t, Globals::kNumVirtGroups> _saveRestoreRegSize {};
  //! Alignment of save/restore area per register group.
  Support::Array<uint8_t, Globals::kNumVirtGroups> _saveRestoreAlignment {};

  //! Stack size required to save registers with push/pop.
  uint16_t _pushPopSaveSize = 0;
  //! Stack size required to save extra registers that cannot use push/pop.
  uint16_t _extraRegSaveSize = 0;
  //! Offset where registers saved/restored via push/pop are stored
  uint32_t _pushPopSaveOffset = 0;
  //! Offset where extra registers that cannot use push/pop are stored.
  uint32_t _extraRegSaveOffset = 0;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a default constructed function frame, which has initialized all members to their default values.
  ASMJIT_INLINE_NODEBUG FuncFrame() noexcept = default;
  //! Creates a copy of `other` function frame.
  ASMJIT_INLINE_NODEBUG FuncFrame(const FuncFrame& other) noexcept = default;

  //! \}

  //! \name Initialization & Reset
  //! \{

  //! Initializes the function frame based on `func` detail.
  ASMJIT_API Error init(const FuncDetail& func) noexcept;
  //! Resets the function frame into its default constructed state.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { *this = FuncFrame{}; }

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Copy assignment - function frame is copy assignable.
  ASMJIT_INLINE_NODEBUG FuncFrame& operator=(const FuncFrame& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the target architecture of the function frame.
  ASMJIT_INLINE_NODEBUG Arch arch() const noexcept { return _arch; }

  //! Returns function frame attributes, see `Attributes`.
  ASMJIT_INLINE_NODEBUG FuncAttributes attributes() const noexcept { return _attributes; }
  //! Checks whether the FuncFame contains an attribute `attr`.
  ASMJIT_INLINE_NODEBUG bool hasAttribute(FuncAttributes attr) const noexcept { return Support::test(_attributes, attr); }
  //! Adds attributes `attrs` to the FuncFrame.
  ASMJIT_INLINE_NODEBUG void addAttributes(FuncAttributes attrs) noexcept { _attributes |= attrs; }
  //! Clears attributes `attrs` from the FrameFrame.
  ASMJIT_INLINE_NODEBUG void clearAttributes(FuncAttributes attrs) noexcept { _attributes &= ~attrs; }

  //! Tests whether the function has variable number of arguments.
  ASMJIT_INLINE_NODEBUG bool hasVarArgs() const noexcept { return hasAttribute(FuncAttributes::kHasVarArgs); }
  //! Sets the variable arguments flag.
  ASMJIT_INLINE_NODEBUG void setVarArgs() noexcept { addAttributes(FuncAttributes::kHasVarArgs); }
  //! Resets variable arguments flag.
  ASMJIT_INLINE_NODEBUG void resetVarArgs() noexcept { clearAttributes(FuncAttributes::kHasVarArgs); }

  //! Tests whether the function preserves frame pointer (EBP|ESP on X86).
  ASMJIT_INLINE_NODEBUG bool hasPreservedFP() const noexcept { return hasAttribute(FuncAttributes::kHasPreservedFP); }
  //! Enables preserved frame pointer.
  ASMJIT_INLINE_NODEBUG void setPreservedFP() noexcept { addAttributes(FuncAttributes::kHasPreservedFP); }
  //! Disables preserved frame pointer.
  ASMJIT_INLINE_NODEBUG void resetPreservedFP() noexcept { clearAttributes(FuncAttributes::kHasPreservedFP); }

  //! Tests whether the function calls other functions.
  ASMJIT_INLINE_NODEBUG bool hasFuncCalls() const noexcept { return hasAttribute(FuncAttributes::kHasFuncCalls); }
  //! Sets `FuncAttributes::kHasFuncCalls` to true.
  ASMJIT_INLINE_NODEBUG void setFuncCalls() noexcept { addAttributes(FuncAttributes::kHasFuncCalls); }
  //! Sets `FuncAttributes::kHasFuncCalls` to false.
  ASMJIT_INLINE_NODEBUG void resetFuncCalls() noexcept { clearAttributes(FuncAttributes::kHasFuncCalls); }

  //! Tests whether the function uses indirect branch protection, see \ref FuncAttributes::kIndirectBranchProtection.
  ASMJIT_INLINE_NODEBUG bool hasIndirectBranchProtection() const noexcept { return hasAttribute(FuncAttributes::kIndirectBranchProtection); }
  //! Enabled indirect branch protection (sets `FuncAttributes::kIndirectBranchProtection` attribute to true).
  ASMJIT_INLINE_NODEBUG void setIndirectBranchProtection() noexcept { addAttributes(FuncAttributes::kIndirectBranchProtection); }
  //! Disables indirect branch protection (sets `FuncAttributes::kIndirectBranchProtection` attribute to false).
  ASMJIT_INLINE_NODEBUG void resetIndirectBranchProtection() noexcept { clearAttributes(FuncAttributes::kIndirectBranchProtection); }

  //! Tests whether the function has AVX enabled.
  ASMJIT_INLINE_NODEBUG bool isAvxEnabled() const noexcept { return hasAttribute(FuncAttributes::kX86_AVXEnabled); }
  //! Enables AVX use.
  ASMJIT_INLINE_NODEBUG void setAvxEnabled() noexcept { addAttributes(FuncAttributes::kX86_AVXEnabled); }
  //! Disables AVX use.
  ASMJIT_INLINE_NODEBUG void resetAvxEnabled() noexcept { clearAttributes(FuncAttributes::kX86_AVXEnabled); }

  //! Tests whether the function has AVX-512 enabled.
  ASMJIT_INLINE_NODEBUG bool isAvx512Enabled() const noexcept { return hasAttribute(FuncAttributes::kX86_AVX512Enabled); }
  //! Enables AVX-512 use.
  ASMJIT_INLINE_NODEBUG void setAvx512Enabled() noexcept { addAttributes(FuncAttributes::kX86_AVX512Enabled); }
  //! Disables AVX-512 use.
  ASMJIT_INLINE_NODEBUG void resetAvx512Enabled() noexcept { clearAttributes(FuncAttributes::kX86_AVX512Enabled); }

  //! Tests whether the function has MMX cleanup - 'emms' instruction in epilog.
  ASMJIT_INLINE_NODEBUG bool hasMmxCleanup() const noexcept { return hasAttribute(FuncAttributes::kX86_MMXCleanup); }
  //! Enables MMX cleanup.
  ASMJIT_INLINE_NODEBUG void setMmxCleanup() noexcept { addAttributes(FuncAttributes::kX86_MMXCleanup); }
  //! Disables MMX cleanup.
  ASMJIT_INLINE_NODEBUG void resetMmxCleanup() noexcept { clearAttributes(FuncAttributes::kX86_MMXCleanup); }

  //! Tests whether the function has AVX cleanup - 'vzeroupper' instruction in epilog.
  ASMJIT_INLINE_NODEBUG bool hasAvxCleanup() const noexcept { return hasAttribute(FuncAttributes::kX86_AVXCleanup); }
  //! Enables AVX cleanup.
  ASMJIT_INLINE_NODEBUG void setAvxCleanup() noexcept { addAttributes(FuncAttributes::kX86_AVXCleanup); }
  //! Disables AVX cleanup.
  ASMJIT_INLINE_NODEBUG void resetAvxCleanup() noexcept { clearAttributes(FuncAttributes::kX86_AVXCleanup); }

  //! Tests whether the function uses call stack.
  ASMJIT_INLINE_NODEBUG bool hasCallStack() const noexcept { return _callStackSize != 0; }
  //! Tests whether the function uses local stack.
  ASMJIT_INLINE_NODEBUG bool hasLocalStack() const noexcept { return _localStackSize != 0; }
  //! Tests whether vector registers can be saved and restored by using aligned reads and writes.
  ASMJIT_INLINE_NODEBUG bool hasAlignedVecSR() const noexcept { return hasAttribute(FuncAttributes::kAlignedVecSR); }
  //! Tests whether the function has to align stack dynamically.
  ASMJIT_INLINE_NODEBUG bool hasDynamicAlignment() const noexcept { return _finalStackAlignment >= _minDynamicAlignment; }

  //! Tests whether the calling convention specifies 'RedZone'.
  ASMJIT_INLINE_NODEBUG bool hasRedZone() const noexcept { return _redZoneSize != 0; }
  //! Tests whether the calling convention specifies 'SpillZone'.
  ASMJIT_INLINE_NODEBUG bool hasSpillZone() const noexcept { return _spillZoneSize != 0; }

  //! Returns the size of 'RedZone'.
  ASMJIT_INLINE_NODEBUG uint32_t redZoneSize() const noexcept { return _redZoneSize; }
  //! Returns the size of 'SpillZone'.
  ASMJIT_INLINE_NODEBUG uint32_t spillZoneSize() const noexcept { return _spillZoneSize; }

  //! Resets the size of red zone, which would disable it entirely.
  //!
  //! \note Red zone is currently only used by an AMD64 SystemV calling convention, which expects 128
  //! bytes of stack to be accessible below stack pointer. These bytes are then accessible within the
  //! function and Compiler can use this space as a spill area. However, sometimes it's better to
  //! disallow the use of red zone in case that a user wants to use this stack for a custom purpose.
  ASMJIT_INLINE_NODEBUG void resetRedZone() noexcept { _redZoneSize = 0; }

  //! Returns natural stack alignment (guaranteed stack alignment upon entry).
  ASMJIT_INLINE_NODEBUG uint32_t naturalStackAlignment() const noexcept { return _naturalStackAlignment; }
  //! Returns natural stack alignment (guaranteed stack alignment upon entry).
  ASMJIT_INLINE_NODEBUG uint32_t minDynamicAlignment() const noexcept { return _minDynamicAlignment; }

  //! Tests whether the callee must adjust SP before returning (X86-STDCALL only)
  ASMJIT_INLINE_NODEBUG bool hasCalleeStackCleanup() const noexcept { return _calleeStackCleanup != 0; }
  //! Returns home many bytes of the stack the callee must adjust before returning (X86-STDCALL only)
  ASMJIT_INLINE_NODEBUG uint32_t calleeStackCleanup() const noexcept { return _calleeStackCleanup; }

  //! Returns call stack alignment.
  ASMJIT_INLINE_NODEBUG uint32_t callStackAlignment() const noexcept { return _callStackAlignment; }
  //! Returns local stack alignment.
  ASMJIT_INLINE_NODEBUG uint32_t localStackAlignment() const noexcept { return _localStackAlignment; }
  //! Returns final stack alignment (the maximum value of call, local, and natural stack alignments).
  ASMJIT_INLINE_NODEBUG uint32_t finalStackAlignment() const noexcept { return _finalStackAlignment; }

  //! Sets call stack alignment.
  //!
  //! \note This also updates the final stack alignment.
  inline void setCallStackAlignment(uint32_t alignment) noexcept {
    _callStackAlignment = uint8_t(alignment);
    _finalStackAlignment = Support::max(_naturalStackAlignment, _callStackAlignment, _localStackAlignment);
  }

  //! Sets local stack alignment.
  //!
  //! \note This also updates the final stack alignment.
  inline void setLocalStackAlignment(uint32_t value) noexcept {
    _localStackAlignment = uint8_t(value);
    _finalStackAlignment = Support::max(_naturalStackAlignment, _callStackAlignment, _localStackAlignment);
  }

  //! Combines call stack alignment with `alignment`, updating it to the greater value.
  //!
  //! \note This also updates the final stack alignment.
  inline void updateCallStackAlignment(uint32_t alignment) noexcept {
    _callStackAlignment = uint8_t(Support::max<uint32_t>(_callStackAlignment, alignment));
    _finalStackAlignment = Support::max(_finalStackAlignment, _callStackAlignment);
  }

  //! Combines local stack alignment with `alignment`, updating it to the greater value.
  //!
  //! \note This also updates the final stack alignment.
  inline void updateLocalStackAlignment(uint32_t alignment) noexcept {
    _localStackAlignment = uint8_t(Support::max<uint32_t>(_localStackAlignment, alignment));
    _finalStackAlignment = Support::max(_finalStackAlignment, _localStackAlignment);
  }

  //! Returns call stack size.
  ASMJIT_INLINE_NODEBUG uint32_t callStackSize() const noexcept { return _callStackSize; }
  //! Returns local stack size.
  ASMJIT_INLINE_NODEBUG uint32_t localStackSize() const noexcept { return _localStackSize; }

  //! Sets call stack size.
  ASMJIT_INLINE_NODEBUG void setCallStackSize(uint32_t size) noexcept { _callStackSize = size; }
  //! Sets local stack size.
  ASMJIT_INLINE_NODEBUG void setLocalStackSize(uint32_t size) noexcept { _localStackSize = size; }

  //! Combines call stack size with `size`, updating it to the greater value.
  ASMJIT_INLINE_NODEBUG void updateCallStackSize(uint32_t size) noexcept { _callStackSize = Support::max(_callStackSize, size); }
  //! Combines local stack size with `size`, updating it to the greater value.
  ASMJIT_INLINE_NODEBUG void updateLocalStackSize(uint32_t size) noexcept { _localStackSize = Support::max(_localStackSize, size); }

  //! Returns final stack size (only valid after the FuncFrame is finalized).
  ASMJIT_INLINE_NODEBUG uint32_t finalStackSize() const noexcept { return _finalStackSize; }

  //! Returns an offset to access the local stack (non-zero only if call stack is used).
  ASMJIT_INLINE_NODEBUG uint32_t localStackOffset() const noexcept { return _localStackOffset; }

  //! Tests whether the function prolog/epilog requires a memory slot for storing unaligned SP.
  ASMJIT_INLINE_NODEBUG bool hasDAOffset() const noexcept { return _daOffset != kTagInvalidOffset; }
  //! Returns a memory offset used to store DA (dynamic alignment) slot (relative to SP).
  ASMJIT_INLINE_NODEBUG uint32_t daOffset() const noexcept { return _daOffset; }

  ASMJIT_INLINE_NODEBUG uint32_t saOffset(uint32_t regId) const noexcept {
    return regId == _spRegId ? saOffsetFromSP()
                             : saOffsetFromSA();
  }

  ASMJIT_INLINE_NODEBUG uint32_t saOffsetFromSP() const noexcept { return _saOffsetFromSP; }
  ASMJIT_INLINE_NODEBUG uint32_t saOffsetFromSA() const noexcept { return _saOffsetFromSA; }

  //! Returns mask of registers of the given register `group` that are modified by the function. The engine would
  //! then calculate which registers must be saved & restored by the function by using the data provided by the
  //! calling convention.
  inline RegMask dirtyRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _dirtyRegs[group];
  }

  //! Sets which registers (as a mask) are modified by the function.
  //!
  //! \remarks Please note that this will completely overwrite the existing register mask, use `addDirtyRegs()`
  //! to modify the existing register mask.
  inline void setDirtyRegs(RegGroup group, RegMask regs) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    _dirtyRegs[group] = regs;
  }

  //! Adds which registers (as a mask) are modified by the function.
  inline void addDirtyRegs(RegGroup group, RegMask regs) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    _dirtyRegs[group] |= regs;
  }

  //! \overload
  inline void addDirtyRegs(const BaseReg& reg) noexcept {
    ASMJIT_ASSERT(reg.id() < Globals::kMaxPhysRegs);
    addDirtyRegs(reg.group(), Support::bitMask(reg.id()));
  }

  //! \overload
  template<typename... Args>
  inline void addDirtyRegs(const BaseReg& reg, Args&&... args) noexcept {
    addDirtyRegs(reg);
    addDirtyRegs(std::forward<Args>(args)...);
  }

  //! A helper function to set all registers from all register groups dirty.
  //!
  //! \note This should not be used in general as it's the most pessimistic case. However, it can be used for testing
  //! or in cases in which all registers are considered clobbered.
  ASMJIT_INLINE_NODEBUG void setAllDirty() noexcept {
    for (size_t i = 0; i < ASMJIT_ARRAY_SIZE(_dirtyRegs); i++)
      _dirtyRegs[i] = 0xFFFFFFFFu;
  }

  //! A helper function to set all registers from the given register `group` dirty.
  inline void setAllDirty(RegGroup group) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    _dirtyRegs[group] = 0xFFFFFFFFu;
  }

  //! Returns a calculated mask of registers of the given `group` that will be saved and restored in the function's
  //! prolog and epilog, respectively. The register mask is calculated from both `dirtyRegs` (provided by user) and
  //! `preservedMask` (provided by the calling convention).
  inline RegMask savedRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _dirtyRegs[group] & _preservedRegs[group];
  }

  //! Returns the mask of preserved registers of the given register `group`.
  //!
  //! Preserved registers are those that must survive the function call unmodified. The function can only modify
  //! preserved registers it they are saved and restored in function's prolog and epilog, respectively.
  inline RegMask preservedRegs(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _preservedRegs[group];
  }

  //! Returns the size of a save-restore are for the required register `group`.
  inline uint32_t saveRestoreRegSize(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _saveRestoreRegSize[group];
  }

  inline uint32_t saveRestoreAlignment(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _saveRestoreAlignment[group];
  }

  ASMJIT_INLINE_NODEBUG bool hasSARegId() const noexcept { return _saRegId != BaseReg::kIdBad; }
  ASMJIT_INLINE_NODEBUG uint32_t saRegId() const noexcept { return _saRegId; }
  ASMJIT_INLINE_NODEBUG void setSARegId(uint32_t regId) { _saRegId = uint8_t(regId); }
  ASMJIT_INLINE_NODEBUG void resetSARegId() { setSARegId(BaseReg::kIdBad); }

  //! Returns stack size required to save/restore registers via push/pop.
  ASMJIT_INLINE_NODEBUG uint32_t pushPopSaveSize() const noexcept { return _pushPopSaveSize; }
  //! Returns an offset to the stack where registers are saved via push/pop.
  ASMJIT_INLINE_NODEBUG uint32_t pushPopSaveOffset() const noexcept { return _pushPopSaveOffset; }

  //! Returns stack size required to save/restore extra registers that don't use push/pop/
  //!
  //! \note On X86 this covers all registers except GP registers, on other architectures it can be always
  //! zero (for example AArch64 saves all registers via push/pop like instructions, so this would be zero).
  ASMJIT_INLINE_NODEBUG uint32_t extraRegSaveSize() const noexcept { return _extraRegSaveSize; }
  //! Returns an offset to the stack where extra registers are saved.
  ASMJIT_INLINE_NODEBUG uint32_t extraRegSaveOffset() const noexcept { return _extraRegSaveOffset; }

  //! Tests whether the functions contains stack adjustment.
  ASMJIT_INLINE_NODEBUG bool hasStackAdjustment() const noexcept { return _stackAdjustment != 0; }
  //! Returns function's stack adjustment used in function's prolog and epilog.
  //!
  //! If the returned value is zero it means that the stack is not adjusted. This can mean both that the stack
  //! is not used and/or the stack is only adjusted by instructions that pust/pop registers into/from stack.
  ASMJIT_INLINE_NODEBUG uint32_t stackAdjustment() const noexcept { return _stackAdjustment; }

  //! \}

  //! \name Finalization
  //! \{

  ASMJIT_API Error finalize() noexcept;

  //! \}
};

//! A helper class that can be used to assign a physical register for each function argument. Use with
//! `BaseEmitter::emitArgsAssignment()`.
class FuncArgsAssignment {
public:
  //! \name Members
  //! \{

  //! Function detail.
  const FuncDetail* _funcDetail {};
  //! Register that can be used to access arguments passed by stack.
  uint8_t _saRegId = uint8_t(BaseReg::kIdBad);
  //! Reserved for future use.
  uint8_t _reserved[3] {};
  //! Mapping of each function argument.
  FuncValuePack _argPacks[Globals::kMaxFuncArgs] {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates either a default initialized `FuncArgsAssignment` or to assignment that links to `fd`, if non-null.
  ASMJIT_INLINE_NODEBUG explicit FuncArgsAssignment(const FuncDetail* fd = nullptr) noexcept { reset(fd); }

  //! Copy constructor.
  ASMJIT_INLINE_NODEBUG FuncArgsAssignment(const FuncArgsAssignment& other) noexcept = default;

  //! Resets this `FuncArgsAssignment` to either default constructed state or to assignment that links to `fd`,
  //! if non-null.
  inline void reset(const FuncDetail* fd = nullptr) noexcept {
    _funcDetail = fd;
    _saRegId = uint8_t(BaseReg::kIdBad);
    memset(_reserved, 0, sizeof(_reserved));
    memset(_argPacks, 0, sizeof(_argPacks));
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Copy assignment.
  ASMJIT_INLINE_NODEBUG FuncArgsAssignment& operator=(const FuncArgsAssignment& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the associated \ref FuncDetail of this `FuncArgsAssignment`.
  ASMJIT_INLINE_NODEBUG const FuncDetail* funcDetail() const noexcept { return _funcDetail; }
  //! Associates \ref FuncDetails with this `FuncArgsAssignment`.
  ASMJIT_INLINE_NODEBUG void setFuncDetail(const FuncDetail* fd) noexcept { _funcDetail = fd; }

  ASMJIT_INLINE_NODEBUG bool hasSARegId() const noexcept { return _saRegId != BaseReg::kIdBad; }
  ASMJIT_INLINE_NODEBUG uint32_t saRegId() const noexcept { return _saRegId; }
  ASMJIT_INLINE_NODEBUG void setSARegId(uint32_t regId) { _saRegId = uint8_t(regId); }
  ASMJIT_INLINE_NODEBUG void resetSARegId() { _saRegId = uint8_t(BaseReg::kIdBad); }

  //! Returns assigned argument at `argIndex` and `valueIndex`.
  //!
  //! \note `argIndex` refers to he function argument and `valueIndex` refers to a value pack (in case multiple
  //! values are passed as a single argument).
  inline FuncValue& arg(size_t argIndex, size_t valueIndex) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    return _argPacks[argIndex][valueIndex];
  }
  //! \overload
  inline const FuncValue& arg(size_t argIndex, size_t valueIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    return _argPacks[argIndex][valueIndex];
  }

  //! Tests whether argument at `argIndex` and `valueIndex` has been assigned.
  inline bool isAssigned(size_t argIndex, size_t valueIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    return _argPacks[argIndex][valueIndex].isAssigned();
  }

  //! Assigns register at `argIndex` and value index of 0 to `reg` and an optional `typeId`.
  inline void assignReg(size_t argIndex, const BaseReg& reg, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    ASMJIT_ASSERT(reg.isPhysReg());
    _argPacks[argIndex][0].initReg(reg.type(), reg.id(), typeId);
  }

  //! Assigns register at `argIndex` and value index of 0 to `regType`, `regId`, and an optional `typeId`.
  inline void assignReg(size_t argIndex, RegType regType, uint32_t regId, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    _argPacks[argIndex][0].initReg(regType, regId, typeId);
  }

  //! Assigns stack at `argIndex` and value index of 0 to `offset` and an optional `typeId`.
  inline void assignStack(size_t argIndex, int32_t offset, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    _argPacks[argIndex][0].initStack(offset, typeId);
  }

  //! Assigns register at `argIndex` and `valueIndex` to `reg` and an optional `typeId`.
  inline void assignRegInPack(size_t argIndex, size_t valueIndex, const BaseReg& reg, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    ASMJIT_ASSERT(reg.isPhysReg());
    _argPacks[argIndex][valueIndex].initReg(reg.type(), reg.id(), typeId);
  }

  //! Assigns register at `argIndex` and `valueIndex` to `regType`, `regId`, and an optional `typeId`.
  inline void assignRegInPack(size_t argIndex, size_t valueIndex, RegType regType, uint32_t regId, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    _argPacks[argIndex][valueIndex].initReg(regType, regId, typeId);
  }

  //! Assigns stack at `argIndex` and `valueIndex` to `offset` and an optional `typeId`.
  inline void assignStackInPack(size_t argIndex, size_t valueIndex, int32_t offset, TypeId typeId = TypeId::kVoid) noexcept {
    ASMJIT_ASSERT(argIndex < ASMJIT_ARRAY_SIZE(_argPacks));
    _argPacks[argIndex][valueIndex].initStack(offset, typeId);
  }

  // NOTE: All `assignAll()` methods are shortcuts to assign all arguments at once, however, since registers are
  // passed all at once these initializers don't provide any way to pass TypeId and/or to keep any argument between
  // the arguments passed unassigned.
  inline void _assignAllInternal(size_t argIndex, const BaseReg& reg) noexcept {
    assignReg(argIndex, reg);
  }

  template<typename... Args>
  inline void _assignAllInternal(size_t argIndex, const BaseReg& reg, Args&&... args) noexcept {
    assignReg(argIndex, reg);
    _assignAllInternal(argIndex + 1, std::forward<Args>(args)...);
  }

  //! Assigns all argument at once.
  //!
  //! \note This function can be only used if the arguments don't contain value packs (multiple values per argument).
  template<typename... Args>
  inline void assignAll(Args&&... args) noexcept {
    _assignAllInternal(0, std::forward<Args>(args)...);
  }

  //! \}

  //! \name Utilities
  //! \{

  //! Update `FuncFrame` based on function's arguments assignment.
  //!
  //! \note This function must be called in order to use `BaseEmitter::emitArgsAssignment()`, otherwise the \ref FuncFrame
  //! would not contain the information necessary to assign all arguments into the registers and/or stack specified.
  ASMJIT_API Error updateFuncFrame(FuncFrame& frame) const noexcept;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_FUNC_H_INCLUDED
