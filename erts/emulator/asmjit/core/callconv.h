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

#ifndef ASMJIT_CORE_CALLCONV_H_INCLUDED
#define ASMJIT_CORE_CALLCONV_H_INCLUDED

#include "../core/arch.h"
#include "../core/operand.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_function
//! \{

// ============================================================================
// [asmjit::CallConv]
// ============================================================================

//! Function calling convention.
//!
//! Function calling convention is a scheme that defines how function parameters
//! are passed and how function returns its result. AsmJit defines a variety of
//! architecture and OS specific calling conventions and also provides a compile
//! time detection to make the code-generation easier.
struct CallConv {
  //! Calling convention id, see `Id`.
  uint8_t _id;
  //! Architecture identifier, see \ref Environment::Arch.
  uint8_t _arch;
  //! Register assignment strategy.
  uint8_t _strategy;
  //! Flags.
  uint8_t _flags;

  //! Red zone size (AMD64 == 128 bytes).
  uint8_t _redZoneSize;
  //! Spill zone size (WIN64 == 32 bytes).
  uint8_t _spillZoneSize;
  //! Natural stack alignment as defined by OS/ABI.
  uint8_t _naturalStackAlignment;
  uint8_t _reserved[1];

  //! Mask of all passed registers, per group.
  uint32_t _passedRegs[BaseReg::kGroupVirt];
  //! Mask of all preserved registers, per group.
  uint32_t _preservedRegs[BaseReg::kGroupVirt];

  //! Internal limits of AsmJit's CallConv.
  enum Limits : uint32_t {
    kMaxRegArgsPerGroup  = 16
  };

  //! Passed registers' order.
  union RegOrder {
    //! Passed registers, ordered.
    uint8_t id[kMaxRegArgsPerGroup];
    uint32_t packed[(kMaxRegArgsPerGroup + 3) / 4];
  };

  //! Passed registers' order, per register group.
  RegOrder _passedOrder[BaseReg::kGroupVirt];

  //! Calling convention id.
  //!
  //! Calling conventions can be divided into the following groups:
  //!
  //!   - Universal - calling conventions are applicable to any target. They
  //!     will be converted to a target dependent calling convention at runtime
  //!     by \ref init(). The purpose of these conventions is to make using
  //!     functions less target dependent and closer to how they are declared
  //!     in C and C++.
  //!
  //!   - Target specific - calling conventions that are used by a particular
  //!     architecture and ABI. For example Windows 64-bit calling convention
  //!     and AMD64 SystemV calling convention.
  enum Id : uint32_t {
    //! None or invalid (can't be used).
    kIdNone = 0,

    // ------------------------------------------------------------------------
    // [Universal Calling Conventions]
    // ------------------------------------------------------------------------

    //! Standard function call or explicit `__cdecl` where it can be specified.
    //!
    //! This is a universal convention, which is used to initialize specific
    //! calling connventions based on architecture, platform, and its ABI.
    kIdCDecl = 1,

    //! `__stdcall` on targets that support this calling convention.
    //!
    //! \note This calling convention is only supported on 32-bit X86. If used
    //! on environment that doesn't support this calling convention \ref kIdCDecl
    //! will be used instead.
    kIdStdCall = 2,

    //! `__fastcall` on targets that support this calling convention.
    //!
    //! \note This calling convention is only supported on 32-bit X86. If used
    //! on environment that doesn't support this calling convention \ref kIdCDecl
    //! will be used instead.
    kIdFastCall = 3,

    //! `__vectorcall` on targets that support this calling convention.
    //!
    //! \note This calling convention is only supported on 32-bit and 64-bit
    //! X86 architecture on Windows platform. If used on environment that doesn't
    //! support this calling convention \ref kIdCDecl will be used instead.
    kIdVectorCall = 4,

    //! `__thiscall` on targets that support this calling convention.
    //!
    //! \note This calling convention is only supported on 32-bit X86 Windows
    //! platform. If used on environment that doesn't support this calling
    //! convention \ref kIdCDecl will be used instead.
    kIdThisCall = 5,

    //! `__attribute__((regparm(1)))` convention (GCC and Clang).
    kIdRegParm1 = 6,
    //! `__attribute__((regparm(2)))` convention (GCC and Clang).
    kIdRegParm2 = 7,
    //! `__attribute__((regparm(3)))` convention (GCC and Clang).
    kIdRegParm3 = 8,

    //! Soft-float calling convention (ARM).
    //!
    //! Floating point arguments are passed via general purpose registers.
    kIdSoftFloat = 9,

    //! Hard-float calling convention (ARM).
    //!
    //! Floating point arguments are passed via SIMD registers.
    kIdHardFloat = 10,

    //! AsmJit specific calling convention designed for calling functions
    //! inside a multimedia code that don't use many registers internally,
    //! but are long enough to be called and not inlined. These functions are
    //! usually used to calculate trigonometric functions, logarithms, etc...
    kIdLightCall2 = 16,
    kIdLightCall3 = 17,
    kIdLightCall4 = 18,

    // ------------------------------------------------------------------------
    // [ABI-Specific Calling Conventions]
    // ------------------------------------------------------------------------

    kIdX64SystemV = 32,
    kIdX64Windows = 33,

    // ------------------------------------------------------------------------
    // [Host]
    // ------------------------------------------------------------------------

    kIdHost =
#if ASMJIT_ARCH_ARM == 32 && defined(__SOFTFP__)
      kIdSoftFloat
#elif ASMJIT_ARCH_ARM == 32 && !defined(__SOFTFP__)
      kIdHardFloat
#else
      kIdCDecl
#endif

#ifndef ASMJIT_NO_DEPRECATE
    , kIdHostCDecl = kIdCDecl
    , kIdHostStdCall = kIdStdCall
    , kIdHostFastCall = kIdFastCall
    , kIdHostLightCall2 = kIdLightCall2
    , kIdHostLightCall3 = kIdLightCall3
    , kIdHostLightCall4 = kIdLightCall4
#endif // !ASMJIT_NO_DEPRECATE
  };

  //! Strategy used to assign registers to function arguments.
  //!
  //! This is AsmJit specific. It basically describes how AsmJit should convert
  //! the function arguments defined by `FuncSignature` into register IDs and
  //! stack offsets. The default strategy `kStrategyDefault` assigns registers
  //! and then stack whereas `kStrategyWin64` strategy does register shadowing
  //! as defined by WIN64 calling convention - it applies to 64-bit calling
  //! conventions only.
  enum Strategy : uint32_t {
    //! Default register assignment strategy.
    kStrategyDefault = 0,
    //! Windows 64-bit ABI register assignment strategy.
    kStrategyX64Windows = 1,
    //! Windows 64-bit __vectorcall register assignment strategy.
    kStrategyX64VectorCall = 2,

    //! Number of assignment strategies.
    kStrategyCount = 3
  };

  //! Calling convention flags.
  enum Flags : uint32_t {
    //! Callee is responsible for cleaning up the stack.
    kFlagCalleePopsStack = 0x01u,
    //! Pass vector arguments indirectly (as a pointer).
    kFlagIndirectVecArgs = 0x02u,
    //! Pass F32 and F64 arguments by VEC128 register.
    kFlagPassFloatsByVec = 0x04u,
    //! Pass MMX and vector arguments by stack if the function has variable arguments.
    kFlagPassVecByStackIfVA = 0x08u,
    //! MMX registers are passed and returned via GP registers.
    kFlagPassMmxByGp = 0x10u,
    //! MMX registers are passed and returned via XMM registers.
    kFlagPassMmxByXmm = 0x20u,
    //! Calling convention can be used with variable arguments.
    kFlagVarArgCompatible = 0x80u
  };

  //! \name Construction & Destruction
  //! \{

  //! Initializes this calling convention to the given `ccId` based on the
  //! `environment`.
  //!
  //! See \ref Id and \ref Environment for more details.
  ASMJIT_API Error init(uint32_t ccId, const Environment& environment) noexcept;

  //! Resets this CallConv struct into a defined state.
  //!
  //! It's recommended to reset the \ref CallConv struct in case you would
  //! like create a custom calling convention as it prevents from using an
  //! uninitialized data (CallConv doesn't have a constructor that would
  //! initialize it, it's just a struct).
  inline void reset() noexcept {
    memset(this, 0, sizeof(*this));
    memset(_passedOrder, 0xFF, sizeof(_passedOrder));
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the calling convention id, see `Id`.
  inline uint32_t id() const noexcept { return _id; }
  //! Sets the calling convention id, see `Id`.
  inline void setId(uint32_t id) noexcept { _id = uint8_t(id); }

  //! Returns the calling function architecture id.
  inline uint32_t arch() const noexcept { return _arch; }
  //! Sets the calling function architecture id.
  inline void setArch(uint32_t arch) noexcept { _arch = uint8_t(arch); }

  //! Returns the strategy used to assign registers to arguments, see `Strategy`.
  inline uint32_t strategy() const noexcept { return _strategy; }
  //! Sets the strategy used to assign registers to arguments, see `Strategy`.
  inline void setStrategy(uint32_t strategy) noexcept { _strategy = uint8_t(strategy); }

  //! Tests whether the calling convention has the given `flag` set.
  inline bool hasFlag(uint32_t flag) const noexcept { return (uint32_t(_flags) & flag) != 0; }
  //! Returns the calling convention flags, see `Flags`.
  inline uint32_t flags() const noexcept { return _flags; }
  //! Adds the calling convention flags, see `Flags`.
  inline void setFlags(uint32_t flag) noexcept { _flags = uint8_t(flag); };
  //! Adds the calling convention flags, see `Flags`.
  inline void addFlags(uint32_t flags) noexcept { _flags = uint8_t(_flags | flags); };

  //! Tests whether this calling convention specifies 'RedZone'.
  inline bool hasRedZone() const noexcept { return _redZoneSize != 0; }
  //! Tests whether this calling convention specifies 'SpillZone'.
  inline bool hasSpillZone() const noexcept { return _spillZoneSize != 0; }

  //! Returns size of 'RedZone'.
  inline uint32_t redZoneSize() const noexcept { return _redZoneSize; }
  //! Returns size of 'SpillZone'.
  inline uint32_t spillZoneSize() const noexcept { return _spillZoneSize; }

  //! Sets size of 'RedZone'.
  inline void setRedZoneSize(uint32_t size) noexcept { _redZoneSize = uint8_t(size); }
  //! Sets size of 'SpillZone'.
  inline void setSpillZoneSize(uint32_t size) noexcept { _spillZoneSize = uint8_t(size); }

  //! Returns a natural stack alignment.
  inline uint32_t naturalStackAlignment() const noexcept { return _naturalStackAlignment; }
  //! Sets a natural stack alignment.
  //!
  //! This function can be used to override the default stack alignment in case
  //! that you know that it's alignment is different. For example it allows to
  //! implement custom calling conventions that guarantee higher stack alignment.
  inline void setNaturalStackAlignment(uint32_t value) noexcept { _naturalStackAlignment = uint8_t(value); }

  //! Returns the order of passed registers of the given `group`, see \ref BaseReg::RegGroup.
  inline const uint8_t* passedOrder(uint32_t group) const noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);
    return _passedOrder[group].id;
  }

  //! Returns the mask of passed registers of the given `group`, see \ref BaseReg::RegGroup.
  inline uint32_t passedRegs(uint32_t group) const noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);
    return _passedRegs[group];
  }

  inline void _setPassedPacked(uint32_t group, uint32_t p0, uint32_t p1, uint32_t p2, uint32_t p3) noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);

    _passedOrder[group].packed[0] = p0;
    _passedOrder[group].packed[1] = p1;
    _passedOrder[group].packed[2] = p2;
    _passedOrder[group].packed[3] = p3;
  }

  //! Resets the order and mask of passed registers.
  inline void setPassedToNone(uint32_t group) noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);

    _setPassedPacked(group, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);
    _passedRegs[group] = 0u;
  }

  //! Sets the order and mask of passed registers.
  inline void setPassedOrder(uint32_t group, uint32_t a0, uint32_t a1 = 0xFF, uint32_t a2 = 0xFF, uint32_t a3 = 0xFF, uint32_t a4 = 0xFF, uint32_t a5 = 0xFF, uint32_t a6 = 0xFF, uint32_t a7 = 0xFF) noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);

    // NOTE: This should always be called with all arguments known at compile time,
    // so even if it looks scary it should be translated into few instructions.
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

  //! Returns preserved register mask of the given `group`, see \ref BaseReg::RegGroup.
  inline uint32_t preservedRegs(uint32_t group) const noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);
    return _preservedRegs[group];
  }

  //! Sets preserved register mask of the given `group`, see \ref BaseReg::RegGroup.
  inline void setPreservedRegs(uint32_t group, uint32_t regs) noexcept {
    ASMJIT_ASSERT(group < BaseReg::kGroupVirt);
    _preservedRegs[group] = regs;
  }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_CALLCONV_H_INCLUDED
