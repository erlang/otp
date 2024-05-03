// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_TARGET_H_INCLUDED
#define ASMJIT_CORE_TARGET_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/cpuinfo.h"
#include "../core/func.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Target is an abstract class that describes a machine code target.
class ASMJIT_VIRTAPI Target {
public:
  ASMJIT_BASE_CLASS(Target)
  ASMJIT_NONCOPYABLE(Target)

  //! Target environment information.
  Environment _environment;
  //! Target CPU features.
  CpuFeatures _cpuFeatures;

  //! \name Construction & Destruction
  //! \{

  //! Creates a `Target` instance.
  ASMJIT_API Target() noexcept;
  //! Destroys the `Target` instance.
  ASMJIT_API virtual ~Target() noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns target's environment.
  ASMJIT_INLINE_NODEBUG const Environment& environment() const noexcept { return _environment; }
  //! Returns the target architecture.
  ASMJIT_INLINE_NODEBUG Arch arch() const noexcept { return _environment.arch(); }
  //! Returns the target sub-architecture.
  ASMJIT_INLINE_NODEBUG SubArch subArch() const noexcept { return _environment.subArch(); }

  //! Returns target CPU features.
  ASMJIT_INLINE_NODEBUG const CpuFeatures& cpuFeatures() const noexcept { return _cpuFeatures; }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_TARGET_H_INCLUDED
