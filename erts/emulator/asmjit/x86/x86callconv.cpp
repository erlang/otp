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
#ifdef ASMJIT_BUILD_X86

#include "../x86/x86callconv_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// ============================================================================
// [asmjit::x86::CallConvInternal - Init]
// ============================================================================

namespace CallConvInternal {

static inline bool shouldThreatAsCDeclIn64BitMode(uint32_t ccId) noexcept {
  return ccId == CallConv::kIdCDecl ||
         ccId == CallConv::kIdStdCall ||
         ccId == CallConv::kIdThisCall ||
         ccId == CallConv::kIdFastCall ||
         ccId == CallConv::kIdRegParm1 ||
         ccId == CallConv::kIdRegParm2 ||
         ccId == CallConv::kIdRegParm3;
}

ASMJIT_FAVOR_SIZE Error init(CallConv& cc, uint32_t ccId, const Environment& environment) noexcept {
  constexpr uint32_t kGroupGp   = Reg::kGroupGp;
  constexpr uint32_t kGroupVec  = Reg::kGroupVec;
  constexpr uint32_t kGroupMm   = Reg::kGroupMm;
  constexpr uint32_t kGroupKReg = Reg::kGroupKReg;

  constexpr uint32_t kZax = Gp::kIdAx;
  constexpr uint32_t kZbx = Gp::kIdBx;
  constexpr uint32_t kZcx = Gp::kIdCx;
  constexpr uint32_t kZdx = Gp::kIdDx;
  constexpr uint32_t kZsp = Gp::kIdSp;
  constexpr uint32_t kZbp = Gp::kIdBp;
  constexpr uint32_t kZsi = Gp::kIdSi;
  constexpr uint32_t kZdi = Gp::kIdDi;

  bool winABI = environment.isPlatformWindows() || environment.isAbiMSVC();

  cc.setArch(environment.arch());

  if (environment.is32Bit()) {
    bool isStandardCallConv = true;

    cc.setPreservedRegs(Reg::kGroupGp, Support::bitMask(Gp::kIdBx, Gp::kIdSp, Gp::kIdBp, Gp::kIdSi, Gp::kIdDi));
    cc.setNaturalStackAlignment(4);

    switch (ccId) {
      case CallConv::kIdCDecl:
        break;

      case CallConv::kIdStdCall:
        cc.setFlags(CallConv::kFlagCalleePopsStack);
        break;

      case CallConv::kIdFastCall:
        cc.setFlags(CallConv::kFlagCalleePopsStack);
        cc.setPassedOrder(kGroupGp, kZcx, kZdx);
        break;

      case CallConv::kIdVectorCall:
        cc.setFlags(CallConv::kFlagCalleePopsStack);
        cc.setPassedOrder(kGroupGp, kZcx, kZdx);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3, 4, 5);
        break;

      case CallConv::kIdThisCall:
        // NOTE: Even MINGW (starting with GCC 4.7.0) now uses __thiscall on MS Windows,
        // so we won't bail to any other calling convention if __thiscall was specified.
        if (winABI) {
          cc.setFlags(CallConv::kFlagCalleePopsStack);
          cc.setPassedOrder(kGroupGp, kZcx);
        }
        else {
          ccId = CallConv::kIdCDecl;
        }
        break;

      case CallConv::kIdRegParm1:
        cc.setPassedOrder(kGroupGp, kZax);
        break;

      case CallConv::kIdRegParm2:
        cc.setPassedOrder(kGroupGp, kZax, kZdx);
        break;

      case CallConv::kIdRegParm3:
        cc.setPassedOrder(kGroupGp, kZax, kZdx, kZcx);
        break;

      case CallConv::kIdLightCall2:
      case CallConv::kIdLightCall3:
      case CallConv::kIdLightCall4: {
        uint32_t n = (ccId - CallConv::kIdLightCall2) + 2;

        cc.setFlags(CallConv::kFlagPassFloatsByVec);
        cc.setPassedOrder(kGroupGp, kZax, kZdx, kZcx, kZsi, kZdi);
        cc.setPassedOrder(kGroupMm, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(kGroupKReg, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPreservedRegs(kGroupGp, Support::lsbMask<uint32_t>(8));
        cc.setPreservedRegs(kGroupVec, Support::lsbMask<uint32_t>(8) & ~Support::lsbMask<uint32_t>(n));

        cc.setNaturalStackAlignment(16);
        isStandardCallConv = false;
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidArgument);
    }

    if (isStandardCallConv) {
      // MMX arguments is something where compiler vendors disagree. For example
      // GCC and MSVC would pass first three via registers and the rest via stack,
      // however Clang passes all via stack. Returning MMX registers is even more
      // fun, where GCC uses MM0, but Clang uses EAX:EDX pair. I'm not sure it's
      // something we should be worried about as MMX is deprecated anyway.
      cc.setPassedOrder(kGroupMm, 0, 1, 2);

      // Vector arguments (XMM|YMM|ZMM) are passed via registers. However, if the
      // function is variadic then they have to be passed via stack.
      cc.setPassedOrder(kGroupVec, 0, 1, 2);

      // Functions with variable arguments always use stack for MM and vector
      // arguments.
      cc.addFlags(CallConv::kFlagPassVecByStackIfVA);
    }

    if (ccId == CallConv::kIdCDecl) {
      cc.addFlags(CallConv::kFlagVarArgCompatible);
    }
  }
  else {
    // Preprocess the calling convention into a common id as many conventions
    // are normally ignored even by C/C++ compilers and treated as `__cdecl`.
    if (shouldThreatAsCDeclIn64BitMode(ccId))
      ccId = winABI ? CallConv::kIdX64Windows : CallConv::kIdX64SystemV;

    switch (ccId) {
      case CallConv::kIdX64SystemV: {
        cc.setFlags(CallConv::kFlagPassFloatsByVec |
                    CallConv::kFlagPassMmxByXmm    |
                    CallConv::kFlagVarArgCompatible);
        cc.setNaturalStackAlignment(16);
        cc.setRedZoneSize(128);
        cc.setPassedOrder(kGroupGp, kZdi, kZsi, kZdx, kZcx, 8, 9);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPreservedRegs(kGroupGp, Support::bitMask(kZbx, kZsp, kZbp, 12, 13, 14, 15));
        break;
      }

      case CallConv::kIdX64Windows: {
        cc.setStrategy(CallConv::kStrategyX64Windows);
        cc.setFlags(CallConv::kFlagPassFloatsByVec |
                    CallConv::kFlagIndirectVecArgs |
                    CallConv::kFlagPassMmxByGp     |
                    CallConv::kFlagVarArgCompatible);
        cc.setNaturalStackAlignment(16);
        // Maximum 4 arguments in registers, each adds 8 bytes to the spill zone.
        cc.setSpillZoneSize(4 * 8);
        cc.setPassedOrder(kGroupGp, kZcx, kZdx, 8, 9);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3);
        cc.setPreservedRegs(kGroupGp, Support::bitMask(kZbx, kZsp, kZbp, kZsi, kZdi, 12, 13, 14, 15));
        cc.setPreservedRegs(kGroupVec, Support::bitMask(6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
        break;
      }

      case CallConv::kIdVectorCall: {
        cc.setStrategy(CallConv::kStrategyX64VectorCall);
        cc.setFlags(CallConv::kFlagPassFloatsByVec |
                    CallConv::kFlagPassMmxByGp     );
        cc.setNaturalStackAlignment(16);
        // Maximum 6 arguments in registers, each adds 8 bytes to the spill zone.
        cc.setSpillZoneSize(6 * 8);
        cc.setPassedOrder(kGroupGp, kZcx, kZdx, 8, 9);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3, 4, 5);
        cc.setPreservedRegs(kGroupGp, Support::bitMask(kZbx, kZsp, kZbp, kZsi, kZdi, 12, 13, 14, 15));
        cc.setPreservedRegs(kGroupVec, Support::bitMask(6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
        break;
      }

      case CallConv::kIdLightCall2:
      case CallConv::kIdLightCall3:
      case CallConv::kIdLightCall4: {
        uint32_t n = (ccId - CallConv::kIdLightCall2) + 2;

        cc.setFlags(CallConv::kFlagPassFloatsByVec);
        cc.setNaturalStackAlignment(16);
        cc.setPassedOrder(kGroupGp, kZax, kZdx, kZcx, kZsi, kZdi);
        cc.setPassedOrder(kGroupMm, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(kGroupVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(kGroupKReg, 0, 1, 2, 3, 4, 5, 6, 7);

        cc.setPreservedRegs(kGroupGp, Support::lsbMask<uint32_t>(16));
        cc.setPreservedRegs(kGroupVec, ~Support::lsbMask<uint32_t>(n));
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidArgument);
    }
  }

  cc.setId(ccId);
  return kErrorOk;
}

} // {CallConvInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_BUILD_X86
