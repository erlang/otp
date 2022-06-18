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
#if !defined(ASMJIT_NO_ARM)

#include "../arm/armfunc_p.h"
#include "../arm/armoperand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

// ============================================================================
// [asmjit::arm::FuncInternal]
// ============================================================================

namespace FuncInternal {

static inline bool shouldThreatAsCDecl(uint32_t ccId) noexcept {
  return ccId == CallConv::kIdCDecl ||
         ccId == CallConv::kIdStdCall ||
         ccId == CallConv::kIdFastCall ||
         ccId == CallConv::kIdVectorCall ||
         ccId == CallConv::kIdThisCall ||
         ccId == CallConv::kIdRegParm1 ||
         ccId == CallConv::kIdRegParm2 ||
         ccId == CallConv::kIdRegParm3;
}

static uint32_t regTypeFromFpOrVecTypeId(uint32_t typeId) noexcept {
  if (typeId == Type::kIdF32)
    return Reg::kTypeVecS;
  else if (typeId == Type::kIdF64)
    return Reg::kTypeVecD;
  else if (Type::isVec32(typeId))
    return Reg::kTypeVecS;
  else if (Type::isVec64(typeId))
    return Reg::kTypeVecD;
  else if (Type::isVec128(typeId))
    return Reg::kTypeVecV;
  else
    return 0;
}

ASMJIT_FAVOR_SIZE Error initCallConv(CallConv& cc, uint32_t ccId, const Environment& environment) noexcept {
  cc.setArch(environment.arch());

  if (environment.is32Bit()) {
    // TODO: [ARM] 32-bit ARM not supported yet.
    return DebugUtils::errored(kErrorInvalidState);
  }
  else {
    cc.setSaveRestoreRegSize(Reg::kGroupGp, 8);
    cc.setSaveRestoreRegSize(Reg::kGroupVec, 8);
    cc.setSaveRestoreAlignment(Reg::kGroupGp, 16);
    cc.setSaveRestoreAlignment(Reg::kGroupVec, 16);
    cc.setSaveRestoreAlignment(Reg::kGroupOther0, 1);
    cc.setSaveRestoreAlignment(Reg::kGroupOther1, 1);

    cc.setPassedOrder(Reg::kGroupGp, 0, 1, 2, 3, 4, 5, 6, 7);
    cc.setPassedOrder(Reg::kGroupVec, 0, 1, 2, 3, 4, 5, 6, 7);
    cc.setNaturalStackAlignment(16);

    if (shouldThreatAsCDecl(ccId)) {
      // ARM doesn't have that many calling conventions as we can find in X86 world, treat most conventions as __cdecl.
      cc.setId(CallConv::kIdCDecl);
      cc.setPreservedRegs(Reg::kGroupGp, Support::bitMask(Gp::kIdOs, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30));
      cc.setPreservedRegs(Reg::kGroupVec, Support::bitMask(8, 9, 10, 11, 12, 13, 14, 15));
    }
    else {
      cc.setId(ccId);
      cc.setSaveRestoreRegSize(Reg::kGroupVec, 16);
      cc.setPreservedRegs(Reg::kGroupGp, Support::bitMask(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30));
      cc.setPreservedRegs(Reg::kGroupVec, Support::bitMask(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31));
    }

    return kErrorOk;
  }
}

ASMJIT_FAVOR_SIZE Error initFuncDetail(FuncDetail& func, const FuncSignature& signature, uint32_t registerSize) noexcept {
  const CallConv& cc = func.callConv();
  uint32_t stackOffset = 0;

  uint32_t i;
  uint32_t argCount = func.argCount();

  if (func.hasRet()) {
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      uint32_t typeId = func._rets[valueIndex].typeId();

      // Terminate at the first void type (end of the pack).
      if (!typeId)
        break;

      switch (typeId) {
        case Type::kIdI8:
        case Type::kIdI16:
        case Type::kIdI32: {
          func._rets[valueIndex].initReg(Reg::kTypeGpW, valueIndex, Type::kIdI32);
          break;
        }

        case Type::kIdU8:
        case Type::kIdU16:
        case Type::kIdU32: {
          func._rets[valueIndex].initReg(Reg::kTypeGpW, valueIndex, Type::kIdU32);
          break;
        }

        case Type::kIdI64:
        case Type::kIdU64: {
          func._rets[valueIndex].initReg(Reg::kTypeGpX, valueIndex, typeId);
          break;
        }

        default: {
          uint32_t regType = regTypeFromFpOrVecTypeId(typeId);
          if (!regType)
            return DebugUtils::errored(kErrorInvalidRegType);

          func._rets[valueIndex].initReg(regType, valueIndex, typeId);
          break;
        }
      }
    }
  }

  switch (cc.strategy()) {
    case CallConv::kStrategyDefault: {
      uint32_t gpzPos = 0;
      uint32_t vecPos = 0;

      for (i = 0; i < argCount; i++) {
        FuncValue& arg = func._args[i][0];
        uint32_t typeId = arg.typeId();

        if (Type::isInt(typeId)) {
          uint32_t regId = BaseReg::kIdBad;

          if (gpzPos < CallConv::kMaxRegArgsPerGroup)
            regId = cc._passedOrder[Reg::kGroupGp].id[gpzPos];

          if (regId != BaseReg::kIdBad) {
            uint32_t regType = (typeId <= Type::kIdU32) ? Reg::kTypeGpW : Reg::kTypeGpX;
            arg.assignRegData(regType, regId);
            func.addUsedRegs(Reg::kGroupGp, Support::bitMask(regId));
            gpzPos++;
          }
          else {
#if defined(__APPLE__)
            uint32_t size = Type::sizeOf(typeId);
#else
            uint32_t size = Support::max<uint32_t>(Type::sizeOf(typeId), registerSize);
#endif
            arg.assignStackOffset(int32_t(stackOffset));
            stackOffset += size;
          }
          continue;
        }

        if (Type::isFloat(typeId) || Type::isVec(typeId)) {
          uint32_t regId = BaseReg::kIdBad;

          if (vecPos < CallConv::kMaxRegArgsPerGroup)
            regId = cc._passedOrder[Reg::kGroupVec].id[vecPos];

          if (regId != BaseReg::kIdBad) {
            uint32_t regType = regTypeFromFpOrVecTypeId(typeId);
            if (!regType)
              return DebugUtils::errored(kErrorInvalidRegType);

            arg.initTypeId(typeId);
            arg.assignRegData(regType, regId);
            func.addUsedRegs(Reg::kGroupVec, Support::bitMask(regId));
            vecPos++;
          }
          else {
            uint32_t size = Type::sizeOf(typeId);
            arg.assignStackOffset(int32_t(stackOffset));
            stackOffset += size;
          }
          continue;
        }
      }
      break;
    }

    default:
      return DebugUtils::errored(kErrorInvalidState);
  }

  func._argStackSize = stackOffset;
  return kErrorOk;
}

} // {FuncInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_ARM
