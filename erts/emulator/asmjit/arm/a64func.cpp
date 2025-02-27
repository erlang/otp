// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64)

#include "../arm/a64func_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

namespace FuncInternal {

static inline bool shouldTreatAsCDecl(CallConvId ccId) noexcept {
  return ccId == CallConvId::kCDecl ||
         ccId == CallConvId::kStdCall ||
         ccId == CallConvId::kFastCall ||
         ccId == CallConvId::kVectorCall ||
         ccId == CallConvId::kThisCall ||
         ccId == CallConvId::kRegParm1 ||
         ccId == CallConvId::kRegParm2 ||
         ccId == CallConvId::kRegParm3;
}

static RegType regTypeFromFpOrVecTypeId(TypeId typeId) noexcept {
  if (typeId == TypeId::kFloat32)
    return RegType::kARM_VecS;
  else if (typeId == TypeId::kFloat64)
    return RegType::kARM_VecD;
  else if (TypeUtils::isVec32(typeId))
    return RegType::kARM_VecS;
  else if (TypeUtils::isVec64(typeId))
    return RegType::kARM_VecD;
  else if (TypeUtils::isVec128(typeId))
    return RegType::kARM_VecV;
  else
    return RegType::kNone;
}

ASMJIT_FAVOR_SIZE Error initCallConv(CallConv& cc, CallConvId ccId, const Environment& environment) noexcept {
  cc.setArch(environment.arch());
  cc.setStrategy(environment.isDarwin() ? CallConvStrategy::kAArch64Apple : CallConvStrategy::kDefault);

  cc.setSaveRestoreRegSize(RegGroup::kGp, 8);
  cc.setSaveRestoreRegSize(RegGroup::kVec, 8);
  cc.setSaveRestoreAlignment(RegGroup::kGp, 16);
  cc.setSaveRestoreAlignment(RegGroup::kVec, 16);
  cc.setSaveRestoreAlignment(RegGroup::kMask, 1);
  cc.setSaveRestoreAlignment(RegGroup::kExtraVirt3, 1);
  cc.setPassedOrder(RegGroup::kGp, 0, 1, 2, 3, 4, 5, 6, 7);
  cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5, 6, 7);
  cc.setNaturalStackAlignment(16);

  if (shouldTreatAsCDecl(ccId)) {
    // ARM doesn't have that many calling conventions as we can find in X86 world, treat most conventions as __cdecl.
    cc.setId(CallConvId::kCDecl);
    cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(Gp::kIdOs, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30));
    cc.setPreservedRegs(RegGroup::kVec, Support::bitMask(8, 9, 10, 11, 12, 13, 14, 15));
  }
  else {
    cc.setId(ccId);
    cc.setSaveRestoreRegSize(RegGroup::kVec, 16);
    cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30));
    cc.setPreservedRegs(RegGroup::kVec, Support::bitMask(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31));
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error initFuncDetail(FuncDetail& func, const FuncSignature& signature) noexcept {
  DebugUtils::unused(signature);

  const CallConv& cc = func.callConv();
  uint32_t stackOffset = 0;

  uint32_t i;
  uint32_t argCount = func.argCount();

  // Minimum stack size of a single argument passed via stack. The standard AArch64 calling convention
  // specifies 8 bytes, so each function argument would occupy at least 8 bytes even if it needs less.
  // However, Apple has decided to not follow this rule and function argument can occupy less, for
  // example two consecutive 32-bit arguments would occupy 8 bytes total, instead of 16 as specified
  // by ARM.
  uint32_t minStackArgSize = cc.strategy() == CallConvStrategy::kAArch64Apple ? 4u : 8u;

  if (func.hasRet()) {
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      TypeId typeId = func._rets[valueIndex].typeId();

      // Terminate at the first void type (end of the pack).
      if (typeId == TypeId::kVoid)
        break;

      switch (typeId) {
        case TypeId::kInt8:
        case TypeId::kInt16:
        case TypeId::kInt32: {
          func._rets[valueIndex].initReg(RegType::kARM_GpW, valueIndex, TypeId::kInt32);
          break;
        }

        case TypeId::kUInt8:
        case TypeId::kUInt16:
        case TypeId::kUInt32: {
          func._rets[valueIndex].initReg(RegType::kARM_GpW, valueIndex, TypeId::kUInt32);
          break;
        }

        case TypeId::kInt64:
        case TypeId::kUInt64: {
          func._rets[valueIndex].initReg(RegType::kARM_GpX, valueIndex, typeId);
          break;
        }

        default: {
          RegType regType = regTypeFromFpOrVecTypeId(typeId);
          if (regType == RegType::kNone)
            return DebugUtils::errored(kErrorInvalidRegType);

          func._rets[valueIndex].initReg(regType, valueIndex, typeId);
          break;
        }
      }
    }
  }

  switch (cc.strategy()) {
    case CallConvStrategy::kDefault:
    case CallConvStrategy::kAArch64Apple: {
      uint32_t gpzPos = 0;
      uint32_t vecPos = 0;

      for (i = 0; i < argCount; i++) {
        FuncValue& arg = func._args[i][0];
        TypeId typeId = arg.typeId();

        if (TypeUtils::isInt(typeId)) {
          uint32_t regId = BaseReg::kIdBad;

          if (gpzPos < CallConv::kMaxRegArgsPerGroup)
            regId = cc._passedOrder[RegGroup::kGp].id[gpzPos];

          if (regId != BaseReg::kIdBad) {
            RegType regType = typeId <= TypeId::kUInt32 ? RegType::kARM_GpW : RegType::kARM_GpX;
            arg.assignRegData(regType, regId);
            func.addUsedRegs(RegGroup::kGp, Support::bitMask(regId));
            gpzPos++;
          }
          else {
            uint32_t size = Support::max<uint32_t>(TypeUtils::sizeOf(typeId), minStackArgSize);
            if (size >= 8)
              stackOffset = Support::alignUp(stackOffset, 8);
            arg.assignStackOffset(int32_t(stackOffset));
            stackOffset += size;
          }
          continue;
        }

        if (TypeUtils::isFloat(typeId) || TypeUtils::isVec(typeId)) {
          uint32_t regId = BaseReg::kIdBad;

          if (vecPos < CallConv::kMaxRegArgsPerGroup)
            regId = cc._passedOrder[RegGroup::kVec].id[vecPos];

          if (regId != BaseReg::kIdBad) {
            RegType regType = regTypeFromFpOrVecTypeId(typeId);
            if (regType == RegType::kNone)
              return DebugUtils::errored(kErrorInvalidRegType);

            arg.initTypeId(typeId);
            arg.assignRegData(regType, regId);
            func.addUsedRegs(RegGroup::kVec, Support::bitMask(regId));
            vecPos++;
          }
          else {
            uint32_t size = Support::max<uint32_t>(TypeUtils::sizeOf(typeId), minStackArgSize);
            if (size >= 8)
              stackOffset = Support::alignUp(stackOffset, 8);
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

  func._argStackSize = Support::alignUp(stackOffset, 8u);
  return kErrorOk;
}

} // {FuncInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
