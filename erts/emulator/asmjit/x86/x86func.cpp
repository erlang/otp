// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86)

#include "../x86/x86func_p.h"
#include "../x86/x86emithelper_p.h"
#include "../x86/x86operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

namespace FuncInternal {

static inline bool shouldThreatAsCDeclIn64BitMode(CallConvId ccId) noexcept {
  return ccId == CallConvId::kCDecl ||
         ccId == CallConvId::kStdCall ||
         ccId == CallConvId::kThisCall ||
         ccId == CallConvId::kFastCall ||
         ccId == CallConvId::kRegParm1 ||
         ccId == CallConvId::kRegParm2 ||
         ccId == CallConvId::kRegParm3;
}

ASMJIT_FAVOR_SIZE Error initCallConv(CallConv& cc, CallConvId ccId, const Environment& environment) noexcept {
  constexpr uint32_t kZax = Gp::kIdAx;
  constexpr uint32_t kZbx = Gp::kIdBx;
  constexpr uint32_t kZcx = Gp::kIdCx;
  constexpr uint32_t kZdx = Gp::kIdDx;
  constexpr uint32_t kZsp = Gp::kIdSp;
  constexpr uint32_t kZbp = Gp::kIdBp;
  constexpr uint32_t kZsi = Gp::kIdSi;
  constexpr uint32_t kZdi = Gp::kIdDi;

  bool winABI = environment.isPlatformWindows() || environment.isMSVC();

  cc.setArch(environment.arch());
  cc.setSaveRestoreRegSize(RegGroup::kVec, 16);
  cc.setSaveRestoreRegSize(RegGroup::kX86_MM, 8);
  cc.setSaveRestoreRegSize(RegGroup::kX86_K, 8);
  cc.setSaveRestoreAlignment(RegGroup::kVec, 16);
  cc.setSaveRestoreAlignment(RegGroup::kX86_MM, 8);
  cc.setSaveRestoreAlignment(RegGroup::kX86_K, 8);

  if (environment.is32Bit()) {
    bool isStandardCallConv = true;

    cc.setSaveRestoreRegSize(RegGroup::kGp, 4);
    cc.setSaveRestoreAlignment(RegGroup::kGp, 4);

    cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(Gp::kIdBx, Gp::kIdSp, Gp::kIdBp, Gp::kIdSi, Gp::kIdDi));
    cc.setNaturalStackAlignment(4);

    switch (ccId) {
      case CallConvId::kCDecl:
        break;

      case CallConvId::kStdCall:
        cc.setFlags(CallConvFlags::kCalleePopsStack);
        break;

      case CallConvId::kFastCall:
        cc.setFlags(CallConvFlags::kCalleePopsStack);
        cc.setPassedOrder(RegGroup::kGp, kZcx, kZdx);
        break;

      case CallConvId::kVectorCall:
        cc.setFlags(CallConvFlags::kCalleePopsStack);
        cc.setPassedOrder(RegGroup::kGp, kZcx, kZdx);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5);
        break;

      case CallConvId::kThisCall:
        // NOTE: Even MINGW (starting with GCC 4.7.0) now uses __thiscall on MS Windows, so we won't bail to any
        // other calling convention if __thiscall was specified.
        if (winABI) {
          cc.setFlags(CallConvFlags::kCalleePopsStack);
          cc.setPassedOrder(RegGroup::kGp, kZcx);
        }
        else {
          ccId = CallConvId::kCDecl;
        }
        break;

      case CallConvId::kRegParm1:
        cc.setPassedOrder(RegGroup::kGp, kZax);
        break;

      case CallConvId::kRegParm2:
        cc.setPassedOrder(RegGroup::kGp, kZax, kZdx);
        break;

      case CallConvId::kRegParm3:
        cc.setPassedOrder(RegGroup::kGp, kZax, kZdx, kZcx);
        break;

      case CallConvId::kLightCall2:
      case CallConvId::kLightCall3:
      case CallConvId::kLightCall4: {
        uint32_t n = uint32_t(ccId) - uint32_t(CallConvId::kLightCall2) + 2;

        cc.setFlags(CallConvFlags::kPassFloatsByVec);
        cc.setPassedOrder(RegGroup::kGp, kZax, kZdx, kZcx, kZsi, kZdi);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(RegGroup::kX86_K, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(RegGroup::kX86_MM, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPreservedRegs(RegGroup::kGp, Support::lsbMask<uint32_t>(8));
        cc.setPreservedRegs(RegGroup::kVec, Support::lsbMask<uint32_t>(8) & ~Support::lsbMask<uint32_t>(n));

        cc.setNaturalStackAlignment(16);
        isStandardCallConv = false;
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidArgument);
    }

    if (isStandardCallConv) {
      // MMX arguments is something where compiler vendors disagree. For example GCC and MSVC would pass first three
      // via registers and the rest via stack, however Clang passes all via stack. Returning MMX registers is even
      // more fun, where GCC uses MM0, but Clang uses EAX:EDX pair. I'm not sure it's something we should be worried
      // about as MMX is deprecated anyway.
      cc.setPassedOrder(RegGroup::kX86_MM, 0, 1, 2);

      // Vector arguments (XMM|YMM|ZMM) are passed via registers. However, if the function is variadic then they have
      // to be passed via stack.
      cc.setPassedOrder(RegGroup::kVec, 0, 1, 2);

      // Functions with variable arguments always use stack for MM and vector arguments.
      cc.addFlags(CallConvFlags::kPassVecByStackIfVA);
    }

    if (ccId == CallConvId::kCDecl) {
      cc.addFlags(CallConvFlags::kVarArgCompatible);
    }
  }
  else {
    cc.setSaveRestoreRegSize(RegGroup::kGp, 8);
    cc.setSaveRestoreAlignment(RegGroup::kGp, 8);

    // Preprocess the calling convention into a common id as many conventions are normally ignored even by C/C++
    // compilers and treated as `__cdecl`.
    if (shouldThreatAsCDeclIn64BitMode(ccId))
      ccId = winABI ? CallConvId::kX64Windows : CallConvId::kX64SystemV;

    switch (ccId) {
      case CallConvId::kX64SystemV: {
        cc.setFlags(CallConvFlags::kPassFloatsByVec |
                    CallConvFlags::kPassMmxByXmm    |
                    CallConvFlags::kVarArgCompatible);
        cc.setNaturalStackAlignment(16);
        cc.setRedZoneSize(128);
        cc.setPassedOrder(RegGroup::kGp, kZdi, kZsi, kZdx, kZcx, 8, 9);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(kZbx, kZsp, kZbp, 12, 13, 14, 15));
        break;
      }

      case CallConvId::kX64Windows: {
        cc.setStrategy(CallConvStrategy::kX64Windows);
        cc.setFlags(CallConvFlags::kPassFloatsByVec |
                    CallConvFlags::kIndirectVecArgs |
                    CallConvFlags::kPassMmxByGp     |
                    CallConvFlags::kVarArgCompatible);
        cc.setNaturalStackAlignment(16);
        // Maximum 4 arguments in registers, each adds 8 bytes to the spill zone.
        cc.setSpillZoneSize(4 * 8);
        cc.setPassedOrder(RegGroup::kGp, kZcx, kZdx, 8, 9);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3);
        cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(kZbx, kZsp, kZbp, kZsi, kZdi, 12, 13, 14, 15));
        cc.setPreservedRegs(RegGroup::kVec, Support::bitMask(6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
        break;
      }

      case CallConvId::kVectorCall: {
        cc.setStrategy(CallConvStrategy::kX64VectorCall);
        cc.setFlags(CallConvFlags::kPassFloatsByVec |
                    CallConvFlags::kPassMmxByGp     );
        cc.setNaturalStackAlignment(16);
        // Maximum 6 arguments in registers, each adds 8 bytes to the spill zone.
        cc.setSpillZoneSize(6 * 8);
        cc.setPassedOrder(RegGroup::kGp, kZcx, kZdx, 8, 9);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5);
        cc.setPreservedRegs(RegGroup::kGp, Support::bitMask(kZbx, kZsp, kZbp, kZsi, kZdi, 12, 13, 14, 15));
        cc.setPreservedRegs(RegGroup::kVec, Support::bitMask(6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
        break;
      }

      case CallConvId::kLightCall2:
      case CallConvId::kLightCall3:
      case CallConvId::kLightCall4: {
        uint32_t n = uint32_t(ccId) - uint32_t(CallConvId::kLightCall2) + 2;

        cc.setFlags(CallConvFlags::kPassFloatsByVec);
        cc.setNaturalStackAlignment(16);
        cc.setPassedOrder(RegGroup::kGp, kZax, kZdx, kZcx, kZsi, kZdi);
        cc.setPassedOrder(RegGroup::kVec, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(RegGroup::kX86_K, 0, 1, 2, 3, 4, 5, 6, 7);
        cc.setPassedOrder(RegGroup::kX86_MM, 0, 1, 2, 3, 4, 5, 6, 7);

        cc.setPreservedRegs(RegGroup::kGp, Support::lsbMask<uint32_t>(16));
        cc.setPreservedRegs(RegGroup::kVec, ~Support::lsbMask<uint32_t>(n));
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidArgument);
    }
  }

  cc.setId(ccId);
  return kErrorOk;
}

ASMJIT_FAVOR_SIZE void unpackValues(FuncDetail& func, FuncValuePack& pack) noexcept {
  TypeId typeId = pack[0].typeId();
  switch (typeId) {
    case TypeId::kInt64:
    case TypeId::kUInt64: {
      if (Environment::is32Bit(func.callConv().arch())) {
        // Convert a 64-bit return value to two 32-bit return values.
        pack[0].initTypeId(TypeId::kUInt32);
        pack[1].initTypeId(TypeId(uint32_t(typeId) - 2));
        break;
      }
      break;
    }

    default: {
      break;
    }
  }
}

ASMJIT_FAVOR_SIZE Error initFuncDetail(FuncDetail& func, const FuncSignature& signature, uint32_t registerSize) noexcept {
  const CallConv& cc = func.callConv();
  Arch arch = cc.arch();
  uint32_t stackOffset = cc._spillZoneSize;
  uint32_t argCount = func.argCount();

  // Up to two return values can be returned in GP registers.
  static const uint8_t gpReturnIndexes[4] = {
    uint8_t(Gp::kIdAx),
    uint8_t(Gp::kIdDx),
    uint8_t(BaseReg::kIdBad),
    uint8_t(BaseReg::kIdBad)
  };

  if (func.hasRet()) {
    unpackValues(func, func._rets);
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      TypeId typeId = func._rets[valueIndex].typeId();

      // Terminate at the first void type (end of the pack).
      if (typeId == TypeId::kVoid)
        break;

      switch (typeId) {
        case TypeId::kInt64:
        case TypeId::kUInt64: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(RegType::kX86_Gpq, gpReturnIndexes[valueIndex], typeId);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case TypeId::kInt8:
        case TypeId::kInt16:
        case TypeId::kInt32: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(RegType::kX86_Gpd, gpReturnIndexes[valueIndex], TypeId::kInt32);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case TypeId::kUInt8:
        case TypeId::kUInt16:
        case TypeId::kUInt32: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(RegType::kX86_Gpd, gpReturnIndexes[valueIndex], TypeId::kUInt32);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case TypeId::kFloat32:
        case TypeId::kFloat64: {
          RegType regType = Environment::is32Bit(arch) ? RegType::kX86_St : RegType::kX86_Xmm;
          func._rets[valueIndex].initReg(regType, valueIndex, typeId);
          break;
        }

        case TypeId::kFloat80: {
          // 80-bit floats are always returned by FP0.
          func._rets[valueIndex].initReg(RegType::kX86_St, valueIndex, typeId);
          break;
        }

        case TypeId::kMmx32:
        case TypeId::kMmx64: {
          // MM registers are returned through XMM (SystemV) or GPQ (Win64).
          RegType regType = RegType::kX86_Mm;
          uint32_t regIndex = valueIndex;
          if (Environment::is64Bit(arch)) {
            regType = cc.strategy() == CallConvStrategy::kDefault ? RegType::kX86_Xmm : RegType::kX86_Gpq;
            regIndex = cc.strategy() == CallConvStrategy::kDefault ? valueIndex : gpReturnIndexes[valueIndex];

            if (regIndex == BaseReg::kIdBad)
              return DebugUtils::errored(kErrorInvalidState);
          }

          func._rets[valueIndex].initReg(regType, regIndex, typeId);
          break;
        }

        default: {
          func._rets[valueIndex].initReg(vecTypeIdToRegType(typeId), valueIndex, typeId);
          break;
        }
      }
    }
  }

  switch (cc.strategy()) {
    case CallConvStrategy::kDefault: {
      uint32_t gpzPos = 0;
      uint32_t vecPos = 0;

      for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
        unpackValues(func, func._args[argIndex]);

        for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
          FuncValue& arg = func._args[argIndex][valueIndex];

          // Terminate if there are no more arguments in the pack.
          if (!arg)
            break;

          TypeId typeId = arg.typeId();

          if (TypeUtils::isInt(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (gpzPos < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[RegGroup::kGp].id[gpzPos];

            if (regId != BaseReg::kIdBad) {
              RegType regType = typeId <= TypeId::kUInt32 ? RegType::kX86_Gpd : RegType::kX86_Gpq;
              arg.assignRegData(regType, regId);
              func.addUsedRegs(RegGroup::kGp, Support::bitMask(regId));
              gpzPos++;
            }
            else {
              uint32_t size = Support::max<uint32_t>(TypeUtils::sizeOf(typeId), registerSize);
              arg.assignStackOffset(int32_t(stackOffset));
              stackOffset += size;
            }
            continue;
          }

          if (TypeUtils::isFloat(typeId) || TypeUtils::isVec(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (vecPos < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[RegGroup::kVec].id[vecPos];

            if (TypeUtils::isFloat(typeId)) {
              // If this is a float, but `kFlagPassFloatsByVec` is false, we have to use stack instead. This should
              // be only used by 32-bit calling conventions.
              if (!cc.hasFlag(CallConvFlags::kPassFloatsByVec))
                regId = BaseReg::kIdBad;
            }
            else {
              // Pass vector registers via stack if this is a variable arguments function. This should be only used
              // by 32-bit calling conventions.
              if (signature.hasVarArgs() && cc.hasFlag(CallConvFlags::kPassVecByStackIfVA))
                regId = BaseReg::kIdBad;
            }

            if (regId != BaseReg::kIdBad) {
              arg.initTypeId(typeId);
              arg.assignRegData(vecTypeIdToRegType(typeId), regId);
              func.addUsedRegs(RegGroup::kVec, Support::bitMask(regId));
              vecPos++;
            }
            else {
              uint32_t size = TypeUtils::sizeOf(typeId);
              arg.assignStackOffset(int32_t(stackOffset));
              stackOffset += size;
            }
            continue;
          }
        }
      }
      break;
    }

    case CallConvStrategy::kX64Windows:
    case CallConvStrategy::kX64VectorCall: {
      // Both X64 and VectorCall behave similarly - arguments are indexed from left to right. The position of the
      // argument determines in which register the argument is allocated, so it's either GP or one of XMM/YMM/ZMM
      // registers.
      //
      //       [       X64       ] [VecCall]
      // Index: #0   #1   #2   #3   #4   #5
      //
      // GP   : RCX  RDX  R8   R9
      // VEC  : XMM0 XMM1 XMM2 XMM3 XMM4 XMM5
      //
      // For example function `f(int a, double b, int c, double d)` will be:
      //
      //        (a)  (b)  (c)  (d)
      //        RCX  XMM1 R8   XMM3
      //
      // Unused vector registers are used by HVA.
      bool isVectorCall = (cc.strategy() == CallConvStrategy::kX64VectorCall);

      for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
        unpackValues(func, func._args[argIndex]);

        for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
          FuncValue& arg = func._args[argIndex][valueIndex];

          // Terminate if there are no more arguments in the pack.
          if (!arg)
            break;

          TypeId typeId = arg.typeId();
          uint32_t size = TypeUtils::sizeOf(typeId);

          if (TypeUtils::isInt(typeId) || TypeUtils::isMmx(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (argIndex < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[RegGroup::kGp].id[argIndex];

            if (regId != BaseReg::kIdBad) {
              RegType regType = size <= 4 && !TypeUtils::isMmx(typeId) ? RegType::kX86_Gpd : RegType::kX86_Gpq;
              arg.assignRegData(regType, regId);
              func.addUsedRegs(RegGroup::kGp, Support::bitMask(regId));
            }
            else {
              arg.assignStackOffset(int32_t(stackOffset));
              stackOffset += 8;
            }
            continue;
          }

          if (TypeUtils::isFloat(typeId) || TypeUtils::isVec(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (argIndex < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[RegGroup::kVec].id[argIndex];

            if (regId != BaseReg::kIdBad) {
              // X64-ABI doesn't allow vector types (XMM|YMM|ZMM) to be passed via registers, however, VectorCall
              // was designed for that purpose.
              if (TypeUtils::isFloat(typeId) || isVectorCall) {
                RegType regType = vecTypeIdToRegType(typeId);
                arg.assignRegData(regType, regId);
                func.addUsedRegs(RegGroup::kVec, Support::bitMask(regId));
                continue;
              }
            }

            // Passed via stack if the argument is float/double or indirectly. The trap is - if the argument is
            // passed indirectly, the address can be passed via register, if the argument's index has GP one.
            if (TypeUtils::isFloat(typeId)) {
              arg.assignStackOffset(int32_t(stackOffset));
            }
            else {
              uint32_t gpRegId = cc._passedOrder[RegGroup::kGp].id[argIndex];
              if (gpRegId != BaseReg::kIdBad)
                arg.assignRegData(RegType::kX86_Gpq, gpRegId);
              else
                arg.assignStackOffset(int32_t(stackOffset));
              arg.addFlags(FuncValue::kFlagIsIndirect);
            }

            // Always 8 bytes (float/double/pointer).
            stackOffset += 8;
            continue;
          }
        }
      }
      break;
    }
  }

  func._argStackSize = stackOffset;
  return kErrorOk;
}

} // {FuncInternal}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86
