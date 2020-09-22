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

#include "../core/formatter.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../x86/x86internal_p.h"

// Can be used for debugging...
// #define ASMJIT_DUMP_ARGS_ASSIGNMENT

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// ============================================================================
// [asmjit::X86Internal - Helpers]
// ============================================================================

static ASMJIT_INLINE uint32_t x86GetXmmMovInst(const FuncFrame& frame) {
  bool avx = frame.isAvxEnabled();
  bool aligned = frame.hasAlignedVecSR();

  return aligned ? (avx ? Inst::kIdVmovaps : Inst::kIdMovaps)
                 : (avx ? Inst::kIdVmovups : Inst::kIdMovups);
}

static ASMJIT_INLINE uint32_t x86VecTypeIdToRegType(uint32_t typeId) noexcept {
  return typeId <= Type::_kIdVec128End ? Reg::kTypeXmm :
         typeId <= Type::_kIdVec256End ? Reg::kTypeYmm : Reg::kTypeZmm;
}

//! Converts `size` to a 'kmov?' instructio.
static inline uint32_t x86KmovFromSize(uint32_t size) noexcept {
  switch (size) {
    case  1: return Inst::kIdKmovb;
    case  2: return Inst::kIdKmovw;
    case  4: return Inst::kIdKmovd;
    case  8: return Inst::kIdKmovq;
    default: return Inst::kIdNone;
  }
}

// ============================================================================
// [asmjit::X86Internal - FuncDetail]
// ============================================================================

ASMJIT_FAVOR_SIZE void unpackValues(FuncDetail& func, FuncValuePack& pack) noexcept {
  uint32_t typeId = pack[0].typeId();
  switch (typeId) {
    case Type::kIdI64:
    case Type::kIdU64: {
      if (Environment::is32Bit(func.callConv().arch())) {
        // Convert a 64-bit return value to two 32-bit return values.
        pack[0].initTypeId(Type::kIdU32);
        pack[1].initTypeId(typeId - 2);
        break;
      }
      break;
    }
  }
}

ASMJIT_FAVOR_SIZE Error X86Internal::initFuncDetail(FuncDetail& func, const FuncSignature& signature, uint32_t registerSize) noexcept {
  const CallConv& cc = func.callConv();
  uint32_t arch = cc.arch();
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
      uint32_t typeId = func._rets[valueIndex].typeId();

      // Terminate at the first void type (end of the pack).
      if (!typeId)
        break;

      switch (typeId) {
        case Type::kIdI64:
        case Type::kIdU64: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(Reg::kTypeGpq, gpReturnIndexes[valueIndex], typeId);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case Type::kIdI8:
        case Type::kIdI16:
        case Type::kIdI32: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(Reg::kTypeGpd, gpReturnIndexes[valueIndex], Type::kIdI32);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case Type::kIdU8:
        case Type::kIdU16:
        case Type::kIdU32: {
          if (gpReturnIndexes[valueIndex] != BaseReg::kIdBad)
            func._rets[valueIndex].initReg(Reg::kTypeGpd, gpReturnIndexes[valueIndex], Type::kIdU32);
          else
            return DebugUtils::errored(kErrorInvalidState);
          break;
        }

        case Type::kIdF32:
        case Type::kIdF64: {
          uint32_t regType = Environment::is32Bit(arch) ? Reg::kTypeSt : Reg::kTypeXmm;
          func._rets[valueIndex].initReg(regType, valueIndex, typeId);
          break;
        }

        case Type::kIdF80: {
          // 80-bit floats are always returned by FP0.
          func._rets[valueIndex].initReg(Reg::kTypeSt, valueIndex, typeId);
          break;
        }

        case Type::kIdMmx32:
        case Type::kIdMmx64: {
          // MM registers are returned through XMM (SystemV) or GPQ (Win64).
          uint32_t regType = Reg::kTypeMm;
          uint32_t regIndex = valueIndex;
          if (Environment::is64Bit(arch)) {
            regType = cc.strategy() == CallConv::kStrategyDefault ? Reg::kTypeXmm : Reg::kTypeGpq;
            regIndex = cc.strategy() == CallConv::kStrategyDefault ? valueIndex : gpReturnIndexes[valueIndex];

            if (regIndex == BaseReg::kIdBad)
              return DebugUtils::errored(kErrorInvalidState);
          }

          func._rets[valueIndex].initReg(regType, regIndex, typeId);
          break;
        }

        default: {
          func._rets[valueIndex].initReg(x86VecTypeIdToRegType(typeId), valueIndex, typeId);
          break;
        }
      }
    }
  }

  switch (cc.strategy()) {
    case CallConv::kStrategyDefault: {
      uint32_t gpzPos = 0;
      uint32_t vecPos = 0;

      for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
        unpackValues(func, func._args[argIndex]);

        for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
          FuncValue& arg = func._args[argIndex][valueIndex];

          // Terminate if there are no more arguments in the pack.
          if (!arg)
            break;

          uint32_t typeId = arg.typeId();

          if (Type::isInt(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (gpzPos < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[Reg::kGroupGp].id[gpzPos];

            if (regId != BaseReg::kIdBad) {
              uint32_t regType = (typeId <= Type::kIdU32) ? Reg::kTypeGpd : Reg::kTypeGpq;
              arg.assignRegData(regType, regId);
              func.addUsedRegs(Reg::kGroupGp, Support::bitMask(regId));
              gpzPos++;
            }
            else {
              uint32_t size = Support::max<uint32_t>(Type::sizeOf(typeId), registerSize);
              arg.assignStackOffset(int32_t(stackOffset));
              stackOffset += size;
            }
            continue;
          }

          if (Type::isFloat(typeId) || Type::isVec(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (vecPos < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[Reg::kGroupVec].id[vecPos];

            if (Type::isFloat(typeId)) {
              // If this is a float, but `kFlagPassFloatsByVec` is false, we have
              // to use stack instead. This should be only used by 32-bit calling
              // conventions.
              if (!cc.hasFlag(CallConv::kFlagPassFloatsByVec))
                regId = BaseReg::kIdBad;
            }
            else {
              // Pass vector registers via stack if this is a variable arguments
              // function. This should be only used by 32-bit calling conventions.
              if (signature.hasVarArgs() && cc.hasFlag(CallConv::kFlagPassVecByStackIfVA))
                regId = BaseReg::kIdBad;
            }

            if (regId != BaseReg::kIdBad) {
              arg.initTypeId(typeId);
              arg.assignRegData(x86VecTypeIdToRegType(typeId), regId);
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
      }
      break;
    }

    case CallConv::kStrategyX64Windows:
    case CallConv::kStrategyX64VectorCall: {
      // Both X64 and VectorCall behave similarly - arguments are indexed
      // from left to right. The position of the argument determines in
      // which register the argument is allocated, so it's either GP or
      // one of XMM/YMM/ZMM registers.
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
      bool isVectorCall = (cc.strategy() == CallConv::kStrategyX64VectorCall);

      for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
        unpackValues(func, func._args[argIndex]);

        for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
          FuncValue& arg = func._args[argIndex][valueIndex];

          // Terminate if there are no more arguments in the pack.
          if (!arg)
            break;

          uint32_t typeId = arg.typeId();
          uint32_t size = Type::sizeOf(typeId);

          if (Type::isInt(typeId) || Type::isMmx(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (argIndex < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[Reg::kGroupGp].id[argIndex];

            if (regId != BaseReg::kIdBad) {
              uint32_t regType = (size <= 4 && !Type::isMmx(typeId)) ? Reg::kTypeGpd : Reg::kTypeGpq;
              arg.assignRegData(regType, regId);
              func.addUsedRegs(Reg::kGroupGp, Support::bitMask(regId));
            }
            else {
              arg.assignStackOffset(int32_t(stackOffset));
              stackOffset += 8;
            }
            continue;
          }

          if (Type::isFloat(typeId) || Type::isVec(typeId)) {
            uint32_t regId = BaseReg::kIdBad;

            if (argIndex < CallConv::kMaxRegArgsPerGroup)
              regId = cc._passedOrder[Reg::kGroupVec].id[argIndex];

            if (regId != BaseReg::kIdBad) {
              // X64-ABI doesn't allow vector types (XMM|YMM|ZMM) to be passed
              // via registers, however, VectorCall was designed for that purpose.
              if (Type::isFloat(typeId) || isVectorCall) {
                uint32_t regType = x86VecTypeIdToRegType(typeId);
                arg.assignRegData(regType, regId);
                func.addUsedRegs(Reg::kGroupVec, Support::bitMask(regId));
                continue;
              }
            }

            // Passed via stack if the argument is float/double or indirectly.
            // The trap is - if the argument is passed indirectly, the address
            // can be passed via register, if the argument's index has GP one.
            if (Type::isFloat(typeId)) {
              arg.assignStackOffset(int32_t(stackOffset));
            }
            else {
              uint32_t gpRegId = cc._passedOrder[Reg::kGroupGp].id[argIndex];
              if (gpRegId != BaseReg::kIdBad)
                arg.assignRegData(Reg::kTypeGpq, gpRegId);
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

// ============================================================================
// [asmjit::X86FuncArgsContext]
// ============================================================================

static RegInfo x86GetRegForMemToMemMove(uint32_t arch, uint32_t dstTypeId, uint32_t srcTypeId) noexcept {
  uint32_t dstSize = Type::sizeOf(dstTypeId);
  uint32_t srcSize = Type::sizeOf(srcTypeId);
  uint32_t maxSize = Support::max<uint32_t>(dstSize, srcSize);
  uint32_t regSize = Environment::registerSizeFromArch(arch);

  uint32_t signature = 0;
  if (maxSize <= regSize || (Type::isInt(dstTypeId) && Type::isInt(srcTypeId)))
    signature = maxSize <= 4 ? Gpd::kSignature : Gpq::kSignature;
  else if (maxSize <= 16)
    signature = Xmm::kSignature;
  else if (maxSize <= 32)
    signature = Ymm::kSignature;
  else if (maxSize <= 64)
    signature = Zmm::kSignature;

  return RegInfo { signature };
}

// Used by both `argsToFuncFrame()` and `emitArgsAssignment()`.
class X86FuncArgsContext {
public:
  enum VarId : uint32_t {
    kVarIdNone = 0xFF
  };

  //! Contains information about a single argument or SA register that may need shuffling.
  struct Var {
    inline void init(const FuncValue& cur_, const FuncValue& out_) noexcept {
      cur = cur_;
      out = out_;
    }

    //! Reset the value to its unassigned state.
    inline void reset() noexcept {
      cur.reset();
      out.reset();
    }

    inline bool isDone() const noexcept { return cur.isDone(); }
    inline void markDone() noexcept { cur.addFlags(FuncValue::kFlagIsDone); }

    FuncValue cur;
    FuncValue out;
  };

  struct WorkData {
    inline void reset() noexcept {
      _archRegs = 0;
      _workRegs = 0;
      _usedRegs = 0;
      _assignedRegs = 0;
      _dstRegs = 0;
      _dstShuf = 0;
      _numSwaps = 0;
      _numStackArgs = 0;
      memset(_reserved, 0, sizeof(_reserved));
      memset(_physToVarId, kVarIdNone, 32);
    }

    inline bool isAssigned(uint32_t regId) const noexcept {
      ASMJIT_ASSERT(regId < 32);
      return Support::bitTest(_assignedRegs, regId);
    }

    inline void assign(uint32_t varId, uint32_t regId) noexcept {
      ASMJIT_ASSERT(!isAssigned(regId));
      ASMJIT_ASSERT(_physToVarId[regId] == kVarIdNone);

      _physToVarId[regId] = uint8_t(varId);
      _assignedRegs ^= Support::bitMask(regId);
    }

    inline void reassign(uint32_t varId, uint32_t newId, uint32_t oldId) noexcept {
      ASMJIT_ASSERT( isAssigned(oldId));
      ASMJIT_ASSERT(!isAssigned(newId));
      ASMJIT_ASSERT(_physToVarId[oldId] == varId);
      ASMJIT_ASSERT(_physToVarId[newId] == kVarIdNone);

      _physToVarId[oldId] = uint8_t(kVarIdNone);
      _physToVarId[newId] = uint8_t(varId);
      _assignedRegs ^= Support::bitMask(newId) ^ Support::bitMask(oldId);
    }

    inline void swap(uint32_t aVarId, uint32_t aRegId, uint32_t bVarId, uint32_t bRegId) noexcept {
      ASMJIT_ASSERT(isAssigned(aRegId));
      ASMJIT_ASSERT(isAssigned(bRegId));
      ASMJIT_ASSERT(_physToVarId[aRegId] == aVarId);
      ASMJIT_ASSERT(_physToVarId[bRegId] == bVarId);

      _physToVarId[aRegId] = uint8_t(bVarId);
      _physToVarId[bRegId] = uint8_t(aVarId);
    }

    inline void unassign(uint32_t varId, uint32_t regId) noexcept {
      ASMJIT_ASSERT(isAssigned(regId));
      ASMJIT_ASSERT(_physToVarId[regId] == varId);

      DebugUtils::unused(varId);
      _physToVarId[regId] = uint8_t(kVarIdNone);
      _assignedRegs ^= Support::bitMask(regId);
    }

    inline uint32_t archRegs() const noexcept { return _archRegs; }
    inline uint32_t workRegs() const noexcept { return _workRegs; }
    inline uint32_t usedRegs() const noexcept { return _usedRegs; }
    inline uint32_t assignedRegs() const noexcept { return _assignedRegs; }
    inline uint32_t dstRegs() const noexcept { return _dstRegs; }
    inline uint32_t availableRegs() const noexcept { return _workRegs & ~_assignedRegs; }

    uint32_t _archRegs;                  //!< All allocable registers provided by the architecture.
    uint32_t _workRegs;                  //!< All registers that can be used by the shuffler.
    uint32_t _usedRegs;                  //!< Registers used by the shuffler (all).
    uint32_t _assignedRegs;              //!< Assigned registers.
    uint32_t _dstRegs;                   //!< Destination registers assigned to arguments or SA.
    uint32_t _dstShuf;                   //!< Destination registers that require shuffling.
    uint8_t _numSwaps;                   //!< Number of register swaps.
    uint8_t _numStackArgs;               //!< Number of stack loads.
    uint8_t _reserved[6];                //!< Reserved (only used as padding).
    uint8_t _physToVarId[32];            //!< Physical ID to variable ID mapping.
  };

  uint8_t _arch;
  bool _hasStackSrc;                     //!< Has arguments passed via stack (SRC).
  bool _hasPreservedFP;                  //!< Has preserved frame-pointer (FP).
  uint8_t _stackDstMask;                 //!< Has arguments assigned to stack (DST).
  uint8_t _regSwapsMask;                 //!< Register swap groups (bit-mask).
  uint8_t _saVarId;
  uint32_t _varCount;
  WorkData _workData[BaseReg::kGroupVirt];
  Var _vars[Globals::kMaxFuncArgs + 1];

  X86FuncArgsContext() noexcept;

  inline uint32_t arch() const noexcept { return _arch; }
  inline uint32_t varCount() const noexcept { return _varCount; }

  inline Var& var(size_t varId) noexcept { return _vars[varId]; }
  inline const Var& var(size_t varId) const noexcept { return _vars[varId]; }
  inline size_t indexOf(const Var* var) const noexcept { return (size_t)(var - _vars); }

  Error initWorkData(const FuncFrame& frame, const FuncArgsAssignment& args) noexcept;
  Error markScratchRegs(FuncFrame& frame) noexcept;
  Error markDstRegsDirty(FuncFrame& frame) noexcept;
  Error markStackArgsReg(FuncFrame& frame) noexcept;
};

X86FuncArgsContext::X86FuncArgsContext() noexcept {
  _arch = Environment::kArchUnknown;
  _varCount = 0;
  _hasStackSrc = false;
  _hasPreservedFP = false;
  _stackDstMask = 0;
  _regSwapsMask = 0;
  _saVarId = kVarIdNone;

  for (uint32_t group = 0; group < BaseReg::kGroupVirt; group++)
    _workData[group].reset();
}

ASMJIT_FAVOR_SIZE Error X86FuncArgsContext::initWorkData(const FuncFrame& frame, const FuncArgsAssignment& args) noexcept {
  // The code has to be updated if this changes.
  ASMJIT_ASSERT(BaseReg::kGroupVirt == 4);

  const FuncDetail& func = *args.funcDetail();

  // Initialize Architecture.
  uint32_t arch = func.callConv().arch();
  uint32_t archRegCount = Environment::is32Bit(arch) ? 8 : 16;

  _arch = uint8_t(arch);

  // Initialize `_archRegs`.
  _workData[Reg::kGroupGp  ]._archRegs = Support::lsbMask<uint32_t>(archRegCount) & ~Support::bitMask(Gp::kIdSp);
  _workData[Reg::kGroupVec ]._archRegs = Support::lsbMask<uint32_t>(archRegCount);
  _workData[Reg::kGroupMm  ]._archRegs = Support::lsbMask<uint32_t>(8);
  _workData[Reg::kGroupKReg]._archRegs = Support::lsbMask<uint32_t>(8);

  if (frame.hasPreservedFP())
    _workData[Reg::kGroupGp]._archRegs &= ~Support::bitMask(Gp::kIdBp);

  // Extract information from all function arguments/assignments and build Var[] array.
  uint32_t varId = 0;
  for (uint32_t argIndex = 0; argIndex < Globals::kMaxFuncArgs; argIndex++) {
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      const FuncValue& dst_ = args.arg(argIndex, valueIndex);
      if (!dst_.isAssigned())
        continue;

      const FuncValue& src_ = func.arg(argIndex, valueIndex);
      if (ASMJIT_UNLIKELY(!src_.isAssigned()))
        return DebugUtils::errored(kErrorInvalidState);

      Var& var = _vars[varId];
      var.init(src_, dst_);

      FuncValue& src = var.cur;
      FuncValue& dst = var.out;

      uint32_t dstGroup = 0xFFFFFFFFu;
      uint32_t dstId = BaseReg::kIdBad;
      WorkData* dstWd = nullptr;

      // Not supported.
      if (src.isIndirect())
        return DebugUtils::errored(kErrorInvalidAssignment);

      if (dst.isReg()) {
        uint32_t dstType = dst.regType();
        if (ASMJIT_UNLIKELY(dstType >= Reg::kTypeCount))
          return DebugUtils::errored(kErrorInvalidRegType);

        // Copy TypeId from source if the destination doesn't have it. The RA
        // used by BaseCompiler would never leave TypeId undefined, but users
        // of FuncAPI can just assign phys regs without specifying the type.
        if (!dst.hasTypeId())
          dst.setTypeId(Reg::typeIdOf(dst.regType()));

        dstGroup = Reg::groupOf(dstType);
        if (ASMJIT_UNLIKELY(dstGroup >= BaseReg::kGroupVirt))
          return DebugUtils::errored(kErrorInvalidRegGroup);

        dstWd = &_workData[dstGroup];
        dstId = dst.regId();
        if (ASMJIT_UNLIKELY(dstId >= 32 || !Support::bitTest(dstWd->archRegs(), dstId)))
          return DebugUtils::errored(kErrorInvalidPhysId);

        if (ASMJIT_UNLIKELY(Support::bitTest(dstWd->dstRegs(), dstId)))
          return DebugUtils::errored(kErrorOverlappedRegs);

        dstWd->_dstRegs  |= Support::bitMask(dstId);
        dstWd->_dstShuf  |= Support::bitMask(dstId);
        dstWd->_usedRegs |= Support::bitMask(dstId);
      }
      else {
        if (!dst.hasTypeId())
          dst.setTypeId(src.typeId());

        RegInfo regInfo = x86GetRegForMemToMemMove(arch, dst.typeId(), src.typeId());
        if (ASMJIT_UNLIKELY(!regInfo.isValid()))
          return DebugUtils::errored(kErrorInvalidState);
        _stackDstMask = uint8_t(_stackDstMask | Support::bitMask(regInfo.group()));
      }

      if (src.isReg()) {
        uint32_t srcId = src.regId();
        uint32_t srcGroup = Reg::groupOf(src.regType());

        if (dstGroup == srcGroup) {
          dstWd->assign(varId, srcId);

          // The best case, register is allocated where it is expected to be.
          if (dstId == srcId)
            var.markDone();
        }
        else {
          if (ASMJIT_UNLIKELY(srcGroup >= BaseReg::kGroupVirt))
            return DebugUtils::errored(kErrorInvalidState);

          WorkData& srcData = _workData[srcGroup];
          srcData.assign(varId, srcId);
        }
      }
      else {
        if (dstWd)
          dstWd->_numStackArgs++;
        _hasStackSrc = true;
      }

      varId++;
    }
  }

  // Initialize WorkData::workRegs.
  uint32_t i;
  for (i = 0; i < BaseReg::kGroupVirt; i++)
    _workData[i]._workRegs = (_workData[i].archRegs() & (frame.dirtyRegs(i) | ~frame.preservedRegs(i))) | _workData[i].dstRegs() | _workData[i].assignedRegs();

  // Create a variable that represents `SARegId` if necessary.
  bool saRegRequired = _hasStackSrc && frame.hasDynamicAlignment() && !frame.hasPreservedFP();

  WorkData& gpRegs = _workData[BaseReg::kGroupGp];
  uint32_t saCurRegId = frame.saRegId();
  uint32_t saOutRegId = args.saRegId();

  if (saCurRegId != BaseReg::kIdBad) {
    // Check if the provided `SARegId` doesn't collide with input registers.
    if (ASMJIT_UNLIKELY(gpRegs.isAssigned(saCurRegId)))
      return DebugUtils::errored(kErrorOverlappedRegs);
  }

  if (saOutRegId != BaseReg::kIdBad) {
    // Check if the provided `SARegId` doesn't collide with argument assignments.
    if (ASMJIT_UNLIKELY(Support::bitTest(gpRegs.dstRegs(), saOutRegId)))
      return DebugUtils::errored(kErrorOverlappedRegs);
    saRegRequired = true;
  }

  if (saRegRequired) {
    uint32_t ptrTypeId = Environment::is32Bit(arch) ? Type::kIdU32 : Type::kIdU64;
    uint32_t ptrRegType = Environment::is32Bit(arch) ? BaseReg::kTypeGp32 : BaseReg::kTypeGp64;

    _saVarId = uint8_t(varId);
    _hasPreservedFP = frame.hasPreservedFP();

    Var& var = _vars[varId];
    var.reset();

    if (saCurRegId == BaseReg::kIdBad) {
      if (saOutRegId != BaseReg::kIdBad && !gpRegs.isAssigned(saOutRegId)) {
        saCurRegId = saOutRegId;
      }
      else {
        uint32_t availableRegs = gpRegs.availableRegs();
        if (!availableRegs)
          availableRegs = gpRegs.archRegs() & ~gpRegs.workRegs();

        if (ASMJIT_UNLIKELY(!availableRegs))
          return DebugUtils::errored(kErrorNoMorePhysRegs);

        saCurRegId = Support::ctz(availableRegs);
      }
    }

    var.cur.initReg(ptrRegType, saCurRegId, ptrTypeId);
    gpRegs.assign(varId, saCurRegId);
    gpRegs._workRegs |= Support::bitMask(saCurRegId);

    if (saOutRegId != BaseReg::kIdBad) {
      var.out.initReg(ptrRegType, saOutRegId, ptrTypeId);
      gpRegs._dstRegs  |= Support::bitMask(saOutRegId);
      gpRegs._workRegs |= Support::bitMask(saOutRegId);
    }
    else {
      var.markDone();
    }

    varId++;
  }

  _varCount = varId;

  // Detect register swaps.
  for (varId = 0; varId < _varCount; varId++) {
    Var& var = _vars[varId];
    if (var.cur.isReg() && var.out.isReg()) {
      uint32_t srcId = var.cur.regId();
      uint32_t dstId = var.out.regId();

      uint32_t group = Reg::groupOf(var.cur.regType());
      if (group != Reg::groupOf(var.out.regType()))
        continue;

      WorkData& wd = _workData[group];
      if (wd.isAssigned(dstId)) {
        Var& other = _vars[wd._physToVarId[dstId]];
        if (Reg::groupOf(other.out.regType()) == group && other.out.regId() == srcId) {
          wd._numSwaps++;
          _regSwapsMask = uint8_t(_regSwapsMask | Support::bitMask(group));
        }
      }
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error X86FuncArgsContext::markDstRegsDirty(FuncFrame& frame) noexcept {
  for (uint32_t i = 0; i < BaseReg::kGroupVirt; i++) {
    WorkData& wd = _workData[i];
    uint32_t regs = wd.usedRegs() | wd._dstShuf;

    wd._workRegs |= regs;
    frame.addDirtyRegs(i, regs);
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error X86FuncArgsContext::markScratchRegs(FuncFrame& frame) noexcept {
  uint32_t groupMask = 0;

  // Handle stack to stack moves.
  groupMask |= _stackDstMask;

  // Handle register swaps.
  groupMask |= _regSwapsMask & ~Support::bitMask(BaseReg::kGroupGp);

  if (!groupMask)
    return kErrorOk;

  // Selects one dirty register per affected group that can be used as a scratch register.
  for (uint32_t group = 0; group < BaseReg::kGroupVirt; group++) {
    if (Support::bitTest(groupMask, group)) {
      WorkData& wd = _workData[group];

      // Initially, pick some clobbered or dirty register.
      uint32_t workRegs = wd.workRegs();
      uint32_t regs = workRegs & ~(wd.usedRegs() | wd._dstShuf);

      // If that didn't work out pick some register which is not in 'used'.
      if (!regs)
        regs = workRegs & ~wd.usedRegs();

      // If that didn't work out pick any other register that is allocable.
      // This last resort case will, however, result in marking one more
      // register dirty.
      if (!regs)
        regs = wd.archRegs() & ~workRegs;

      // If that didn't work out we will have to use XORs instead of MOVs.
      if (!regs)
        continue;

      uint32_t regMask = Support::blsi(regs);
      wd._workRegs |= regMask;
      frame.addDirtyRegs(group, regMask);
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error X86FuncArgsContext::markStackArgsReg(FuncFrame& frame) noexcept {
  if (_saVarId != kVarIdNone) {
    const Var& var = _vars[_saVarId];
    frame.setSARegId(var.cur.regId());
  }
  else if (frame.hasPreservedFP()) {
    // Always EBP|RBP if the frame-pointer isn't omitted.
    frame.setSARegId(Gp::kIdBp);
  }

  return kErrorOk;
}

// ============================================================================
// [asmjit::X86Internal - FrameLayout]
// ============================================================================

ASMJIT_FAVOR_SIZE Error X86Internal::initFuncFrame(FuncFrame& frame, const FuncDetail& func) noexcept {
  uint32_t arch = func.callConv().arch();

  // Initializing FuncFrame means making a copy of some properties of `func`.
  // Properties like `_localStackSize` will be set by the user before the frame
  // is finalized.
  frame.reset();

  frame._arch = uint8_t(arch);
  frame._spRegId = Gp::kIdSp;
  frame._saRegId = Gp::kIdBad;

  uint32_t naturalStackAlignment = func.callConv().naturalStackAlignment();
  uint32_t minDynamicAlignment = Support::max<uint32_t>(naturalStackAlignment, 16);

  if (minDynamicAlignment == naturalStackAlignment)
    minDynamicAlignment <<= 1;

  frame._naturalStackAlignment = uint8_t(naturalStackAlignment);
  frame._minDynamicAlignment = uint8_t(minDynamicAlignment);
  frame._redZoneSize = uint8_t(func.redZoneSize());
  frame._spillZoneSize = uint8_t(func.spillZoneSize());
  frame._finalStackAlignment = uint8_t(frame._naturalStackAlignment);

  if (func.hasFlag(CallConv::kFlagCalleePopsStack)) {
    frame._calleeStackCleanup = uint16_t(func.argStackSize());
  }

  // Initial masks of dirty and preserved registers.
  for (uint32_t group = 0; group < BaseReg::kGroupVirt; group++) {
    frame._dirtyRegs[group] = func.usedRegs(group);
    frame._preservedRegs[group] = func.preservedRegs(group);
  }

  // Exclude ESP/RSP - this register is never included in saved GP regs.
  frame._preservedRegs[BaseReg::kGroupGp] &= ~Support::bitMask(Gp::kIdSp);

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error X86Internal::finalizeFuncFrame(FuncFrame& frame) noexcept {
  uint32_t registerSize = Environment::registerSizeFromArch(frame.arch());

  // The final stack alignment must be updated accordingly to call and local stack alignments.
  uint32_t stackAlignment = frame._finalStackAlignment;
  ASMJIT_ASSERT(stackAlignment == Support::max(frame._naturalStackAlignment,
                                               frame._callStackAlignment,
                                               frame._localStackAlignment));

  // TODO: Must be configurable.
  uint32_t vecSize = 16;

  bool hasFP = frame.hasPreservedFP();
  bool hasDA = frame.hasDynamicAlignment();

  // Include EBP|RBP if the function preserves the frame-pointer.
  if (hasFP)
    frame._dirtyRegs[Reg::kGroupGp] |= Support::bitMask(Gp::kIdBp);

  // These two are identical if the function doesn't align its stack dynamically.
  uint32_t saRegId = frame.saRegId();
  if (saRegId == BaseReg::kIdBad)
    saRegId = Gp::kIdSp;

  // Fix stack arguments base-register from ESP|RSP to EBP|RBP in case it was
  // not picked before and the function performs dynamic stack alignment.
  if (hasDA && saRegId == Gp::kIdSp)
    saRegId = Gp::kIdBp;

  // Mark as dirty any register but ESP|RSP if used as SA pointer.
  if (saRegId != Gp::kIdSp)
    frame._dirtyRegs[Reg::kGroupGp] |= Support::bitMask(saRegId);

  frame._spRegId = uint8_t(Gp::kIdSp);
  frame._saRegId = uint8_t(saRegId);

  // Setup stack size used to save preserved registers.
  frame._gpSaveSize    = uint16_t(Support::popcnt(frame.savedRegs(Reg::kGroupGp  )) * registerSize);
  frame._nonGpSaveSize = uint16_t(Support::popcnt(frame.savedRegs(Reg::kGroupVec )) * vecSize +
                                  Support::popcnt(frame.savedRegs(Reg::kGroupMm  )) * 8 +
                                  Support::popcnt(frame.savedRegs(Reg::kGroupKReg)) * 8);

  uint32_t v = 0;                             // The beginning of the stack frame relative to SP after prolog.
  v += frame.callStackSize();                 // Count 'callStackSize'    <- This is used to call functions.
  v  = Support::alignUp(v, stackAlignment);   // Align to function's stack alignment.

  frame._localStackOffset = v;                // Store 'localStackOffset' <- Function's local stack starts here.
  v += frame.localStackSize();                // Count 'localStackSize'   <- Function's local stack ends here.

  // If the function's stack must be aligned, calculate the alignment necessary
  // to store vector registers, and set `FuncFrame::kAttrAlignedVecSR` to inform
  // PEI that it can use instructions that perform aligned stores/loads.
  if (stackAlignment >= vecSize && frame._nonGpSaveSize) {
    frame.addAttributes(FuncFrame::kAttrAlignedVecSR);
    v = Support::alignUp(v, vecSize);         // Align '_nonGpSaveOffset'.
  }

  frame._nonGpSaveOffset = v;                 // Store '_nonGpSaveOffset' <- Non-GP Save/Restore starts here.
  v += frame._nonGpSaveSize;                  // Count '_nonGpSaveSize'   <- Non-GP Save/Restore ends here.

  // Calculate if dynamic alignment (DA) slot (stored as offset relative to SP) is required and its offset.
  if (hasDA && !hasFP) {
    frame._daOffset = v;                      // Store 'daOffset'         <- DA pointer would be stored here.
    v += registerSize;                        // Count 'daOffset'.
  }
  else {
    frame._daOffset = FuncFrame::kTagInvalidOffset;
  }

  // The return address should be stored after GP save/restore regs. It has
  // the same size as `registerSize` (basically the native register/pointer
  // size). We don't adjust it now as `v` now contains the exact size that the
  // function requires to adjust (call frame + stack frame, vec stack size).
  // The stack (if we consider this size) is misaligned now, as it's always
  // aligned before the function call - when `call()` is executed it pushes
  // the current EIP|RIP onto the stack, and misaligns it by 12 or 8 bytes
  // (depending on the architecture). So count number of bytes needed to align
  // it up to the function's CallFrame (the beginning).
  if (v || frame.hasFuncCalls())
    v += Support::alignUpDiff(v + frame.gpSaveSize() + registerSize, stackAlignment);

  frame._gpSaveOffset = v;                    // Store 'gpSaveOffset'     <- Function's GP Save/Restore starts here.
  frame._stackAdjustment = v;                 // Store 'stackAdjustment'  <- SA used by 'add zsp, SA' and 'sub zsp, SA'.

  v += frame._gpSaveSize;                     // Count 'gpSaveSize'       <- Function's GP Save/Restore ends here.
  v += registerSize;                          // Count 'ReturnAddress'    <- As CALL pushes onto stack.

  // If the function performs dynamic stack alignment then the stack-adjustment must be aligned.
  if (hasDA)
    frame._stackAdjustment = Support::alignUp(frame._stackAdjustment, stackAlignment);

  uint32_t saInvOff = FuncFrame::kTagInvalidOffset;
  uint32_t saTmpOff = registerSize + frame._gpSaveSize;

  // Calculate where the function arguments start relative to SP.
  frame._saOffsetFromSP = hasDA ? saInvOff : v;

  // Calculate where the function arguments start relative to FP or user-provided register.
  frame._saOffsetFromSA = hasFP ? registerSize * 2  // Return address + frame pointer.
                                : saTmpOff;         // Return address + all saved GP regs.

  return kErrorOk;
}

// ============================================================================
// [asmjit::X86Internal - ArgsToFrameInfo]
// ============================================================================

ASMJIT_FAVOR_SIZE Error X86Internal::argsToFuncFrame(const FuncArgsAssignment& args, FuncFrame& frame) noexcept {
  X86FuncArgsContext ctx;
  ASMJIT_PROPAGATE(ctx.initWorkData(frame, args));
  ASMJIT_PROPAGATE(ctx.markDstRegsDirty(frame));
  ASMJIT_PROPAGATE(ctx.markScratchRegs(frame));
  ASMJIT_PROPAGATE(ctx.markStackArgsReg(frame));
  return kErrorOk;
}

// ============================================================================
// [asmjit::X86Internal - Emit Helpers]
// ============================================================================

ASMJIT_FAVOR_SIZE Error X86Internal::emitRegMove(Emitter* emitter,
  const Operand_& dst_,
  const Operand_& src_, uint32_t typeId, bool avxEnabled, const char* comment) {

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(Type::isValid(typeId) && !Type::isAbstract(typeId));

  Operand dst(dst_);
  Operand src(src_);

  uint32_t instId = Inst::kIdNone;
  uint32_t memFlags = 0;
  uint32_t overrideMemSize = 0;

  enum MemFlags : uint32_t {
    kDstMem = 0x1,
    kSrcMem = 0x2
  };

  // Detect memory operands and patch them to have the same size as the register.
  // BaseCompiler always sets memory size of allocs and spills, so it shouldn't
  // be really necessary, however, after this function was separated from Compiler
  // it's better to make sure that the size is always specified, as we can use
  // 'movzx' and 'movsx' that rely on it.
  if (dst.isMem()) { memFlags |= kDstMem; dst.as<Mem>().setSize(src.size()); }
  if (src.isMem()) { memFlags |= kSrcMem; src.as<Mem>().setSize(dst.size()); }

  switch (typeId) {
    case Type::kIdI8:
    case Type::kIdU8:
    case Type::kIdI16:
    case Type::kIdU16:
      // Special case - 'movzx' load.
      if (memFlags & kSrcMem) {
        instId = Inst::kIdMovzx;
        dst.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
      }
      else if (!memFlags) {
        // Change both destination and source registers to GPD (safer, no dependencies).
        dst.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        src.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
      }
      ASMJIT_FALLTHROUGH;

    case Type::kIdI32:
    case Type::kIdU32:
    case Type::kIdI64:
    case Type::kIdU64:
      instId = Inst::kIdMov;
      break;

    case Type::kIdMmx32:
      instId = Inst::kIdMovd;
      if (memFlags) break;
      ASMJIT_FALLTHROUGH;

    case Type::kIdMmx64 : instId = Inst::kIdMovq ; break;
    case Type::kIdMask8 : instId = Inst::kIdKmovb; break;
    case Type::kIdMask16: instId = Inst::kIdKmovw; break;
    case Type::kIdMask32: instId = Inst::kIdKmovd; break;
    case Type::kIdMask64: instId = Inst::kIdKmovq; break;

    default: {
      uint32_t elementTypeId = Type::baseOf(typeId);
      if (Type::isVec32(typeId) && memFlags) {
        overrideMemSize = 4;
        if (elementTypeId == Type::kIdF32)
          instId = avxEnabled ? Inst::kIdVmovss : Inst::kIdMovss;
        else
          instId = avxEnabled ? Inst::kIdVmovd : Inst::kIdMovd;
        break;
      }

      if (Type::isVec64(typeId) && memFlags) {
        overrideMemSize = 8;
        if (elementTypeId == Type::kIdF64)
          instId = avxEnabled ? Inst::kIdVmovsd : Inst::kIdMovsd;
        else
          instId = avxEnabled ? Inst::kIdVmovq : Inst::kIdMovq;
        break;
      }

      if (elementTypeId == Type::kIdF32)
        instId = avxEnabled ? Inst::kIdVmovaps : Inst::kIdMovaps;
      else if (elementTypeId == Type::kIdF64)
        instId = avxEnabled ? Inst::kIdVmovapd : Inst::kIdMovapd;
      else if (typeId <= Type::_kIdVec256End)
        instId = avxEnabled ? Inst::kIdVmovdqa : Inst::kIdMovdqa;
      else if (elementTypeId <= Type::kIdU32)
        instId = Inst::kIdVmovdqa32;
      else
        instId = Inst::kIdVmovdqa64;
      break;
    }
  }

  if (!instId)
    return DebugUtils::errored(kErrorInvalidState);

  if (overrideMemSize) {
    if (dst.isMem()) dst.as<Mem>().setSize(overrideMemSize);
    if (src.isMem()) src.as<Mem>().setSize(overrideMemSize);
  }

  emitter->setInlineComment(comment);
  return emitter->emit(instId, dst, src);
}

ASMJIT_FAVOR_SIZE Error X86Internal::emitArgMove(Emitter* emitter,
  const Reg& dst_, uint32_t dstTypeId,
  const Operand_& src_, uint32_t srcTypeId, bool avxEnabled, const char* comment) {

  // Deduce optional `dstTypeId`, which may be `Type::kIdVoid` in some cases.
  if (!dstTypeId)
    dstTypeId = opData.archRegs.regTypeToTypeId[dst_.type()];

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(Type::isValid(dstTypeId) && !Type::isAbstract(dstTypeId));
  ASMJIT_ASSERT(Type::isValid(srcTypeId) && !Type::isAbstract(srcTypeId));

  Reg dst(dst_);
  Operand src(src_);

  uint32_t dstSize = Type::sizeOf(dstTypeId);
  uint32_t srcSize = Type::sizeOf(srcTypeId);

  uint32_t instId = Inst::kIdNone;

  // Not a real loop, just 'break' is nicer than 'goto'.
  for (;;) {
    if (Type::isInt(dstTypeId)) {
      if (Type::isInt(srcTypeId)) {
        instId = Inst::kIdMovsx;
        uint32_t typeOp = (dstTypeId << 8) | srcTypeId;

        // Sign extend by using 'movsx'.
        if (typeOp == ((Type::kIdI16 << 8) | Type::kIdI8 ) ||
            typeOp == ((Type::kIdI32 << 8) | Type::kIdI8 ) ||
            typeOp == ((Type::kIdI32 << 8) | Type::kIdI16) ||
            typeOp == ((Type::kIdI64 << 8) | Type::kIdI8 ) ||
            typeOp == ((Type::kIdI64 << 8) | Type::kIdI16))
          break;

        // Sign extend by using 'movsxd'.
        instId = Inst::kIdMovsxd;
        if (typeOp == ((Type::kIdI64 << 8) | Type::kIdI32))
          break;
      }

      if (Type::isInt(srcTypeId) || src_.isMem()) {
        // Zero extend by using 'movzx' or 'mov'.
        if (dstSize <= 4 && srcSize < 4) {
          instId = Inst::kIdMovzx;
          dst.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        }
        else {
          // We should have caught all possibilities where `srcSize` is less
          // than 4, so we don't have to worry about 'movzx' anymore. Minimum
          // size is enough to determine if we want 32-bit or 64-bit move.
          instId = Inst::kIdMov;
          srcSize = Support::min(srcSize, dstSize);

          dst.setSignature(srcSize == 4 ? Reg::signatureOfT<Reg::kTypeGpd>()
                                        : Reg::signatureOfT<Reg::kTypeGpq>());
          if (src.isReg())
            src.setSignature(dst.signature());
        }
        break;
      }

      // NOTE: The previous branch caught all memory sources, from here it's
      // always register to register conversion, so catch the remaining cases.
      srcSize = Support::min(srcSize, dstSize);

      if (Type::isMmx(srcTypeId)) {
        // 64-bit move.
        instId = Inst::kIdMovq;
        if (srcSize == 8)
          break;

        // 32-bit move.
        instId = Inst::kIdMovd;
        dst.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        break;
      }

      if (Type::isMask(srcTypeId)) {
        instId = x86KmovFromSize(srcSize);
        dst.setSignature(srcSize <= 4 ? Reg::signatureOfT<Reg::kTypeGpd>()
                                      : Reg::signatureOfT<Reg::kTypeGpq>());
        break;
      }

      if (Type::isVec(srcTypeId)) {
        // 64-bit move.
        instId = avxEnabled ? Inst::kIdVmovq : Inst::kIdMovq;
        if (srcSize == 8)
          break;

        // 32-bit move.
        instId = avxEnabled ? Inst::kIdVmovd : Inst::kIdMovd;
        dst.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        break;
      }
    }

    if (Type::isMmx(dstTypeId)) {
      instId = Inst::kIdMovq;
      srcSize = Support::min(srcSize, dstSize);

      if (Type::isInt(srcTypeId) || src.isMem()) {
        // 64-bit move.
        if (srcSize == 8)
          break;

        // 32-bit move.
        instId = Inst::kIdMovd;
        if (src.isReg())
          src.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        break;
      }

      if (Type::isMmx(srcTypeId))
        break;

      // This will hurt if `avxEnabled`.
      instId = Inst::kIdMovdq2q;
      if (Type::isVec(srcTypeId))
break;
    }

    if (Type::isMask(dstTypeId)) {
      srcSize = Support::min(srcSize, dstSize);

      if (Type::isInt(srcTypeId) || Type::isMask(srcTypeId) || src.isMem()) {
        instId = x86KmovFromSize(srcSize);
        if (Reg::isGp(src) && srcSize <= 4)
          src.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
        break;
      }
    }

    if (Type::isVec(dstTypeId)) {
      // By default set destination to XMM, will be set to YMM|ZMM if needed.
      dst.setSignature(Reg::signatureOfT<Reg::kTypeXmm>());

      // This will hurt if `avxEnabled`.
      if (Reg::isMm(src)) {
        // 64-bit move.
        instId = Inst::kIdMovq2dq;
        break;
      }

      // Argument conversion.
      uint32_t dstElement = Type::baseOf(dstTypeId);
      uint32_t srcElement = Type::baseOf(srcTypeId);

      if (dstElement == Type::kIdF32 && srcElement == Type::kIdF64) {
        srcSize = Support::min(dstSize * 2, srcSize);
        dstSize = srcSize / 2;

        if (srcSize <= 8)
          instId = avxEnabled ? Inst::kIdVcvtss2sd : Inst::kIdCvtss2sd;
        else
          instId = avxEnabled ? Inst::kIdVcvtps2pd : Inst::kIdCvtps2pd;

        if (dstSize == 32)
          dst.setSignature(Reg::signatureOfT<Reg::kTypeYmm>());
        if (src.isReg())
          src.setSignature(Reg::signatureOfVecBySize(srcSize));
        break;
      }

      if (dstElement == Type::kIdF64 && srcElement == Type::kIdF32) {
        srcSize = Support::min(dstSize, srcSize * 2) / 2;
        dstSize = srcSize * 2;

        if (srcSize <= 4)
          instId = avxEnabled ? Inst::kIdVcvtsd2ss : Inst::kIdCvtsd2ss;
        else
          instId = avxEnabled ? Inst::kIdVcvtpd2ps : Inst::kIdCvtpd2ps;

        dst.setSignature(Reg::signatureOfVecBySize(dstSize));
        if (src.isReg() && srcSize >= 32)
          src.setSignature(Reg::signatureOfT<Reg::kTypeYmm>());
        break;
      }

      srcSize = Support::min(srcSize, dstSize);
      if (Reg::isGp(src) || src.isMem()) {
        // 32-bit move.
        if (srcSize <= 4) {
          instId = avxEnabled ? Inst::kIdVmovd : Inst::kIdMovd;
          if (src.isReg())
            src.setSignature(Reg::signatureOfT<Reg::kTypeGpd>());
          break;
        }

        // 64-bit move.
        if (srcSize == 8) {
          instId = avxEnabled ? Inst::kIdVmovq : Inst::kIdMovq;
          break;
        }
      }

      if (Reg::isVec(src) || src.isMem()) {
        instId = avxEnabled ? Inst::kIdVmovaps : Inst::kIdMovaps;

        if (src.isMem() && srcSize < emitter->environment().stackAlignment())
          instId = avxEnabled ? Inst::kIdVmovups : Inst::kIdMovups;

        uint32_t signature = Reg::signatureOfVecBySize(srcSize);
        dst.setSignature(signature);
        if (src.isReg())
          src.setSignature(signature);
        break;
      }
    }

    return DebugUtils::errored(kErrorInvalidState);
  }

  if (src.isMem())
    src.as<Mem>().setSize(srcSize);

  emitter->setInlineComment(comment);
  return emitter->emit(instId, dst, src);
}

// ============================================================================
// [asmjit::X86Internal - Emit Prolog & Epilog]
// ============================================================================

static ASMJIT_INLINE void X86Internal_setupSaveRestoreInfo(uint32_t group, const FuncFrame& frame, Reg& xReg, uint32_t& xInst, uint32_t& xSize) noexcept {
  switch (group) {
    case Reg::kGroupVec:
      xReg = xmm(0);
      xInst = x86GetXmmMovInst(frame);
      xSize = xReg.size();
      break;
    case Reg::kGroupMm:
      xReg = mm(0);
      xInst = Inst::kIdMovq;
      xSize = xReg.size();
      break;
    case Reg::kGroupKReg:
      xReg = k(0);
      xInst = Inst::kIdKmovq;
      xSize = xReg.size();
      break;
  }
}

ASMJIT_FAVOR_SIZE Error X86Internal::emitProlog(Emitter* emitter, const FuncFrame& frame) {
  uint32_t gpSaved = frame.savedRegs(Reg::kGroupGp);

  Gp zsp = emitter->zsp();   // ESP|RSP register.
  Gp zbp = emitter->zbp();   // EBP|RBP register.
  Gp gpReg = zsp;            // General purpose register (temporary).
  Gp saReg = zsp;            // Stack-arguments base pointer.

  // Emit: 'push zbp'
  //       'mov  zbp, zsp'.
  if (frame.hasPreservedFP()) {
    gpSaved &= ~Support::bitMask(Gp::kIdBp);
    ASMJIT_PROPAGATE(emitter->push(zbp));
    ASMJIT_PROPAGATE(emitter->mov(zbp, zsp));
  }

  // Emit: 'push gp' sequence.
  {
    Support::BitWordIterator<uint32_t> it(gpSaved);
    while (it.hasNext()) {
      gpReg.setId(it.next());
      ASMJIT_PROPAGATE(emitter->push(gpReg));
    }
  }

  // Emit: 'mov saReg, zsp'.
  uint32_t saRegId = frame.saRegId();
  if (saRegId != BaseReg::kIdBad && saRegId != Gp::kIdSp) {
    saReg.setId(saRegId);
    if (frame.hasPreservedFP()) {
      if (saRegId != Gp::kIdBp)
        ASMJIT_PROPAGATE(emitter->mov(saReg, zbp));
    }
    else {
      ASMJIT_PROPAGATE(emitter->mov(saReg, zsp));
    }
  }

  // Emit: 'and zsp, StackAlignment'.
  if (frame.hasDynamicAlignment()) {
    ASMJIT_PROPAGATE(emitter->and_(zsp, -int32_t(frame.finalStackAlignment())));
  }

  // Emit: 'sub zsp, StackAdjustment'.
  if (frame.hasStackAdjustment()) {
    ASMJIT_PROPAGATE(emitter->sub(zsp, frame.stackAdjustment()));
  }

  // Emit: 'mov [zsp + DAOffset], saReg'.
  if (frame.hasDynamicAlignment() && frame.hasDAOffset()) {
    Mem saMem = ptr(zsp, int32_t(frame.daOffset()));
    ASMJIT_PROPAGATE(emitter->mov(saMem, saReg));
  }

  // Emit 'movxxx [zsp + X], {[x|y|z]mm, k}'.
  {
    Reg xReg;
    Mem xBase = ptr(zsp, int32_t(frame.nonGpSaveOffset()));

    uint32_t xInst;
    uint32_t xSize;

    for (uint32_t group = 1; group < BaseReg::kGroupVirt; group++) {
      Support::BitWordIterator<uint32_t> it(frame.savedRegs(group));
      if (it.hasNext()) {
        X86Internal_setupSaveRestoreInfo(group, frame, xReg, xInst, xSize);
        do {
          xReg.setId(it.next());
          ASMJIT_PROPAGATE(emitter->emit(xInst, xBase, xReg));
          xBase.addOffsetLo32(int32_t(xSize));
        } while (it.hasNext());
      }
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error X86Internal::emitEpilog(Emitter* emitter, const FuncFrame& frame) {
  uint32_t i;
  uint32_t regId;

  uint32_t registerSize = emitter->registerSize();
  uint32_t gpSaved = frame.savedRegs(Reg::kGroupGp);

  Gp zsp = emitter->zsp();   // ESP|RSP register.
  Gp zbp = emitter->zbp();   // EBP|RBP register.
  Gp gpReg = emitter->zsp(); // General purpose register (temporary).

  // Don't emit 'pop zbp' in the pop sequence, this case is handled separately.
  if (frame.hasPreservedFP())
    gpSaved &= ~Support::bitMask(Gp::kIdBp);

  // Emit 'movxxx {[x|y|z]mm, k}, [zsp + X]'.
  {
    Reg xReg;
    Mem xBase = ptr(zsp, int32_t(frame.nonGpSaveOffset()));

    uint32_t xInst;
    uint32_t xSize;

    for (uint32_t group = 1; group < BaseReg::kGroupVirt; group++) {
      Support::BitWordIterator<uint32_t> it(frame.savedRegs(group));
      if (it.hasNext()) {
        X86Internal_setupSaveRestoreInfo(group, frame, xReg, xInst, xSize);
        do {
          xReg.setId(it.next());
          ASMJIT_PROPAGATE(emitter->emit(xInst, xReg, xBase));
          xBase.addOffsetLo32(int32_t(xSize));
        } while (it.hasNext());
      }
    }
  }

  // Emit 'emms' and/or 'vzeroupper'.
  if (frame.hasMmxCleanup()) ASMJIT_PROPAGATE(emitter->emms());
  if (frame.hasAvxCleanup()) ASMJIT_PROPAGATE(emitter->vzeroupper());

  if (frame.hasPreservedFP()) {
    // Emit 'mov zsp, zbp' or 'lea zsp, [zbp - x]'
    int32_t count = int32_t(frame.gpSaveSize() - registerSize);
    if (!count)
      ASMJIT_PROPAGATE(emitter->mov(zsp, zbp));
    else
      ASMJIT_PROPAGATE(emitter->lea(zsp, ptr(zbp, -count)));
  }
  else {
    if (frame.hasDynamicAlignment() && frame.hasDAOffset()) {
      // Emit 'mov zsp, [zsp + DsaSlot]'.
      Mem saMem = ptr(zsp, int32_t(frame.daOffset()));
      ASMJIT_PROPAGATE(emitter->mov(zsp, saMem));
    }
    else if (frame.hasStackAdjustment()) {
      // Emit 'add zsp, StackAdjustment'.
      ASMJIT_PROPAGATE(emitter->add(zsp, int32_t(frame.stackAdjustment())));
    }
  }

  // Emit 'pop gp' sequence.
  if (gpSaved) {
    i = gpSaved;
    regId = 16;

    do {
      regId--;
      if (i & 0x8000) {
        gpReg.setId(regId);
        ASMJIT_PROPAGATE(emitter->pop(gpReg));
      }
      i <<= 1;
    } while (regId != 0);
  }

  // Emit 'pop zbp'.
  if (frame.hasPreservedFP())
    ASMJIT_PROPAGATE(emitter->pop(zbp));

  // Emit 'ret' or 'ret x'.
  if (frame.hasCalleeStackCleanup())
    ASMJIT_PROPAGATE(emitter->emit(Inst::kIdRet, int(frame.calleeStackCleanup())));
  else
    ASMJIT_PROPAGATE(emitter->emit(Inst::kIdRet));

  return kErrorOk;
}

// ============================================================================
// [asmjit::X86Internal - Emit Arguments Assignment]
// ============================================================================

#ifdef ASMJIT_DUMP_ARGS_ASSIGNMENT
static void dumpFuncValue(String& sb, uint32_t arch, const FuncValue& value) noexcept {
  Formatter::formatTypeId(sb, value.typeId());
  sb.append('@');

  if (value.isIndirect())
    sb.append('[');

  if (value.isReg())
    Formatter::formatRegister(sb, 0, nullptr, arch, value.regType(), value.regId());
  else if (value.isStack())
    sb.appendFormat("[%d]", value.stackOffset());
  else
    sb.append("<none>");

  if (value.isIndirect())
    sb.append(']');
}

static void dumpAssignment(String& sb, const X86FuncArgsContext& ctx) noexcept {
  typedef X86FuncArgsContext::Var Var;

  uint32_t arch = ctx.arch();
  uint32_t varCount = ctx.varCount();

  for (uint32_t i = 0; i < varCount; i++) {
    const Var& var = ctx.var(i);
    const FuncValue& dst = var.out;
    const FuncValue& cur = var.cur;

    sb.appendFormat("Var%u: ", i);
    dumpFuncValue(sb, arch, dst);
    sb.append(" <- ");
    dumpFuncValue(sb, arch, cur);

    if (var.isDone())
      sb.append(" {Done}");

    sb.append('\n');
  }
}
#endif

ASMJIT_FAVOR_SIZE Error X86Internal::emitArgsAssignment(Emitter* emitter, const FuncFrame& frame, const FuncArgsAssignment& args) {
  typedef X86FuncArgsContext::Var Var;
  typedef X86FuncArgsContext::WorkData WorkData;

  enum WorkFlags : uint32_t {
    kWorkNone      = 0x00,
    kWorkDidSome   = 0x01,
    kWorkPending   = 0x02,
    kWorkPostponed = 0x04
  };

  X86FuncArgsContext ctx;
  ASMJIT_PROPAGATE(ctx.initWorkData(frame, args));

#ifdef ASMJIT_DUMP_ARGS_ASSIGNMENT
  {
    String sb;
    dumpAssignment(sb, ctx);
    printf("%s\n", sb.data());
  }
#endif

  uint32_t arch = ctx.arch();
  uint32_t varCount = ctx._varCount;
  WorkData* workData = ctx._workData;

  // Use AVX if it's enabled.
  bool avxEnabled = frame.isAvxEnabled();

  uint32_t saVarId = ctx._saVarId;
  uint32_t saRegId = Gp::kIdSp;

  if (frame.hasDynamicAlignment()) {
    if (frame.hasPreservedFP())
      saRegId = Gp::kIdBp;
    else
      saRegId = saVarId < varCount ? ctx._vars[saVarId].cur.regId() : frame.saRegId();
  }

  RegInfo gpRegInfo = emitter->_gpRegInfo;

  // --------------------------------------------------------------------------
  // Register to stack and stack to stack moves must be first as now we have
  // the biggest chance of having as many as possible unassigned registers.
  // --------------------------------------------------------------------------

  if (ctx._stackDstMask) {
    // Base address of all arguments passed by stack.
    Mem baseArgPtr = ptr(emitter->gpz(saRegId), int32_t(frame.saOffset(saRegId)));
    Mem baseStackPtr = ptr(emitter->gpz(Gp::kIdSp), int32_t(0));

    for (uint32_t varId = 0; varId < varCount; varId++) {
      Var& var = ctx._vars[varId];

      if (!var.out.isStack())
        continue;

      FuncValue& cur = var.cur;
      FuncValue& out = var.out;

      ASMJIT_ASSERT(cur.isReg() || cur.isStack());
      Reg reg;

      Mem dstStackPtr = baseStackPtr.cloneAdjusted(out.stackOffset());
      Mem srcStackPtr = baseArgPtr.cloneAdjusted(cur.stackOffset());

      if (cur.isIndirect()) {
        if (cur.isStack()) {
          // TODO: Indirect stack.
          return DebugUtils::errored(kErrorInvalidAssignment);
        }
        else {
          srcStackPtr = ptr(Gp(gpRegInfo.signature(), cur.regId()));
        }
      }

      if (cur.isReg() && !cur.isIndirect()) {
        WorkData& wd = workData[Reg::groupOf(cur.regType())];
        uint32_t rId = cur.regId();

        reg.setSignatureAndId(Reg::signatureOf(cur.regType()), rId);
        wd.unassign(varId, rId);
      }
      else {
        // Stack to reg move - tricky since we move stack to stack we can decide which
        // register to use. In general we follow the rule that IntToInt moves will use
        // GP regs with possibility to signature or zero extend, and all other moves will
        // either use GP or VEC regs depending on the size of the move.
        RegInfo rInfo = x86GetRegForMemToMemMove(arch, out.typeId(), cur.typeId());
        if (ASMJIT_UNLIKELY(!rInfo.isValid()))
          return DebugUtils::errored(kErrorInvalidState);

        WorkData& wd = workData[rInfo.group()];
        uint32_t availableRegs = wd.availableRegs();
        if (ASMJIT_UNLIKELY(!availableRegs))
          return DebugUtils::errored(kErrorInvalidState);

        uint32_t rId = Support::ctz(availableRegs);
        reg.setSignatureAndId(rInfo.signature(), rId);

        ASMJIT_PROPAGATE(emitArgMove(emitter, reg, out.typeId(), srcStackPtr, cur.typeId(), avxEnabled));
      }

      if (cur.isIndirect() && cur.isReg())
        workData[BaseReg::kGroupGp].unassign(varId, cur.regId());

      // Register to stack move.
      ASMJIT_PROPAGATE(emitRegMove(emitter, dstStackPtr, reg, cur.typeId(), avxEnabled));
      var.markDone();
    }
  }

  // --------------------------------------------------------------------------
  // Shuffle all registers that are currently assigned accordingly to target
  // assignment.
  // --------------------------------------------------------------------------

  uint32_t workFlags = kWorkNone;
  for (;;) {
    for (uint32_t varId = 0; varId < varCount; varId++) {
      Var& var = ctx._vars[varId];
      if (var.isDone() || !var.cur.isReg())
        continue;

      FuncValue& cur = var.cur;
      FuncValue& out = var.out;

      uint32_t curGroup = Reg::groupOf(cur.regType());
      uint32_t outGroup = Reg::groupOf(out.regType());

      uint32_t curId = cur.regId();
      uint32_t outId = out.regId();

      if (curGroup != outGroup) {
        // TODO: Conversion is not supported.
        return DebugUtils::errored(kErrorInvalidAssignment);
      }
      else {
        WorkData& wd = workData[outGroup];
        if (!wd.isAssigned(outId)) {
EmitMove:
          ASMJIT_PROPAGATE(
            emitArgMove(emitter,
              Reg::fromTypeAndId(out.regType(), outId), out.typeId(),
              Reg::fromTypeAndId(cur.regType(), curId), cur.typeId(), avxEnabled));

          wd.reassign(varId, outId, curId);
          cur.initReg(out.regType(), outId, out.typeId());

          if (outId == out.regId())
            var.markDone();
          workFlags |= kWorkDidSome | kWorkPending;
        }
        else {
          uint32_t altId = wd._physToVarId[outId];
          Var& altVar = ctx._vars[altId];

          if (!altVar.out.isInitialized() || (altVar.out.isReg() && altVar.out.regId() == curId)) {
            // Swap operation is possible only between two GP registers.
            if (curGroup == Reg::kGroupGp) {
              uint32_t highestType = Support::max(cur.regType(), altVar.cur.regType());
              uint32_t signature = highestType == Reg::kTypeGpq ? Reg::signatureOfT<Reg::kTypeGpq>()
                                                                : Reg::signatureOfT<Reg::kTypeGpd>();

              ASMJIT_PROPAGATE(emitter->emit(Inst::kIdXchg, Reg(signature, outId), Reg(signature, curId)));
              wd.swap(varId, curId, altId, outId);
              cur.setRegId(outId);
              var.markDone();
              altVar.cur.setRegId(curId);

              if (altVar.out.isInitialized())
                altVar.markDone();
              workFlags |= kWorkDidSome;
            }
            else {
              // If there is a scratch register it can be used to perform the swap.
              uint32_t availableRegs = wd.availableRegs();
              if (availableRegs) {
                uint32_t inOutRegs = wd.dstRegs();
                if (availableRegs & ~inOutRegs)
                  availableRegs &= ~inOutRegs;
                outId = Support::ctz(availableRegs);
                goto EmitMove;
              }
              else {
                workFlags |= kWorkPending;
              }
            }
          }
          else {
            workFlags |= kWorkPending;
          }
        }
      }
    }

    if (!(workFlags & kWorkPending))
      break;

    // If we did nothing twice it means that something is really broken.
    if ((workFlags & (kWorkDidSome | kWorkPostponed)) == kWorkPostponed)
      return DebugUtils::errored(kErrorInvalidState);

    workFlags = (workFlags & kWorkDidSome) ? kWorkNone : kWorkPostponed;
  }

  // --------------------------------------------------------------------------
  // Load arguments passed by stack into registers. This is pretty simple and
  // it never requires multiple iterations like the previous phase.
  // --------------------------------------------------------------------------

  if (ctx._hasStackSrc) {
    uint32_t iterCount = 1;
    if (frame.hasDynamicAlignment() && !frame.hasPreservedFP())
      saRegId = saVarId < varCount ? ctx._vars[saVarId].cur.regId() : frame.saRegId();

    // Base address of all arguments passed by stack.
    Mem baseArgPtr = ptr(emitter->gpz(saRegId), int32_t(frame.saOffset(saRegId)));

    for (uint32_t iter = 0; iter < iterCount; iter++) {
      for (uint32_t varId = 0; varId < varCount; varId++) {
        Var& var = ctx._vars[varId];
        if (var.isDone())
          continue;

        if (var.cur.isStack()) {
          ASMJIT_ASSERT(var.out.isReg());

          uint32_t outId = var.out.regId();
          uint32_t outType = var.out.regType();

          uint32_t group = Reg::groupOf(outType);
          WorkData& wd = ctx._workData[group];

          if (outId == saRegId && group == BaseReg::kGroupGp) {
            // This register will be processed last as we still need `saRegId`.
            if (iterCount == 1) {
              iterCount++;
              continue;
            }
            wd.unassign(wd._physToVarId[outId], outId);
          }

          Reg dstReg = Reg::fromTypeAndId(outType, outId);
          Mem srcMem = baseArgPtr.cloneAdjusted(var.cur.stackOffset());

          ASMJIT_PROPAGATE(
            emitArgMove(emitter,
              dstReg, var.out.typeId(),
              srcMem, var.cur.typeId(), avxEnabled));

          wd.assign(varId, outId);
          var.cur.initReg(outType, outId, var.cur.typeId(), FuncValue::kFlagIsDone);
        }
      }
    }
  }

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_BUILD_X86
