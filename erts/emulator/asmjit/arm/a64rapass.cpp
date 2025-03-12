// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_COMPILER)

#include "../core/cpuinfo.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../arm/a64assembler.h"
#include "../arm/a64compiler.h"
#include "../arm/a64emithelper_p.h"
#include "../arm/a64instapi_p.h"
#include "../arm/a64instdb_p.h"
#include "../arm/a64rapass_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::ARMRAPass - Helpers
// ========================

// TODO: [ARM] These should be shared with all backends.
ASMJIT_MAYBE_UNUSED
static inline uint64_t raImmMaskFromSize(uint32_t size) noexcept {
  ASMJIT_ASSERT(size > 0 && size < 256);
  static const uint64_t masks[] = {
    0x00000000000000FFu, //   1
    0x000000000000FFFFu, //   2
    0x00000000FFFFFFFFu, //   4
    0xFFFFFFFFFFFFFFFFu, //   8
    0x0000000000000000u, //  16
    0x0000000000000000u, //  32
    0x0000000000000000u, //  64
    0x0000000000000000u, // 128
    0x0000000000000000u  // 256
  };
  return masks[Support::ctz(size)];
}

static const RegMask raConsecutiveLeadCountToRegMaskFilter[5] = {
  0xFFFFFFFFu, // [0] No consecutive.
  0x00000000u, // [1] Invalid, never used.
  0x7FFFFFFFu, // [2] 2 consecutive registers.
  0x3FFFFFFFu, // [3] 3 consecutive registers.
  0x1FFFFFFFu  // [4] 4 consecutive registers.
};

static inline RATiedFlags raUseOutFlagsFromRWFlags(OpRWFlags rwFlags) noexcept {
  static constexpr RATiedFlags map[] = {
    RATiedFlags::kNone,
    RATiedFlags::kRead  | RATiedFlags::kUse, // kRead
    RATiedFlags::kWrite | RATiedFlags::kOut, // kWrite
    RATiedFlags::kRW    | RATiedFlags::kUse, // kRW
  };

  return map[uint32_t(rwFlags & OpRWFlags::kRW)];
}

static inline RATiedFlags raRegRwFlags(OpRWFlags flags) noexcept {
  return raUseOutFlagsFromRWFlags(flags);
}

static inline RATiedFlags raMemBaseRwFlags(OpRWFlags flags) noexcept {
  constexpr uint32_t shift = Support::ConstCTZ<uint32_t(OpRWFlags::kMemBaseRW)>::value;
  return raUseOutFlagsFromRWFlags(OpRWFlags(uint32_t(flags) >> shift) & OpRWFlags::kRW);
}

static inline RATiedFlags raMemIndexRwFlags(OpRWFlags flags) noexcept {
  constexpr uint32_t shift = Support::ConstCTZ<uint32_t(OpRWFlags::kMemIndexRW)>::value;
  return raUseOutFlagsFromRWFlags(OpRWFlags(uint32_t(flags) >> shift) & OpRWFlags::kRW);
}
// a64::RACFGBuilder
// =================

class RACFGBuilder : public RACFGBuilderT<RACFGBuilder> {
public:
  Arch _arch;

  inline RACFGBuilder(ARMRAPass* pass) noexcept
    : RACFGBuilderT<RACFGBuilder>(pass),
      _arch(pass->cc()->arch()) {}

  inline Compiler* cc() const noexcept { return static_cast<Compiler*>(_cc); }

  Error onInst(InstNode* inst, InstControlFlow& controlType, RAInstBuilder& ib) noexcept;

  Error onBeforeInvoke(InvokeNode* invokeNode) noexcept;
  Error onInvoke(InvokeNode* invokeNode, RAInstBuilder& ib) noexcept;

  Error moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept;
  Error moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept;
  Error moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept;

  Error onBeforeRet(FuncRetNode* funcRet) noexcept;
  Error onRet(FuncRetNode* funcRet, RAInstBuilder& ib) noexcept;
};

// a64::RACFGBuilder - OnInst
// ==========================

// TODO: [ARM] This is just a workaround...
static InstControlFlow getControlFlowType(InstId instId) noexcept {
  switch (BaseInst::extractRealId(instId)) {
    case Inst::kIdB:
    case Inst::kIdBr:
      if (BaseInst::extractARMCondCode(instId) == CondCode::kAL)
        return InstControlFlow::kJump;
      else
        return InstControlFlow::kBranch;
    case Inst::kIdBl:
    case Inst::kIdBlr:
      return InstControlFlow::kCall;
    case Inst::kIdCbz:
    case Inst::kIdCbnz:
    case Inst::kIdTbz:
    case Inst::kIdTbnz:
      return InstControlFlow::kBranch;
    case Inst::kIdRet:
      return InstControlFlow::kReturn;
    default:
      return InstControlFlow::kRegular;
  }
}

Error RACFGBuilder::onInst(InstNode* inst, InstControlFlow& controlType, RAInstBuilder& ib) noexcept {
  InstRWInfo rwInfo;

  if (Inst::isDefinedId(inst->realId())) {
    InstId instId = inst->id();
    uint32_t opCount = inst->opCount();
    const Operand* opArray = inst->operands();
    ASMJIT_PROPAGATE(InstInternal::queryRWInfo(inst->baseInst(), opArray, opCount, &rwInfo));

    const InstDB::InstInfo& instInfo = InstDB::infoById(instId);
    uint32_t singleRegOps = 0;

    ib.addInstRWFlags(rwInfo.instFlags());

    if (opCount) {
      uint32_t consecutiveOffset = 0xFFFFFFFFu;
      uint32_t consecutiveParent = Globals::kInvalidId;

      for (uint32_t i = 0; i < opCount; i++) {
        const Operand& op = opArray[i];
        const OpRWInfo& opRwInfo = rwInfo.operand(i);

        if (op.isReg()) {
          // Register Operand
          // ----------------
          const Reg& reg = op.as<Reg>();

          RATiedFlags flags = raRegRwFlags(opRwInfo.opFlags());
          uint32_t vIndex = Operand::virtIdToIndex(reg.id());

          if (vIndex < Operand::kVirtIdCount) {
            RAWorkReg* workReg;
            ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

            // Use RW instead of Write in case that not the whole register is overwritten. This is important for
            // liveness as we cannot kill a register that will be used.
            if ((flags & RATiedFlags::kRW) == RATiedFlags::kWrite) {
              if (workReg->regByteMask() & ~(opRwInfo.writeByteMask() | opRwInfo.extendByteMask())) {
                // Not write-only operation.
                flags = (flags & ~RATiedFlags::kOut) | (RATiedFlags::kRead | RATiedFlags::kUse);
              }
            }

            RegGroup group = workReg->group();

            RegMask useRegs = _pass->_availableRegs[group];
            RegMask outRegs = useRegs;

            uint32_t useId = BaseReg::kIdBad;
            uint32_t outId = BaseReg::kIdBad;

            uint32_t useRewriteMask = 0;
            uint32_t outRewriteMask = 0;

            if (opRwInfo.consecutiveLeadCount()) {
              // There must be a single consecutive register lead, otherwise the RW data is invalid.
              if (consecutiveOffset != 0xFFFFFFFFu)
                return DebugUtils::errored(kErrorInvalidState);

              // A consecutive lead register cannot be used as a consecutive +1/+2/+3 register, the registers must be distinct.
              if (RATiedReg::consecutiveDataFromFlags(flags) != 0)
                return DebugUtils::errored(kErrorNotConsecutiveRegs);

              flags |= RATiedFlags::kLeadConsecutive | RATiedReg::consecutiveDataToFlags(opRwInfo.consecutiveLeadCount() - 1);
              consecutiveOffset = 0;

              RegMask filter = raConsecutiveLeadCountToRegMaskFilter[opRwInfo.consecutiveLeadCount()];
              if (Support::test(flags, RATiedFlags::kUse)) {
                flags |= RATiedFlags::kUseConsecutive;
                useRegs &= filter;
              }
              else {
                flags |= RATiedFlags::kOutConsecutive;
                outRegs &= filter;
              }
            }

            if (Support::test(flags, RATiedFlags::kUse)) {
              useRewriteMask = Support::bitMask(inst->getRewriteIndex(&reg._baseId));
              if (opRwInfo.hasOpFlag(OpRWFlags::kRegPhysId)) {
                useId = opRwInfo.physId();
                flags |= RATiedFlags::kUseFixed;
              }
              else if (opRwInfo.hasOpFlag(OpRWFlags::kConsecutive)) {
                if (consecutiveOffset == 0xFFFFFFFFu)
                  return DebugUtils::errored(kErrorInvalidState);
                flags |= RATiedFlags::kUseConsecutive | RATiedReg::consecutiveDataToFlags(++consecutiveOffset);
              }
            }
            else {
              outRewriteMask = Support::bitMask(inst->getRewriteIndex(&reg._baseId));
              if (opRwInfo.hasOpFlag(OpRWFlags::kRegPhysId)) {
                outId = opRwInfo.physId();
                flags |= RATiedFlags::kOutFixed;
              }
              else if (opRwInfo.hasOpFlag(OpRWFlags::kConsecutive)) {
                if (consecutiveOffset == 0xFFFFFFFFu)
                  return DebugUtils::errored(kErrorInvalidState);
                flags |= RATiedFlags::kOutConsecutive | RATiedReg::consecutiveDataToFlags(++consecutiveOffset);
              }
            }

            // Special cases regarding element access.
            if (reg.as<Vec>().hasElementIndex()) {
              // Only the first 0..15 registers can be used if the register uses
              // element accessor that accesses half-words (h[0..7] elements).
              if (instInfo.hasFlag(InstDB::kInstFlagVH0_15) && reg.as<Vec>().elementType() == VecElementType::kH) {
                if (Support::test(flags, RATiedFlags::kUse))
                  useId &= 0x0000FFFFu;
                else
                  outId &= 0x0000FFFFu;
              }
            }

            ASMJIT_PROPAGATE(ib.add(workReg, flags, useRegs, useId, useRewriteMask, outRegs, outId, outRewriteMask, opRwInfo.rmSize(), consecutiveParent));
            if (singleRegOps == i)
              singleRegOps++;

            if (Support::test(flags, RATiedFlags::kLeadConsecutive | RATiedFlags::kUseConsecutive | RATiedFlags::kOutConsecutive))
              consecutiveParent = workReg->workId();
          }
        }
        else if (op.isMem()) {
          // Memory Operand
          // --------------
          const Mem& mem = op.as<Mem>();

          if (mem.isRegHome()) {
            RAWorkReg* workReg;
            ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(Operand::virtIdToIndex(mem.baseId()), &workReg));
            _pass->getOrCreateStackSlot(workReg);
          }
          else if (mem.hasBaseReg()) {
            uint32_t vIndex = Operand::virtIdToIndex(mem.baseId());
            if (vIndex < Operand::kVirtIdCount) {
              RAWorkReg* workReg;
              ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

              RATiedFlags flags = raMemBaseRwFlags(opRwInfo.opFlags());
              RegGroup group = workReg->group();
              RegMask allocable = _pass->_availableRegs[group];

              // Base registers have never fixed id on ARM.
              const uint32_t useId = BaseReg::kIdBad;
              const uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (Support::test(flags, RATiedFlags::kUse))
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));
              else
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));

              ASMJIT_PROPAGATE(ib.add(workReg, flags, allocable, useId, useRewriteMask, allocable, outId, outRewriteMask));
            }
          }

          if (mem.hasIndexReg()) {
            uint32_t vIndex = Operand::virtIdToIndex(mem.indexId());
            if (vIndex < Operand::kVirtIdCount) {
              RAWorkReg* workReg;
              ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

              RATiedFlags flags = raMemIndexRwFlags(opRwInfo.opFlags());
              RegGroup group = workReg->group();
              RegMask allocable = _pass->_availableRegs[group];

              // Index registers have never fixed id on ARM.
              const uint32_t useId = BaseReg::kIdBad;
              const uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (Support::test(flags, RATiedFlags::kUse))
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));
              else
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));

              ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRead, allocable, useId, useRewriteMask, allocable, outId, outRewriteMask));
            }
          }
        }
      }
    }

    controlType = getControlFlowType(instId);
  }

  return kErrorOk;
}

// a64::RACFGBuilder - OnInvoke
// ============================

Error RACFGBuilder::onBeforeInvoke(InvokeNode* invokeNode) noexcept {
  const FuncDetail& fd = invokeNode->detail();
  uint32_t argCount = invokeNode->argCount();

  cc()->_setCursor(invokeNode->prev());

  for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
    const FuncValuePack& argPack = fd.argPack(argIndex);
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      if (!argPack[valueIndex])
        break;

      const FuncValue& arg = argPack[valueIndex];
      const Operand& op = invokeNode->arg(argIndex, valueIndex);

      if (op.isNone())
        continue;

      if (op.isReg()) {
        const Reg& reg = op.as<Reg>();
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(Operand::virtIdToIndex(reg.id()), &workReg));

        if (arg.isReg()) {
          RegGroup regGroup = workReg->group();
          RegGroup argGroup = Reg::groupOf(arg.regType());

          if (regGroup != argGroup) {
            // TODO: [ARM] Conversion is not supported.
            return DebugUtils::errored(kErrorInvalidAssignment);
          }
        }
        else {
          ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, reg));
        }
      }
      else if (op.isImm()) {
        if (arg.isReg()) {
          BaseReg reg;
          ASMJIT_PROPAGATE(moveImmToRegArg(invokeNode, arg, op.as<Imm>(), &reg));
          invokeNode->_args[argIndex][valueIndex] = reg;
        }
        else {
          ASMJIT_PROPAGATE(moveImmToStackArg(invokeNode, arg, op.as<Imm>()));
        }
      }
    }
  }

  cc()->_setCursor(invokeNode);

  if (fd.hasRet()) {
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      const FuncValue& ret = fd.ret(valueIndex);
      if (!ret)
        break;

      const Operand& op = invokeNode->ret(valueIndex);
      if (op.isReg()) {
        const Reg& reg = op.as<Reg>();
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(Operand::virtIdToIndex(reg.id()), &workReg));

        if (ret.isReg()) {
          RegGroup regGroup = workReg->group();
          RegGroup retGroup = Reg::groupOf(ret.regType());

          if (regGroup != retGroup) {
            // TODO: [ARM] Conversion is not supported.
            return DebugUtils::errored(kErrorInvalidAssignment);
          }
        }
      }
    }
  }

  // This block has function call(s).
  _curBlock->addFlags(RABlockFlags::kHasFuncCalls);
  _pass->func()->frame().addAttributes(FuncAttributes::kHasFuncCalls);
  _pass->func()->frame().updateCallStackSize(fd.argStackSize());

  return kErrorOk;
}

Error RACFGBuilder::onInvoke(InvokeNode* invokeNode, RAInstBuilder& ib) noexcept {
  uint32_t argCount = invokeNode->argCount();
  const FuncDetail& fd = invokeNode->detail();

  for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
    const FuncValuePack& argPack = fd.argPack(argIndex);
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      if (!argPack[valueIndex])
        continue;

      const FuncValue& arg = argPack[valueIndex];
      const Operand& op = invokeNode->arg(argIndex, valueIndex);

      if (op.isNone())
        continue;

      if (op.isReg()) {
        const Reg& reg = op.as<Reg>();
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(Operand::virtIdToIndex(reg.id()), &workReg));

        if (arg.isIndirect()) {
          RegGroup regGroup = workReg->group();
          if (regGroup != RegGroup::kGp)
            return DebugUtils::errored(kErrorInvalidState);
          ASMJIT_PROPAGATE(ib.addCallArg(workReg, arg.regId()));
        }
        else if (arg.isReg()) {
          RegGroup regGroup = workReg->group();
          RegGroup argGroup = Reg::groupOf(arg.regType());

          if (regGroup == argGroup) {
            ASMJIT_PROPAGATE(ib.addCallArg(workReg, arg.regId()));
          }
        }
      }
    }
  }

  for (uint32_t retIndex = 0; retIndex < Globals::kMaxValuePack; retIndex++) {
    const FuncValue& ret = fd.ret(retIndex);
    if (!ret)
      break;

    const Operand& op = invokeNode->ret(retIndex);
    if (op.isReg()) {
      const Reg& reg = op.as<Reg>();
      RAWorkReg* workReg;
      ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(Operand::virtIdToIndex(reg.id()), &workReg));

      if (ret.isReg()) {
        RegGroup regGroup = workReg->group();
        RegGroup retGroup = Reg::groupOf(ret.regType());

        if (regGroup == retGroup) {
          ASMJIT_PROPAGATE(ib.addCallRet(workReg, ret.regId()));
        }
      }
      else {
        return DebugUtils::errored(kErrorInvalidAssignment);
      }
    }
  }

  // Setup clobbered registers.
  ib._clobbered[0] = Support::lsbMask<RegMask>(_pass->_physRegCount[RegGroup(0)]) & ~fd.preservedRegs(RegGroup(0));
  ib._clobbered[1] = Support::lsbMask<RegMask>(_pass->_physRegCount[RegGroup(1)]) & ~fd.preservedRegs(RegGroup(1));
  ib._clobbered[2] = Support::lsbMask<RegMask>(_pass->_physRegCount[RegGroup(2)]) & ~fd.preservedRegs(RegGroup(2));
  ib._clobbered[3] = Support::lsbMask<RegMask>(_pass->_physRegCount[RegGroup(3)]) & ~fd.preservedRegs(RegGroup(3));

  return kErrorOk;
}

// a64::RACFGBuilder - MoveImmToRegArg
// ===================================

Error RACFGBuilder::moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isReg());

  Imm imm(imm_);
  TypeId typeId = TypeId::kVoid;

  switch (arg.typeId()) {
    case TypeId::kInt8  : typeId = TypeId::kUInt64; imm.signExtend8Bits(); break;
    case TypeId::kUInt8 : typeId = TypeId::kUInt64; imm.zeroExtend8Bits(); break;
    case TypeId::kInt16 : typeId = TypeId::kUInt64; imm.signExtend16Bits(); break;
    case TypeId::kUInt16: typeId = TypeId::kUInt64; imm.zeroExtend16Bits(); break;
    case TypeId::kInt32 : typeId = TypeId::kUInt64; imm.signExtend32Bits(); break;
    case TypeId::kUInt32: typeId = TypeId::kUInt64; imm.zeroExtend32Bits(); break;
    case TypeId::kInt64 : typeId = TypeId::kUInt64; break;
    case TypeId::kUInt64: typeId = TypeId::kUInt64; break;

    default:
      return DebugUtils::errored(kErrorInvalidAssignment);
  }

  ASMJIT_PROPAGATE(cc()->_newReg(out, typeId, nullptr));
  cc()->virtRegById(out->id())->setWeight(BaseRAPass::kCallArgWeight);
  return cc()->mov(out->as<Gp>(), imm);
}

// a64::RACFGBuilder - MoveImmToStackArg
// =====================================

Error RACFGBuilder::moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept {
  BaseReg reg;

  ASMJIT_PROPAGATE(moveImmToRegArg(invokeNode, arg, imm_, &reg));
  ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, reg));

  return kErrorOk;
}

// a64::RACFGBuilder - MoveRegToStackArg
// =====================================

Error RACFGBuilder::moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept {
  DebugUtils::unused(invokeNode);
  Mem stackPtr = ptr(_pass->_sp.as<Gp>(), arg.stackOffset());

  if (reg.isGp())
    return cc()->str(reg.as<Gp>(), stackPtr);

  if (reg.isVec())
    return cc()->str(reg.as<Vec>(), stackPtr);

  return DebugUtils::errored(kErrorInvalidState);
}

// a64::RACFGBuilder - OnReg
// =========================

Error RACFGBuilder::onBeforeRet(FuncRetNode* funcRet) noexcept {
  DebugUtils::unused(funcRet);
  return kErrorOk;
}

Error RACFGBuilder::onRet(FuncRetNode* funcRet, RAInstBuilder& ib) noexcept {
  const FuncDetail& funcDetail = _pass->func()->detail();
  const Operand* opArray = funcRet->operands();
  uint32_t opCount = funcRet->opCount();

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand& op = opArray[i];
    if (op.isNone()) continue;

    const FuncValue& ret = funcDetail.ret(i);
    if (ASMJIT_UNLIKELY(!ret.isReg()))
      return DebugUtils::errored(kErrorInvalidAssignment);

    if (op.isReg()) {
      // Register return value.
      const Reg& reg = op.as<Reg>();
      uint32_t vIndex = Operand::virtIdToIndex(reg.id());

      if (vIndex < Operand::kVirtIdCount) {
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

        RegGroup group = workReg->group();
        RegMask allocable = _pass->_availableRegs[group];
        ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRead, allocable, ret.regId(), 0, 0, BaseReg::kIdBad, 0));
      }
    }
    else {
      return DebugUtils::errored(kErrorInvalidAssignment);
    }
  }

  return kErrorOk;
}

// a64::ARMRAPass - Construction & Destruction
// ===========================================

ARMRAPass::ARMRAPass() noexcept
  : BaseRAPass() { _iEmitHelper = &_emitHelper; }
ARMRAPass::~ARMRAPass() noexcept {}

// a64::ARMRAPass - OnInit / OnDone
// ================================

void ARMRAPass::onInit() noexcept {
  Arch arch = cc()->arch();

  _emitHelper._emitter = _cb;

  _archTraits = &ArchTraits::byArch(arch);
  _physRegCount.set(RegGroup::kGp, 32);
  _physRegCount.set(RegGroup::kVec, 32);
  _physRegCount.set(RegGroup::kMask, 0);
  _physRegCount.set(RegGroup::kExtraVirt3, 0);
  _buildPhysIndex();

  _availableRegCount = _physRegCount;
  _availableRegs[RegGroup::kGp] = Support::lsbMask<uint32_t>(_physRegCount.get(RegGroup::kGp));
  _availableRegs[RegGroup::kVec] = Support::lsbMask<uint32_t>(_physRegCount.get(RegGroup::kVec));
  _availableRegs[RegGroup::kMask] = Support::lsbMask<uint32_t>(_physRegCount.get(RegGroup::kMask));
  _availableRegs[RegGroup::kExtraVirt3] = Support::lsbMask<uint32_t>(_physRegCount.get(RegGroup::kExtraVirt3));

  _scratchRegIndexes[0] = uint8_t(27);
  _scratchRegIndexes[1] = uint8_t(28);

  // The architecture specific setup makes implicitly all registers available. So
  // make unavailable all registers that are special and cannot be used in general.
  bool hasFP = _func->frame().hasPreservedFP();

  // Apple ABI requires that the frame-pointer register is not changed by leaf functions and properly updated
  // by non-leaf functions. So, let's make this register unavailable as it's just not safe to update it.
  if (hasFP || cc()->environment().isDarwin())
    makeUnavailable(RegGroup::kGp, Gp::kIdFp);

  makeUnavailable(RegGroup::kGp, Gp::kIdSp);
  makeUnavailable(RegGroup::kGp, Gp::kIdOs); // OS-specific use, usually TLS.

  _sp = sp;
  _fp = x29;
}

void ARMRAPass::onDone() noexcept {}

// a64::ARMRAPass - BuildCFG
// =========================

Error ARMRAPass::buildCFG() noexcept {
  return RACFGBuilder(this).run();
}

// a64::ARMRAPass - Rewrite
// ========================

ASMJIT_FAVOR_SPEED Error ARMRAPass::_rewrite(BaseNode* first, BaseNode* stop) noexcept {
  uint32_t virtCount = cc()->_vRegArray.size();

  BaseNode* node = first;
  while (node != stop) {
    BaseNode* next = node->next();
    if (node->isInst()) {
      InstNode* inst = node->as<InstNode>();
      RAInst* raInst = node->passData<RAInst>();

      Operand* operands = inst->operands();
      uint32_t opCount = inst->opCount();

      uint32_t i;

      // Rewrite virtual registers into physical registers.
      if (raInst) {
        // If the instruction contains pass data (raInst) then it was a subject
        // for register allocation and must be rewritten to use physical regs.
        RATiedReg* tiedRegs = raInst->tiedRegs();
        uint32_t tiedCount = raInst->tiedCount();

        for (i = 0; i < tiedCount; i++) {
          RATiedReg* tiedReg = &tiedRegs[i];

          Support::BitWordIterator<uint32_t> useIt(tiedReg->useRewriteMask());
          uint32_t useId = tiedReg->useId();
          while (useIt.hasNext())
            inst->rewriteIdAtIndex(useIt.next(), useId);

          Support::BitWordIterator<uint32_t> outIt(tiedReg->outRewriteMask());
          uint32_t outId = tiedReg->outId();
          while (outIt.hasNext())
            inst->rewriteIdAtIndex(outIt.next(), outId);
        }

        // This data is allocated by Zone passed to `runOnFunction()`, which
        // will be reset after the RA pass finishes. So reset this data to
        // prevent having a dead pointer after the RA pass is complete.
        node->resetPassData();

        if (ASMJIT_UNLIKELY(node->type() != NodeType::kInst)) {
          // FuncRet terminates the flow, it must either be removed if the exit
          // label is next to it (optimization) or patched to an architecture
          // dependent jump instruction that jumps to the function's exit before
          // the epilog.
          if (node->type() == NodeType::kFuncRet) {
            RABlock* block = raInst->block();
            if (!isNextTo(node, _func->exitNode())) {
              cc()->_setCursor(node->prev());
              ASMJIT_PROPAGATE(emitJump(_func->exitNode()->label()));
            }

            BaseNode* prev = node->prev();
            cc()->removeNode(node);
            block->setLast(prev);
          }
        }
      }

      // Rewrite stack slot addresses.
      for (i = 0; i < opCount; i++) {
        Operand& op = operands[i];
        if (op.isMem()) {
          BaseMem& mem = op.as<BaseMem>();
          if (mem.isRegHome()) {
            uint32_t virtIndex = Operand::virtIdToIndex(mem.baseId());
            if (ASMJIT_UNLIKELY(virtIndex >= virtCount))
              return DebugUtils::errored(kErrorInvalidVirtId);

            VirtReg* virtReg = cc()->virtRegByIndex(virtIndex);
            RAWorkReg* workReg = virtReg->workReg();
            ASMJIT_ASSERT(workReg != nullptr);

            RAStackSlot* slot = workReg->stackSlot();
            int32_t offset = slot->offset();

            mem._setBase(_sp.type(), slot->baseRegId());
            mem.clearRegHome();
            mem.addOffsetLo32(offset);
          }
        }
      }

      // Rewrite `loadAddressOf()` construct.
      if (inst->realId() == Inst::kIdAdr && inst->opCount() == 2 && inst->op(1).isMem()) {
        BaseMem mem = inst->op(1).as<BaseMem>();
        int64_t offset = mem.offset();

        if (!mem.hasBaseOrIndex()) {
          inst->setId(Inst::kIdMov);
          inst->setOp(1, Imm(offset));
        }
        else {
          if (mem.hasIndex())
            return DebugUtils::errored(kErrorInvalidAddressIndex);

          GpX dst(inst->op(0).as<Gp>().id());
          GpX base(mem.baseId());

          InstId arithInstId = offset < 0 ? Inst::kIdSub : Inst::kIdAdd;
          uint64_t absOffset = offset < 0 ? Support::neg(uint64_t(offset)) : uint64_t(offset);

          inst->setId(arithInstId);
          inst->setOpCount(3);
          inst->setOp(1, base);
          inst->setOp(2, Imm(absOffset));

          // Use two operations if the offset cannot be encoded with ADD/SUB.
          if (absOffset > 0xFFFu && (absOffset & ~uint64_t(0xFFF000u)) != 0) {
            if (absOffset <= 0xFFFFFFu) {
              cc()->_setCursor(inst->prev());
              ASMJIT_PROPAGATE(cc()->emit(arithInstId, dst, base, Imm(absOffset & 0xFFFu)));

              inst->setOp(1, dst);
              inst->setOp(2, Imm(absOffset & 0xFFF000u));
            }
            else {
              cc()->_setCursor(inst->prev());
              ASMJIT_PROPAGATE(cc()->emit(Inst::kIdMov, inst->op(0), Imm(absOffset)));

              inst->setOp(1, base);
              inst->setOp(2, dst);
            }
          }
        }
      }
    }

    node = next;
  }

  return kErrorOk;
}

// a64::ARMRAPass - Prolog & Epilog
// ================================

Error ARMRAPass::updateStackFrame() noexcept {
  if (_func->frame().hasFuncCalls())
    _func->frame().addDirtyRegs(RegGroup::kGp, Support::bitMask(Gp::kIdLr));

  return BaseRAPass::updateStackFrame();
}

// a64::ARMRAPass - OnEmit
// =======================

Error ARMRAPass::emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
  RAWorkReg* wReg = workRegById(workId);
  BaseReg dst(wReg->signature(), dstPhysId);
  BaseReg src(wReg->signature(), srcPhysId);

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (hasDiagnosticOption(DiagnosticOptions::kRAAnnotate)) {
    _tmpString.assignFormat("<MOVE> %s", workRegById(workId)->name());
    comment = _tmpString.data();
  }
#endif

  return _emitHelper.emitRegMove(dst, src, wReg->typeId(), comment);
}

Error ARMRAPass::emitSwap(uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept {
  DebugUtils::unused(aWorkId, aPhysId, bWorkId, bPhysId);
  return DebugUtils::errored(kErrorInvalidState);
}

Error ARMRAPass::emitLoad(uint32_t workId, uint32_t dstPhysId) noexcept {
  RAWorkReg* wReg = workRegById(workId);
  BaseReg dstReg(wReg->signature(), dstPhysId);
  BaseMem srcMem(workRegAsMem(wReg));

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (hasDiagnosticOption(DiagnosticOptions::kRAAnnotate)) {
    _tmpString.assignFormat("<LOAD> %s", workRegById(workId)->name());
    comment = _tmpString.data();
  }
#endif

  return _emitHelper.emitRegMove(dstReg, srcMem, wReg->typeId(), comment);
}

Error ARMRAPass::emitSave(uint32_t workId, uint32_t srcPhysId) noexcept {
  RAWorkReg* wReg = workRegById(workId);
  BaseMem dstMem(workRegAsMem(wReg));
  BaseReg srcReg(wReg->signature(), srcPhysId);

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (hasDiagnosticOption(DiagnosticOptions::kRAAnnotate)) {
    _tmpString.assignFormat("<SAVE> %s", workRegById(workId)->name());
    comment = _tmpString.data();
  }
#endif

  return _emitHelper.emitRegMove(dstMem, srcReg, wReg->typeId(), comment);
}

Error ARMRAPass::emitJump(const Label& label) noexcept {
  return cc()->b(label);
}

Error ARMRAPass::emitPreCall(InvokeNode* invokeNode) noexcept {
  DebugUtils::unused(invokeNode);
  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64 && !ASMJIT_NO_COMPILER
