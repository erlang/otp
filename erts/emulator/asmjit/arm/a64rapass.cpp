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
#if !defined(ASMJIT_NO_ARM) && !defined(ASMJIT_NO_COMPILER)

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

// ============================================================================
// [asmjit::a64::ARMRAPass - Helpers]
// ============================================================================

// TODO: [ARM] These should be shared with all backends.

static ASMJIT_INLINE uint64_t raImmMaskFromSize(uint32_t size) noexcept {
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

static ASMJIT_INLINE uint32_t raUseOutFlagsFromRWFlags(uint32_t rwFlags) noexcept {
  static const uint32_t map[] = {
    0,
    RATiedReg::kRead  | RATiedReg::kUse, // kRead
    RATiedReg::kWrite | RATiedReg::kOut, // kWrite
    RATiedReg::kRW    | RATiedReg::kUse, // kRW
  };

  return map[rwFlags & OpRWInfo::kRW];
}

static ASMJIT_INLINE uint32_t raRegRwFlags(uint32_t flags) noexcept {
  return raUseOutFlagsFromRWFlags(flags);
}

static ASMJIT_INLINE uint32_t raMemBaseRwFlags(uint32_t flags) noexcept {
  constexpr uint32_t shift = Support::constCtz(OpRWInfo::kMemBaseRW);
  return raUseOutFlagsFromRWFlags((flags >> shift) & OpRWInfo::kRW);
}

static ASMJIT_INLINE uint32_t raMemIndexRwFlags(uint32_t flags) noexcept {
  constexpr uint32_t shift = Support::constCtz(OpRWInfo::kMemIndexRW);
  return raUseOutFlagsFromRWFlags((flags >> shift) & OpRWInfo::kRW);
}
// ============================================================================
// [asmjit::a64::RACFGBuilder]
// ============================================================================

class RACFGBuilder : public RACFGBuilderT<RACFGBuilder> {
public:
  uint32_t _arch;

  inline RACFGBuilder(ARMRAPass* pass) noexcept
    : RACFGBuilderT<RACFGBuilder>(pass),
      _arch(pass->cc()->arch()) {}

  inline Compiler* cc() const noexcept { return static_cast<Compiler*>(_cc); }

  Error onInst(InstNode* inst, uint32_t& controlType, RAInstBuilder& ib) noexcept;

  Error onBeforeInvoke(InvokeNode* invokeNode) noexcept;
  Error onInvoke(InvokeNode* invokeNode, RAInstBuilder& ib) noexcept;

  Error moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept;
  Error moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept;
  Error moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept;

  Error onBeforeRet(FuncRetNode* funcRet) noexcept;
  Error onRet(FuncRetNode* funcRet, RAInstBuilder& ib) noexcept;
};

// ============================================================================
// [asmjit::a64::RACFGBuilder - OnInst]
// ============================================================================

// TODO: [ARM] This is just a workaround...
static uint32_t getControlType(uint32_t instId, uint32_t options) noexcept {
  switch (instId) {
    case Inst::kIdB:
    case Inst::kIdBr:
      if (options & Inst::kOptionCondFlagMask)
        return BaseInst::kControlBranch;
      else
        return BaseInst::kControlJump;
    case Inst::kIdBl:
    case Inst::kIdBlr:
      return BaseInst::kControlCall;
    case Inst::kIdCbz:
    case Inst::kIdCbnz:
    case Inst::kIdTbz:
    case Inst::kIdTbnz:
      return BaseInst::kControlBranch;
    case Inst::kIdRet:
      return BaseInst::kControlReturn;
    default:
      return BaseInst::kControlNone;
  }
}

Error RACFGBuilder::onInst(InstNode* inst, uint32_t& controlType, RAInstBuilder& ib) noexcept {
  InstRWInfo rwInfo;

  uint32_t instId = inst->id();
  if (Inst::isDefinedId(instId)) {
    uint32_t opCount = inst->opCount();
    const Operand* opArray = inst->operands();
    ASMJIT_PROPAGATE(InstInternal::queryRWInfo(_arch, inst->baseInst(), opArray, opCount, &rwInfo));

    const InstDB::InstInfo& instInfo = InstDB::infoById(instId);
    uint32_t singleRegOps = 0;

    if (opCount) {
      for (uint32_t i = 0; i < opCount; i++) {
        const Operand& op = opArray[i];
        const OpRWInfo& opRwInfo = rwInfo.operand(i);

        if (op.isReg()) {
          // Register Operand
          // ----------------
          const Reg& reg = op.as<Reg>();

          uint32_t flags = raRegRwFlags(opRwInfo.opFlags());
          uint32_t vIndex = Operand::virtIdToIndex(reg.id());

          if (vIndex < Operand::kVirtIdCount) {
            RAWorkReg* workReg;
            ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

            // Use RW instead of Write in case that not the whole register is
            // overwritten. This is important for liveness as we cannot kill a
            // register that will be used.
            if ((flags & RATiedReg::kRW) == RATiedReg::kWrite) {
              if (workReg->regByteMask() & ~(opRwInfo.writeByteMask() | opRwInfo.extendByteMask())) {
                // Not write-only operation.
                flags = (flags & ~RATiedReg::kOut) | (RATiedReg::kRead | RATiedReg::kUse);
              }
            }

            uint32_t group = workReg->group();
            uint32_t allocable = _pass->_availableRegs[group];

            uint32_t useId = BaseReg::kIdBad;
            uint32_t outId = BaseReg::kIdBad;

            uint32_t useRewriteMask = 0;
            uint32_t outRewriteMask = 0;

            if (flags & RATiedReg::kUse) {
              useRewriteMask = Support::bitMask(inst->getRewriteIndex(&reg._baseId));
              if (opRwInfo.opFlags() & OpRWInfo::kRegPhysId) {
                useId = opRwInfo.physId();
                flags |= RATiedReg::kUseFixed;
              }
            }
            else {
              outRewriteMask = Support::bitMask(inst->getRewriteIndex(&reg._baseId));
              if (opRwInfo.opFlags() & OpRWInfo::kRegPhysId) {
                outId = opRwInfo.physId();
                flags |= RATiedReg::kOutFixed;
              }
            }

            // Special cases regarding element access.
            if (reg.as<Vec>().hasElementIndex()) {
              // Only the first 0..15 registers can be used if the register uses
              // element accessor that accesses half-words (h[0..7] elements).
              if (instInfo.hasFlag(InstDB::kInstFlagVH0_15) && reg.as<Vec>().elementType() == Vec::kElementTypeH) {
                allocable &= 0xF;
              }
            }

            ASMJIT_PROPAGATE(ib.add(workReg, flags, allocable, useId, useRewriteMask, outId, outRewriteMask, opRwInfo.rmSize()));
            if (singleRegOps == i)
              singleRegOps++;
          }
        }
        else if (op.isMem()) {
          // Memory Operand
          // --------------
          const Mem& mem = op.as<Mem>();
          ib.addForbiddenFlags(RATiedReg::kUseRM | RATiedReg::kOutRM);

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

              uint32_t flags = raMemBaseRwFlags(opRwInfo.opFlags());
              uint32_t group = workReg->group();
              uint32_t allocable = _pass->_availableRegs[group];

              uint32_t useId = BaseReg::kIdBad;
              uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (flags & RATiedReg::kUse)
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));
              else
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));

              ASMJIT_PROPAGATE(ib.add(workReg, flags, allocable, useId, useRewriteMask, outId, outRewriteMask));
            }
          }

          if (mem.hasIndexReg()) {
            uint32_t vIndex = Operand::virtIdToIndex(mem.indexId());
            if (vIndex < Operand::kVirtIdCount) {
              RAWorkReg* workReg;
              ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

              uint32_t flags = raMemIndexRwFlags(opRwInfo.opFlags());
              uint32_t group = workReg->group();
              uint32_t allocable = _pass->_availableRegs[group];

              // Index registers have never fixed id on X86/x64.
              const uint32_t useId = BaseReg::kIdBad;
              const uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (flags & RATiedReg::kUse)
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));
              else
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));

              ASMJIT_PROPAGATE(ib.add(workReg, RATiedReg::kUse | RATiedReg::kRead, allocable, useId, useRewriteMask, outId, outRewriteMask));
            }
          }
        }
      }
    }

    // controlType = instInfo.controlType();
    controlType = getControlType(instId, inst->instOptions());
  }

  return kErrorOk;
}

// ============================================================================
// [asmjit::a64::RACFGBuilder - OnInvoke]
// ============================================================================

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
          uint32_t regGroup = workReg->group();
          uint32_t argGroup = Reg::groupOf(arg.regType());

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
          uint32_t regGroup = workReg->group();
          uint32_t retGroup = Reg::groupOf(ret.regType());

          if (regGroup != retGroup) {
            // TODO: [ARM] Conversion is not supported.
            return DebugUtils::errored(kErrorInvalidAssignment);
          }
        }
      }
    }
  }

  // This block has function call(s).
  _curBlock->addFlags(RABlock::kFlagHasFuncCalls);
  _pass->func()->frame().addAttributes(FuncFrame::kAttrHasFuncCalls);
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
          uint32_t regGroup = workReg->group();
          if (regGroup != BaseReg::kGroupGp)
            return DebugUtils::errored(kErrorInvalidState);
          ASMJIT_PROPAGATE(ib.addCallArg(workReg, arg.regId()));
        }
        else if (arg.isReg()) {
          uint32_t regGroup = workReg->group();
          uint32_t argGroup = Reg::groupOf(arg.regType());

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
        uint32_t regGroup = workReg->group();
        uint32_t retGroup = Reg::groupOf(ret.regType());

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
  ib._clobbered[0] = Support::lsbMask<uint32_t>(_pass->_physRegCount[0]) & ~fd.preservedRegs(0);
  ib._clobbered[1] = Support::lsbMask<uint32_t>(_pass->_physRegCount[1]) & ~fd.preservedRegs(1);
  ib._clobbered[2] = Support::lsbMask<uint32_t>(_pass->_physRegCount[2]) & ~fd.preservedRegs(2);
  ib._clobbered[3] = Support::lsbMask<uint32_t>(_pass->_physRegCount[3]) & ~fd.preservedRegs(3);

  return kErrorOk;
}

// ============================================================================
// [asmjit::a64::RACFGBuilder - MoveImmToRegArg]
// ============================================================================

Error RACFGBuilder::moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isReg());

  Imm imm(imm_);
  uint32_t rTypeId = Type::kIdVoid;

  switch (arg.typeId()) {
    case Type::kIdI8 : rTypeId = Type::kIdU64; imm.signExtend8Bits(); break;
    case Type::kIdU8 : rTypeId = Type::kIdU64; imm.zeroExtend8Bits(); break;
    case Type::kIdI16: rTypeId = Type::kIdU64; imm.signExtend16Bits(); break;
    case Type::kIdU16: rTypeId = Type::kIdU64; imm.zeroExtend16Bits(); break;
    case Type::kIdI32: rTypeId = Type::kIdU64; imm.signExtend32Bits(); break;
    case Type::kIdU32: rTypeId = Type::kIdU64; imm.zeroExtend32Bits(); break;
    case Type::kIdI64: rTypeId = Type::kIdU64; break;
    case Type::kIdU64: rTypeId = Type::kIdU64; break;

    default:
      return DebugUtils::errored(kErrorInvalidAssignment);
  }

  ASMJIT_PROPAGATE(cc()->_newReg(out, rTypeId, nullptr));
  cc()->virtRegById(out->id())->setWeight(BaseRAPass::kCallArgWeight);
  return cc()->mov(out->as<Gp>(), imm);
}

// ============================================================================
// [asmjit::a64::RACFGBuilder - MoveImmToStackArg]
// ============================================================================

Error RACFGBuilder::moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept {
  BaseReg reg;

  ASMJIT_PROPAGATE(moveImmToRegArg(invokeNode, arg, imm_, &reg));
  ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, reg));

  return kErrorOk;
}

// ============================================================================
// [asmjit::a64::RACFGBuilder - MoveRegToStackArg]
// ============================================================================

Error RACFGBuilder::moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept {
  Mem stackPtr = ptr(_pass->_sp.as<Gp>(), arg.stackOffset());

  if (reg.isGp())
    return cc()->str(reg.as<Gp>(), stackPtr);

  if (reg.isVec())
    return cc()->str(reg.as<Vec>(), stackPtr);

  return DebugUtils::errored(kErrorInvalidState);
}

// ============================================================================
// [asmjit::a64::RACFGBuilder - OnReg]
// ============================================================================

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

        uint32_t group = workReg->group();
        uint32_t allocable = _pass->_availableRegs[group];
        ASMJIT_PROPAGATE(ib.add(workReg, RATiedReg::kUse | RATiedReg::kRead, allocable, ret.regId(), 0, BaseReg::kIdBad, 0));
      }
    }
    else {
      return DebugUtils::errored(kErrorInvalidAssignment);
    }
  }

  return kErrorOk;
}

// ============================================================================
// [asmjit::a64::ARMRAPass - Construction / Destruction]
// ============================================================================

ARMRAPass::ARMRAPass() noexcept
  : BaseRAPass() { _iEmitHelper = &_emitHelper; }
ARMRAPass::~ARMRAPass() noexcept {}

// ============================================================================
// [asmjit::a64::ARMRAPass - OnInit / OnDone]
// ============================================================================

void ARMRAPass::onInit() noexcept {
  uint32_t arch = cc()->arch();

  _emitHelper._emitter = _cb;

  _archTraits = &ArchTraits::byArch(arch);
  _physRegCount.set(Reg::kGroupGp    , 32);
  _physRegCount.set(Reg::kGroupVec   , 32);
  _physRegCount.set(Reg::kGroupOther0, 0);
  _physRegCount.set(Reg::kGroupOther1, 0);
  _buildPhysIndex();

  _availableRegCount = _physRegCount;
  _availableRegs[Reg::kGroupGp    ] = Support::lsbMask<uint32_t>(_physRegCount.get(Reg::kGroupGp));
  _availableRegs[Reg::kGroupVec   ] = Support::lsbMask<uint32_t>(_physRegCount.get(Reg::kGroupVec));
  _availableRegs[Reg::kGroupOther0] = Support::lsbMask<uint32_t>(_physRegCount.get(Reg::kGroupOther0));
  _availableRegs[Reg::kGroupOther1] = Support::lsbMask<uint32_t>(_physRegCount.get(Reg::kGroupOther1));

  _scratchRegIndexes[0] = uint8_t(27);
  _scratchRegIndexes[1] = uint8_t(28);

  // The architecture specific setup makes implicitly all registers available. So
  // make unavailable all registers that are special and cannot be used in general.
  bool hasFP = _func->frame().hasPreservedFP();

  if (hasFP)
    makeUnavailable(Reg::kGroupGp, Gp::kIdFp);

  makeUnavailable(Reg::kGroupGp, Gp::kIdSp);
  makeUnavailable(Reg::kGroupGp, Gp::kIdOs); // OS-specific use, usually TLS.

  _sp = sp;
  _fp = x29;
}

void ARMRAPass::onDone() noexcept {}

// ============================================================================
// [asmjit::a64::ARMRAPass - BuildCFG]
// ============================================================================

Error ARMRAPass::buildCFG() noexcept {
  return RACFGBuilder(this).run();
}

// ============================================================================
// [asmjit::a64::ARMRAPass - Rewrite]
// ============================================================================

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

        if (ASMJIT_UNLIKELY(node->type() != BaseNode::kNodeInst)) {
          // FuncRet terminates the flow, it must either be removed if the exit
          // label is next to it (optimization) or patched to an architecture
          // dependent jump instruction that jumps to the function's exit before
          // the epilog.
          if (node->type() == BaseNode::kNodeFuncRet) {
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
    }

    node = next;
  }

  return kErrorOk;
}

// ============================================================================
// [asmjit::a64::ARMRAPass - Prolog / Epilog]
// ============================================================================

Error ARMRAPass::updateStackFrame() noexcept {
  if (_func->frame().hasFuncCalls())
    _func->frame().addDirtyRegs(Reg::kGroupGp, Support::bitMask(Gp::kIdLr));

  return BaseRAPass::updateStackFrame();
}

// ============================================================================
// [asmjit::a64::ARMRAPass - OnEmit]
// ============================================================================

Error ARMRAPass::emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
  RAWorkReg* wReg = workRegById(workId);
  BaseReg dst = BaseReg::fromSignatureAndId(wReg->info().signature(), dstPhysId);
  BaseReg src = BaseReg::fromSignatureAndId(wReg->info().signature(), srcPhysId);

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (_loggerFlags & FormatOptions::kFlagAnnotations) {
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
  BaseReg dstReg = BaseReg::fromSignatureAndId(wReg->info().signature(), dstPhysId);
  BaseMem srcMem = BaseMem(workRegAsMem(wReg));

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (_loggerFlags & FormatOptions::kFlagAnnotations) {
    _tmpString.assignFormat("<LOAD> %s", workRegById(workId)->name());
    comment = _tmpString.data();
  }
#endif

  return _emitHelper.emitRegMove(dstReg, srcMem, wReg->typeId(), comment);
}

Error ARMRAPass::emitSave(uint32_t workId, uint32_t srcPhysId) noexcept {
  RAWorkReg* wReg = workRegById(workId);
  BaseMem dstMem = BaseMem(workRegAsMem(wReg));
  BaseReg srcReg = BaseReg::fromSignatureAndId(wReg->info().signature(), srcPhysId);

  const char* comment = nullptr;

#ifndef ASMJIT_NO_LOGGING
  if (_loggerFlags & FormatOptions::kFlagAnnotations) {
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

#endif // !ASMJIT_NO_ARM && !ASMJIT_NO_COMPILER
