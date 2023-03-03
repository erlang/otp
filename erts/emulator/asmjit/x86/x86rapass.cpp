// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_X86) && !defined(ASMJIT_NO_COMPILER)

#include "../core/cpuinfo.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../x86/x86assembler.h"
#include "../x86/x86compiler.h"
#include "../x86/x86instapi_p.h"
#include "../x86/x86instdb_p.h"
#include "../x86/x86emithelper_p.h"
#include "../x86/x86rapass_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

// x86::X86RAPass - Utilities
// ==========================

static ASMJIT_FORCE_INLINE uint64_t raImmMaskFromSize(uint32_t size) noexcept {
  ASMJIT_ASSERT(size > 0 && size < 256);
  static constexpr uint64_t masks[] = {
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
  0x55555555u, // [2] Even registers.
  0x00000000u, // [3] Invalid, never used.
  0x11111111u  // [4] Every fourth register.
};

static ASMJIT_FORCE_INLINE RATiedFlags raUseOutFlagsFromRWFlags(OpRWFlags rwFlags) noexcept {
  static constexpr RATiedFlags map[] = {
    RATiedFlags::kNone,
    RATiedFlags::kRead  | RATiedFlags::kUse,                       // kRead
    RATiedFlags::kWrite | RATiedFlags::kOut,                       // kWrite
    RATiedFlags::kRW    | RATiedFlags::kUse,                       // kRW
    RATiedFlags::kNone,
    RATiedFlags::kRead  | RATiedFlags::kUse | RATiedFlags::kUseRM, // kRead  | kRegMem
    RATiedFlags::kWrite | RATiedFlags::kOut | RATiedFlags::kOutRM, // kWrite | kRegMem
    RATiedFlags::kRW    | RATiedFlags::kUse | RATiedFlags::kUseRM  // kRW    | kRegMem
  };

  return map[uint32_t(rwFlags & (OpRWFlags::kRW | OpRWFlags::kRegMem))];
}

static ASMJIT_FORCE_INLINE RATiedFlags raRegRwFlags(OpRWFlags flags) noexcept {
  return (RATiedFlags)raUseOutFlagsFromRWFlags(flags);
}

static ASMJIT_FORCE_INLINE RATiedFlags raMemBaseRwFlags(OpRWFlags flags) noexcept {
  constexpr uint32_t kShift = Support::ConstCTZ<uint32_t(OpRWFlags::kMemBaseRW)>::value;
  return (RATiedFlags)raUseOutFlagsFromRWFlags(OpRWFlags(uint32_t(flags) >> kShift) & OpRWFlags::kRW);
}

static ASMJIT_FORCE_INLINE RATiedFlags raMemIndexRwFlags(OpRWFlags flags) noexcept {
  constexpr uint32_t kShift = Support::ConstCTZ<uint32_t(OpRWFlags::kMemIndexRW)>::value;
  return (RATiedFlags)raUseOutFlagsFromRWFlags(OpRWFlags(uint32_t(flags) >> kShift) & OpRWFlags::kRW);
}

// x86::RACFGBuilder
// =================

class RACFGBuilder : public RACFGBuilderT<RACFGBuilder> {
public:
  Arch _arch;
  bool _is64Bit;
  bool _avxEnabled;

  inline RACFGBuilder(X86RAPass* pass) noexcept
    : RACFGBuilderT<RACFGBuilder>(pass),
      _arch(pass->cc()->arch()),
      _is64Bit(pass->registerSize() == 8),
      _avxEnabled(pass->avxEnabled()) {
  }

  inline Compiler* cc() const noexcept { return static_cast<Compiler*>(_cc); }

  inline uint32_t choose(uint32_t sseInst, uint32_t avxInst) const noexcept {
    return _avxEnabled ? avxInst : sseInst;
  }

  Error onInst(InstNode* inst, InstControlFlow& cf, RAInstBuilder& ib) noexcept;

  Error onBeforeInvoke(InvokeNode* invokeNode) noexcept;
  Error onInvoke(InvokeNode* invokeNode, RAInstBuilder& ib) noexcept;

  Error moveVecToPtr(InvokeNode* invokeNode, const FuncValue& arg, const Vec& src, BaseReg* out) noexcept;
  Error moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept;
  Error moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept;
  Error moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept;

  Error onBeforeRet(FuncRetNode* funcRet) noexcept;
  Error onRet(FuncRetNode* funcRet, RAInstBuilder& ib) noexcept;
};

// x86::RACFGBuilder - OnInst
// ==========================

Error RACFGBuilder::onInst(InstNode* inst, InstControlFlow& cf, RAInstBuilder& ib) noexcept {
  InstRWInfo rwInfo;

  InstId instId = inst->id();
  if (Inst::isDefinedId(instId)) {
    uint32_t opCount = inst->opCount();
    const Operand* opArray = inst->operands();
    ASMJIT_PROPAGATE(InstInternal::queryRWInfo(_arch, inst->baseInst(), opArray, opCount, &rwInfo));

    const InstDB::InstInfo& instInfo = InstDB::infoById(instId);
    bool hasGpbHiConstraint = false;
    uint32_t singleRegOps = 0;

    // Copy instruction RW flags to instruction builder except kMovOp, which is propagated manually later.
    ib.addInstRWFlags(rwInfo.instFlags() & ~InstRWFlags::kMovOp);

    // Mask of all operand types used by the instruction - can be used as an optimization later.
    uint32_t opTypesMask = 0u;

    if (opCount) {
      // The mask is for all registers, but we are mostly interested in AVX-512 registers at the moment. The mask
      // will be combined with all available registers of the Compiler at the end so we it never use more registers
      // than available.
      RegMask instructionAllowedRegs = 0xFFFFFFFFu;

      uint32_t consecutiveOffset = 0;
      uint32_t consecutiveLeadId = Globals::kInvalidId;
      uint32_t consecutiveParent = Globals::kInvalidId;

      if (instInfo.isEvex()) {
        // EVEX instruction and VEX instructions that can be encoded with EVEX have the possibility to use 32 SIMD
        // registers (XMM/YMM/ZMM).
        if (instInfo.isVex() && !instInfo.isEvexCompatible()) {
          if (instInfo.isEvexKRegOnly()) {
            // EVEX encodable only if the first operand is K register (compare instructions).
            if (!Reg::isKReg(opArray[0]))
              instructionAllowedRegs = 0xFFFFu;
          }
          else if (instInfo.isEvexTwoOpOnly()) {
            // EVEX encodable only if the instruction has two operands (gather instructions).
            if (opCount != 2)
              instructionAllowedRegs = 0xFFFFu;
          }
          else {
            instructionAllowedRegs = 0xFFFFu;
          }
        }
      }
      else if (instInfo.isEvexTransformable()) {
        ib.addAggregatedFlags(RATiedFlags::kInst_IsTransformable);
      }
      else {
        // Not EVEX, restrict everything to [0-15] registers.
        instructionAllowedRegs = 0xFFFFu;
      }

      for (uint32_t i = 0; i < opCount; i++) {
        const Operand& op = opArray[i];
        const OpRWInfo& opRwInfo = rwInfo.operand(i);

        opTypesMask |= 1u << uint32_t(op.opType());

        if (op.isReg()) {
          // Register Operand
          // ----------------
          const Reg& reg = op.as<Reg>();

          RATiedFlags flags = raRegRwFlags(opRwInfo.opFlags());
          RegMask allowedRegs = instructionAllowedRegs;

          // X86-specific constraints related to LO|HI general purpose registers. This is only required when the
          // register is part of the encoding. If the register is fixed we won't restrict anything as it doesn't
          // restrict encoding of other registers.
          if (reg.isGpb() && !opRwInfo.hasOpFlag(OpRWFlags::kRegPhysId)) {
            flags |= RATiedFlags::kX86_Gpb;
            if (!_is64Bit) {
              // Restrict to first four - AL|AH|BL|BH|CL|CH|DL|DH. In 32-bit mode it's not possible to access
              // SIL|DIL, etc, so this is just enough.
              allowedRegs = 0x0Fu;
            }
            else {
              // If we encountered GPB-HI register the situation is much more complicated than in 32-bit mode.
              // We need to patch all registers to not use ID higher than 7 and all GPB-LO registers to not use
              // index higher than 3. Instead of doing the patching here we just set a flag and will do it later,
              // to not complicate this loop.
              if (reg.isGpbHi()) {
                hasGpbHiConstraint = true;
                allowedRegs = 0x0Fu;
              }
            }
          }

          uint32_t vIndex = Operand::virtIdToIndex(reg.id());
          if (vIndex < Operand::kVirtIdCount) {
            RAWorkReg* workReg;
            ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

            // Use RW instead of Write in case that not the whole register is overwritten. This is important
            // for liveness as we cannot kill a register that will be used. For example `mov al, 0xFF` is not
            // a write-only operation if user allocated the whole `rax` register.
            if ((flags & RATiedFlags::kRW) == RATiedFlags::kWrite) {
              if (workReg->regByteMask() & ~(opRwInfo.writeByteMask() | opRwInfo.extendByteMask())) {
                // Not write-only operation.
                flags = (flags & ~RATiedFlags::kOut) | (RATiedFlags::kRead | RATiedFlags::kUse);
              }
            }

            // Do not use RegMem flag if changing Reg to Mem requires a CPU feature that is not available.
            if (rwInfo.rmFeature() && Support::test(flags, RATiedFlags::kUseRM | RATiedFlags::kOutRM)) {
              if (!cc()->code()->cpuFeatures().has(rwInfo.rmFeature())) {
                flags &= ~(RATiedFlags::kUseRM | RATiedFlags::kOutRM);
              }
            }

            RegGroup group = workReg->group();
            RegMask useRegs = _pass->_availableRegs[group] & allowedRegs;
            RegMask outRegs = useRegs;

            uint32_t useId = BaseReg::kIdBad;
            uint32_t outId = BaseReg::kIdBad;

            uint32_t useRewriteMask = 0;
            uint32_t outRewriteMask = 0;

            if (opRwInfo.consecutiveLeadCount()) {
              // There must be a single consecutive register lead, otherwise the RW data is invalid.
              if (consecutiveLeadId != Globals::kInvalidId)
                return DebugUtils::errored(kErrorInvalidState);

              // A consecutive lead register cannot be used as a consecutive +1/+2/+3 register, the registers must be distinct.
              if (RATiedReg::consecutiveDataFromFlags(flags) != 0)
                return DebugUtils::errored(kErrorNotConsecutiveRegs);

              flags |= RATiedFlags::kLeadConsecutive | RATiedReg::consecutiveDataToFlags(opRwInfo.consecutiveLeadCount() - 1);
              consecutiveLeadId = workReg->workId();

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
                if (consecutiveLeadId == Globals::kInvalidId)
                  return DebugUtils::errored(kErrorInvalidState);

                if (consecutiveLeadId == workReg->workId())
                  return DebugUtils::errored(kErrorOverlappedRegs);

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
                if (consecutiveLeadId == Globals::kInvalidId)
                  return DebugUtils::errored(kErrorInvalidState);

                if (consecutiveLeadId == workReg->workId())
                  return DebugUtils::errored(kErrorOverlappedRegs);

                flags |= RATiedFlags::kOutConsecutive | RATiedReg::consecutiveDataToFlags(++consecutiveOffset);
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
          ib.addForbiddenFlags(RATiedFlags::kUseRM | RATiedFlags::kOutRM);

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
              RegMask inOutRegs = _pass->_availableRegs[group];

              uint32_t useId = BaseReg::kIdBad;
              uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (Support::test(flags, RATiedFlags::kUse)) {
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));
                if (opRwInfo.hasOpFlag(OpRWFlags::kMemPhysId)) {
                  useId = opRwInfo.physId();
                  flags |= RATiedFlags::kUseFixed;
                }
              }
              else {
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._baseId));
                if (opRwInfo.hasOpFlag(OpRWFlags::kMemPhysId)) {
                  outId = opRwInfo.physId();
                  flags |= RATiedFlags::kOutFixed;
                }
              }

              ASMJIT_PROPAGATE(ib.add(workReg, flags, inOutRegs, useId, useRewriteMask, inOutRegs, outId, outRewriteMask));
            }
          }

          if (mem.hasIndexReg()) {
            uint32_t vIndex = Operand::virtIdToIndex(mem.indexId());
            if (vIndex < Operand::kVirtIdCount) {
              RAWorkReg* workReg;
              ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

              RATiedFlags flags = raMemIndexRwFlags(opRwInfo.opFlags());
              RegGroup group = workReg->group();
              RegMask inOutRegs = _pass->_availableRegs[group] & instructionAllowedRegs;

              // Index registers have never fixed id on X86/x64.
              const uint32_t useId = BaseReg::kIdBad;
              const uint32_t outId = BaseReg::kIdBad;

              uint32_t useRewriteMask = 0;
              uint32_t outRewriteMask = 0;

              if (Support::test(flags, RATiedFlags::kUse))
                useRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));
              else
                outRewriteMask = Support::bitMask(inst->getRewriteIndex(&mem._data[Operand::kDataMemIndexId]));

              ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRead, inOutRegs, useId, useRewriteMask, inOutRegs, outId, outRewriteMask));
            }
          }
        }
      }
    }

    // Handle extra operand (either REP {cx|ecx|rcx} or AVX-512 {k} selector).
    if (inst->hasExtraReg()) {
      uint32_t vIndex = Operand::virtIdToIndex(inst->extraReg().id());
      if (vIndex < Operand::kVirtIdCount) {
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

        RegGroup group = workReg->group();
        RegMask inOutRegs = _pass->_availableRegs[group];
        uint32_t rewriteMask = Support::bitMask(inst->getRewriteIndex(&inst->extraReg()._id));

        if (group == RegGroup::kX86_K) {
          // AVX-512 mask selector {k} register - read-only, allocable to any register except {k0}.
          ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRead, inOutRegs, BaseReg::kIdBad, rewriteMask, inOutRegs, BaseReg::kIdBad, 0));
          singleRegOps = 0;
        }
        else {
          // REP {cx|ecx|rcx} register - read & write, allocable to {cx|ecx|rcx} only.
          ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRW, inOutRegs, Gp::kIdCx, rewriteMask, inOutRegs, Gp::kIdBad, 0));
        }
      }
      else {
        RegGroup group = inst->extraReg().group();
        if (group == RegGroup::kX86_K && inst->extraReg().id() != 0)
          singleRegOps = 0;
      }
    }

    // If this instruction has move semantics then check whether it could be eliminated if all virtual registers
    // are allocated into the same register. Take into account the virtual size of the destination register as that's
    // more important than a physical register size in this case.
    if (rwInfo.hasInstFlag(InstRWFlags::kMovOp) && !inst->hasExtraReg() && Support::bitTest(opTypesMask, uint32_t(OperandType::kReg))) {
      // AVX+ move instructions have 3 operand form - the first two operands must be the same to guarantee move semantics.
      if (opCount == 2 || (opCount == 3 && opArray[0] == opArray[1])) {
        uint32_t vIndex = Operand::virtIdToIndex(opArray[0].as<Reg>().id());
        if (vIndex < Operand::kVirtIdCount) {
          const VirtReg* vReg = _cc->virtRegByIndex(vIndex);
          const OpRWInfo& opRwInfo = rwInfo.operand(0);

          uint64_t remainingByteMask = vReg->workReg()->regByteMask() & ~opRwInfo.writeByteMask();
          if (remainingByteMask == 0u || (remainingByteMask & opRwInfo.extendByteMask()) == 0)
            ib.addInstRWFlags(InstRWFlags::kMovOp);
        }
      }
    }

    // Handle X86 constraints.
    if (hasGpbHiConstraint) {
      for (RATiedReg& tiedReg : ib) {
        RegMask filter = tiedReg.hasFlag(RATiedFlags::kX86_Gpb) ? 0x0Fu : 0xFFu;
        tiedReg._useRegMask &= filter;
        tiedReg._outRegMask &= filter;
      }
    }

    if (ib.tiedRegCount() == 1) {
      // Handle special cases of some instructions where all operands share the same
      // register. In such case the single operand becomes read-only or write-only.
      InstSameRegHint sameRegHint = InstSameRegHint::kNone;
      if (singleRegOps == opCount) {
        sameRegHint = instInfo.sameRegHint();
      }
      else if (opCount == 2 && inst->op(1).isImm()) {
        // Handle some tricks used by X86 asm.
        const BaseReg& reg = inst->op(0).as<BaseReg>();
        const Imm& imm = inst->op(1).as<Imm>();

        const RAWorkReg* workReg = _pass->workRegById(ib[0]->workId());
        uint32_t workRegSize = workReg->signature().size();

        switch (inst->id()) {
          case Inst::kIdOr: {
            // Sets the value of the destination register to -1, previous content unused.
            if (reg.size() >= 4 || reg.size() >= workRegSize) {
              if (imm.value() == -1 || imm.valueAs<uint64_t>() == raImmMaskFromSize(reg.size()))
                sameRegHint = InstSameRegHint::kWO;
            }
            ASMJIT_FALLTHROUGH;
          }

          case Inst::kIdAdd:
          case Inst::kIdAnd:
          case Inst::kIdRol:
          case Inst::kIdRor:
          case Inst::kIdSar:
          case Inst::kIdShl:
          case Inst::kIdShr:
          case Inst::kIdSub:
          case Inst::kIdXor: {
            // Updates [E|R]FLAGS without changing the content.
            if (reg.size() != 4 || reg.size() >= workRegSize) {
              if (imm.value() == 0)
                sameRegHint = InstSameRegHint::kRO;
            }
            break;
          }
        }
      }

      switch (sameRegHint) {
        case InstSameRegHint::kNone:
          break;
        case InstSameRegHint::kRO:
          ib[0]->makeReadOnly();
          break;
        case InstSameRegHint::kWO:
          ib[0]->makeWriteOnly();
          break;
      }
    }

    cf = instInfo.controlFlow();
  }

  return kErrorOk;
}

// x86::RACFGBuilder - OnInvoke
// ============================

Error RACFGBuilder::onBeforeInvoke(InvokeNode* invokeNode) noexcept {
  const FuncDetail& fd = invokeNode->detail();
  uint32_t argCount = invokeNode->argCount();

  cc()->_setCursor(invokeNode->prev());
  RegType nativeRegType = cc()->_gpSignature.regType();

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

          if (arg.isIndirect()) {
            if (reg.isGp()) {
              if (reg.type() != nativeRegType)
                return DebugUtils::errored(kErrorInvalidAssignment);
              // It's considered allocated if this is an indirect argument and the user used GP.
              continue;
            }

            BaseReg indirectReg;
            moveVecToPtr(invokeNode, arg, reg.as<Vec>(), &indirectReg);
            invokeNode->_args[argIndex][valueIndex] = indirectReg;
          }
          else {
            if (regGroup != argGroup) {
              // TODO: Conversion is not supported.
              return DebugUtils::errored(kErrorInvalidAssignment);
            }
          }
        }
        else {
          if (arg.isIndirect()) {
            if (reg.isGp()) {
              if (reg.type() != nativeRegType)
                return DebugUtils::errored(kErrorInvalidAssignment);

              ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, reg));
              continue;
            }

            BaseReg indirectReg;
            moveVecToPtr(invokeNode, arg, reg.as<Vec>(), &indirectReg);
            ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, indirectReg));
          }
          else {
            ASMJIT_PROPAGATE(moveRegToStackArg(invokeNode, arg, reg));
          }
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
  if (fd.hasFlag(CallConvFlags::kCalleePopsStack) && fd.argStackSize() != 0)
    ASMJIT_PROPAGATE(cc()->sub(cc()->zsp(), fd.argStackSize()));

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
          if (ret.regType() == RegType::kX86_St) {
            if (workReg->group() != RegGroup::kVec)
              return DebugUtils::errored(kErrorInvalidAssignment);

            Reg dst(workReg->signature(), workReg->virtId());
            Mem mem;

            TypeId typeId = TypeUtils::scalarOf(workReg->typeId());
            if (ret.hasTypeId())
              typeId = ret.typeId();

            switch (typeId) {
              case TypeId::kFloat32:
                ASMJIT_PROPAGATE(_pass->useTemporaryMem(mem, 4, 4));
                mem.setSize(4);
                ASMJIT_PROPAGATE(cc()->fstp(mem));
                ASMJIT_PROPAGATE(cc()->emit(choose(Inst::kIdMovss, Inst::kIdVmovss), dst.as<Xmm>(), mem));
                break;

              case TypeId::kFloat64:
                ASMJIT_PROPAGATE(_pass->useTemporaryMem(mem, 8, 4));
                mem.setSize(8);
                ASMJIT_PROPAGATE(cc()->fstp(mem));
                ASMJIT_PROPAGATE(cc()->emit(choose(Inst::kIdMovsd, Inst::kIdVmovsd), dst.as<Xmm>(), mem));
                break;

              default:
                return DebugUtils::errored(kErrorInvalidAssignment);
            }
          }
          else {
            RegGroup regGroup = workReg->group();
            RegGroup retGroup = Reg::groupOf(ret.regType());

            if (regGroup != retGroup) {
              // TODO: Conversion is not supported.
              return DebugUtils::errored(kErrorInvalidAssignment);
            }
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

    // Not handled here...
    const Operand& op = invokeNode->ret(retIndex);
    if (ret.regType() == RegType::kX86_St)
      continue;

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
  for (RegGroup group : RegGroupVirtValues{})
    ib._clobbered[group] = Support::lsbMask<RegMask>(_pass->_physRegCount[group]) & ~fd.preservedRegs(group);

  return kErrorOk;
}

// x86::RACFGBuilder - MoveVecToPtr
// ================================

static inline OperandSignature x86VecRegSignatureBySize(uint32_t size) noexcept {
  return OperandSignature{size >= 64 ? uint32_t(Zmm::kSignature) :
                          size >= 32 ? uint32_t(Ymm::kSignature) : uint32_t(Xmm::kSignature)};
}

Error RACFGBuilder::moveVecToPtr(InvokeNode* invokeNode, const FuncValue& arg, const Vec& src, BaseReg* out) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isReg());

  uint32_t argSize = TypeUtils::sizeOf(arg.typeId());
  if (argSize == 0)
    return DebugUtils::errored(kErrorInvalidState);

  if (argSize < 16)
    argSize = 16;

  uint32_t argStackOffset = Support::alignUp(invokeNode->detail()._argStackSize, argSize);
  _funcNode->frame().updateCallStackAlignment(argSize);
  invokeNode->detail()._argStackSize = argStackOffset + argSize;

  Vec vecReg(x86VecRegSignatureBySize(argSize), src.id());
  Mem vecPtr = ptr(_pass->_sp.as<Gp>(), int32_t(argStackOffset));

  uint32_t vMovInstId = choose(Inst::kIdMovaps, Inst::kIdVmovaps);
  if (argSize > 16)
    vMovInstId = Inst::kIdVmovaps;

  ASMJIT_PROPAGATE(cc()->_newReg(out, ArchTraits::byArch(cc()->arch()).regTypeToTypeId(cc()->_gpSignature.regType()), nullptr));

  VirtReg* vReg = cc()->virtRegById(out->id());
  vReg->setWeight(BaseRAPass::kCallArgWeight);

  ASMJIT_PROPAGATE(cc()->lea(out->as<Gp>(), vecPtr));
  ASMJIT_PROPAGATE(cc()->emit(vMovInstId, ptr(out->as<Gp>()), vecReg));

  if (arg.isStack()) {
    Mem stackPtr = ptr(_pass->_sp.as<Gp>(), arg.stackOffset());
    ASMJIT_PROPAGATE(cc()->mov(stackPtr, out->as<Gp>()));
  }

  return kErrorOk;
}

// x86::RACFGBuilder - MoveImmToRegArg
// ===================================

Error RACFGBuilder::moveImmToRegArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_, BaseReg* out) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isReg());

  Imm imm(imm_);
  TypeId rTypeId = TypeId::kUInt32;

  switch (arg.typeId()) {
    case TypeId::kInt8: imm.signExtend8Bits(); goto MovU32;
    case TypeId::kUInt8: imm.zeroExtend8Bits(); goto MovU32;
    case TypeId::kInt16: imm.signExtend16Bits(); goto MovU32;
    case TypeId::kUInt16: imm.zeroExtend16Bits(); goto MovU32;

    case TypeId::kInt32:
    case TypeId::kUInt32:
MovU32:
      imm.zeroExtend32Bits();
      break;

    case TypeId::kInt64:
    case TypeId::kUInt64:
      // Moving to GPD automatically zero extends in 64-bit mode.
      if (imm.isUInt32()) {
        imm.zeroExtend32Bits();
        break;
      }

      rTypeId = TypeId::kUInt64;
      break;

    default:
      return DebugUtils::errored(kErrorInvalidAssignment);
  }

  ASMJIT_PROPAGATE(cc()->_newReg(out, rTypeId, nullptr));
  cc()->virtRegById(out->id())->setWeight(BaseRAPass::kCallArgWeight);

  return cc()->mov(out->as<x86::Gp>(), imm);
}

// x86::RACFGBuilder - MoveImmToStackArg
// =====================================

Error RACFGBuilder::moveImmToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const Imm& imm_) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isStack());

  Mem stackPtr = ptr(_pass->_sp.as<Gp>(), arg.stackOffset());
  Imm imm[2];

  stackPtr.setSize(4);
  imm[0] = imm_;
  uint32_t nMovs = 0;

  // One stack entry has the same size as the native register size. That means that if we want to move a 32-bit
  // integer on the stack in 64-bit mode, we need to extend it to a 64-bit integer first. In 32-bit mode, pushing
  // a 64-bit on stack is done in two steps by pushing low and high parts separately.
  switch (arg.typeId()) {
    case TypeId::kInt8: imm[0].signExtend8Bits(); goto MovU32;
    case TypeId::kUInt8: imm[0].zeroExtend8Bits(); goto MovU32;
    case TypeId::kInt16: imm[0].signExtend16Bits(); goto MovU32;
    case TypeId::kUInt16: imm[0].zeroExtend16Bits(); goto MovU32;

    case TypeId::kInt32:
    case TypeId::kUInt32:
    case TypeId::kFloat32:
MovU32:
      imm[0].zeroExtend32Bits();
      nMovs = 1;
      break;

    case TypeId::kInt64:
    case TypeId::kUInt64:
    case TypeId::kFloat64:
    case TypeId::kMmx32:
    case TypeId::kMmx64:
      if (_is64Bit && imm[0].isInt32()) {
        stackPtr.setSize(8);
        nMovs = 1;
        break;
      }

      imm[1].setValue(imm[0].uint32Hi());
      imm[0].zeroExtend32Bits();
      nMovs = 2;
      break;

    default:
      return DebugUtils::errored(kErrorInvalidAssignment);
  }

  for (uint32_t i = 0; i < nMovs; i++) {
    ASMJIT_PROPAGATE(cc()->mov(stackPtr, imm[i]));
    stackPtr.addOffsetLo32(int32_t(stackPtr.size()));
  }

  return kErrorOk;
}

// x86::RACFGBuilder - MoveRegToStackArg
// =====================================

Error RACFGBuilder::moveRegToStackArg(InvokeNode* invokeNode, const FuncValue& arg, const BaseReg& reg) noexcept {
  DebugUtils::unused(invokeNode);
  ASMJIT_ASSERT(arg.isStack());

  Mem stackPtr = ptr(_pass->_sp.as<Gp>(), arg.stackOffset());
  Reg r0, r1;

  VirtReg* vr = cc()->virtRegById(reg.id());
  uint32_t registerSize = cc()->registerSize();
  InstId instId = 0;

  TypeId dstTypeId = arg.typeId();
  TypeId srcTypeId = vr->typeId();

  switch (dstTypeId) {
    case TypeId::kInt64:
    case TypeId::kUInt64:
      // Extend BYTE->QWORD (GP).
      if (TypeUtils::isGp8(srcTypeId)) {
        r1.setRegT<RegType::kX86_GpbLo>(reg.id());

        instId = (dstTypeId == TypeId::kInt64 && srcTypeId == TypeId::kInt8) ? Inst::kIdMovsx : Inst::kIdMovzx;
        goto ExtendMovGpXQ;
      }

      // Extend WORD->QWORD (GP).
      if (TypeUtils::isGp16(srcTypeId)) {
        r1.setRegT<RegType::kX86_Gpw>(reg.id());

        instId = (dstTypeId == TypeId::kInt64 && srcTypeId == TypeId::kInt16) ? Inst::kIdMovsx : Inst::kIdMovzx;
        goto ExtendMovGpXQ;
      }

      // Extend DWORD->QWORD (GP).
      if (TypeUtils::isGp32(srcTypeId)) {
        r1.setRegT<RegType::kX86_Gpd>(reg.id());

        instId = Inst::kIdMovsxd;
        if (dstTypeId == TypeId::kInt64 && srcTypeId == TypeId::kInt32)
          goto ExtendMovGpXQ;
        else
          goto ZeroExtendGpDQ;
      }

      // Move QWORD (GP).
      if (TypeUtils::isGp64(srcTypeId)) goto MovGpQ;
      if (TypeUtils::isMmx(srcTypeId)) goto MovMmQ;
      if (TypeUtils::isVec(srcTypeId)) goto MovXmmQ;
      break;

    case TypeId::kInt32:
    case TypeId::kUInt32:
    case TypeId::kInt16:
    case TypeId::kUInt16:
      // DWORD <- WORD (Zero|Sign Extend).
      if (TypeUtils::isGp16(srcTypeId)) {
        bool isDstSigned = dstTypeId == TypeId::kInt16 || dstTypeId == TypeId::kInt32;
        bool isSrcSigned = srcTypeId == TypeId::kInt8  || srcTypeId == TypeId::kInt16;

        r1.setRegT<RegType::kX86_Gpw>(reg.id());
        instId = isDstSigned && isSrcSigned ? Inst::kIdMovsx : Inst::kIdMovzx;
        goto ExtendMovGpD;
      }

      // DWORD <- BYTE (Zero|Sign Extend).
      if (TypeUtils::isGp8(srcTypeId)) {
        bool isDstSigned = dstTypeId == TypeId::kInt16 || dstTypeId == TypeId::kInt32;
        bool isSrcSigned = srcTypeId == TypeId::kInt8  || srcTypeId == TypeId::kInt16;

        r1.setRegT<RegType::kX86_GpbLo>(reg.id());
        instId = isDstSigned && isSrcSigned ? Inst::kIdMovsx : Inst::kIdMovzx;
        goto ExtendMovGpD;
      }
      ASMJIT_FALLTHROUGH;

    case TypeId::kInt8:
    case TypeId::kUInt8:
      if (TypeUtils::isInt(srcTypeId)) goto MovGpD;
      if (TypeUtils::isMmx(srcTypeId)) goto MovMmD;
      if (TypeUtils::isVec(srcTypeId)) goto MovXmmD;
      break;

    case TypeId::kMmx32:
    case TypeId::kMmx64:
      // Extend BYTE->QWORD (GP).
      if (TypeUtils::isGp8(srcTypeId)) {
        r1.setRegT<RegType::kX86_GpbLo>(reg.id());

        instId = Inst::kIdMovzx;
        goto ExtendMovGpXQ;
      }

      // Extend WORD->QWORD (GP).
      if (TypeUtils::isGp16(srcTypeId)) {
        r1.setRegT<RegType::kX86_Gpw>(reg.id());

        instId = Inst::kIdMovzx;
        goto ExtendMovGpXQ;
      }

      if (TypeUtils::isGp32(srcTypeId)) goto ExtendMovGpDQ;
      if (TypeUtils::isGp64(srcTypeId)) goto MovGpQ;
      if (TypeUtils::isMmx(srcTypeId)) goto MovMmQ;
      if (TypeUtils::isVec(srcTypeId)) goto MovXmmQ;
      break;

    case TypeId::kFloat32:
    case TypeId::kFloat32x1:
      if (TypeUtils::isVec(srcTypeId)) goto MovXmmD;
      break;

    case TypeId::kFloat64:
    case TypeId::kFloat64x1:
      if (TypeUtils::isVec(srcTypeId)) goto MovXmmQ;
      break;

    default:
      if (TypeUtils::isVec(dstTypeId) && reg.as<Reg>().isVec()) {
        stackPtr.setSize(TypeUtils::sizeOf(dstTypeId));
        uint32_t vMovInstId = choose(Inst::kIdMovaps, Inst::kIdVmovaps);

        if (TypeUtils::isVec128(dstTypeId))
          r0.setRegT<RegType::kX86_Xmm>(reg.id());
        else if (TypeUtils::isVec256(dstTypeId))
          r0.setRegT<RegType::kX86_Ymm>(reg.id());
        else if (TypeUtils::isVec512(dstTypeId))
          r0.setRegT<RegType::kX86_Zmm>(reg.id());
        else
          break;

        return cc()->emit(vMovInstId, stackPtr, r0);
      }
      break;
  }
  return DebugUtils::errored(kErrorInvalidAssignment);

  // Extend+Move Gp.
ExtendMovGpD:
  stackPtr.setSize(4);
  r0.setRegT<RegType::kX86_Gpd>(reg.id());

  ASMJIT_PROPAGATE(cc()->emit(instId, r0, r1));
  ASMJIT_PROPAGATE(cc()->emit(Inst::kIdMov, stackPtr, r0));
  return kErrorOk;

ExtendMovGpXQ:
  if (registerSize == 8) {
    stackPtr.setSize(8);
    r0.setRegT<RegType::kX86_Gpq>(reg.id());

    ASMJIT_PROPAGATE(cc()->emit(instId, r0, r1));
    ASMJIT_PROPAGATE(cc()->emit(Inst::kIdMov, stackPtr, r0));
  }
  else {
    stackPtr.setSize(4);
    r0.setRegT<RegType::kX86_Gpd>(reg.id());

    ASMJIT_PROPAGATE(cc()->emit(instId, r0, r1));

ExtendMovGpDQ:
    ASMJIT_PROPAGATE(cc()->emit(Inst::kIdMov, stackPtr, r0));
    stackPtr.addOffsetLo32(4);
    ASMJIT_PROPAGATE(cc()->emit(Inst::kIdAnd, stackPtr, 0));
  }
  return kErrorOk;

ZeroExtendGpDQ:
  stackPtr.setSize(4);
  r0.setRegT<RegType::kX86_Gpd>(reg.id());
  goto ExtendMovGpDQ;

MovGpD:
  stackPtr.setSize(4);
  r0.setRegT<RegType::kX86_Gpd>(reg.id());
  return cc()->emit(Inst::kIdMov, stackPtr, r0);

MovGpQ:
  stackPtr.setSize(8);
  r0.setRegT<RegType::kX86_Gpq>(reg.id());
  return cc()->emit(Inst::kIdMov, stackPtr, r0);

MovMmD:
  stackPtr.setSize(4);
  r0.setRegT<RegType::kX86_Mm>(reg.id());
  return cc()->emit(choose(Inst::kIdMovd, Inst::kIdVmovd), stackPtr, r0);

MovMmQ:
  stackPtr.setSize(8);
  r0.setRegT<RegType::kX86_Mm>(reg.id());
  return cc()->emit(choose(Inst::kIdMovq, Inst::kIdVmovq), stackPtr, r0);

MovXmmD:
  stackPtr.setSize(4);
  r0.setRegT<RegType::kX86_Xmm>(reg.id());
  return cc()->emit(choose(Inst::kIdMovss, Inst::kIdVmovss), stackPtr, r0);

MovXmmQ:
  stackPtr.setSize(8);
  r0.setRegT<RegType::kX86_Xmm>(reg.id());
  return cc()->emit(choose(Inst::kIdMovlps, Inst::kIdVmovlps), stackPtr, r0);
}

// x86::RACFGBuilder - OnReg
// =========================

Error RACFGBuilder::onBeforeRet(FuncRetNode* funcRet) noexcept {
  const FuncDetail& funcDetail = _pass->func()->detail();
  const Operand* opArray = funcRet->operands();
  uint32_t opCount = funcRet->opCount();

  cc()->_setCursor(funcRet->prev());

  for (uint32_t i = 0; i < opCount; i++) {
    const Operand& op = opArray[i];
    const FuncValue& ret = funcDetail.ret(i);

    if (!op.isReg())
      continue;

    if (ret.regType() == RegType::kX86_St) {
      const Reg& reg = op.as<Reg>();
      uint32_t vIndex = Operand::virtIdToIndex(reg.id());

      if (vIndex < Operand::kVirtIdCount) {
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

        if (workReg->group() != RegGroup::kVec)
          return DebugUtils::errored(kErrorInvalidAssignment);

        Reg src(workReg->signature(), workReg->virtId());
        Mem mem;

        TypeId typeId = TypeUtils::scalarOf(workReg->typeId());
        if (ret.hasTypeId())
          typeId = ret.typeId();

        switch (typeId) {
          case TypeId::kFloat32:
            ASMJIT_PROPAGATE(_pass->useTemporaryMem(mem, 4, 4));
            mem.setSize(4);
            ASMJIT_PROPAGATE(cc()->emit(choose(Inst::kIdMovss, Inst::kIdVmovss), mem, src.as<Xmm>()));
            ASMJIT_PROPAGATE(cc()->fld(mem));
            break;

          case TypeId::kFloat64:
            ASMJIT_PROPAGATE(_pass->useTemporaryMem(mem, 8, 4));
            mem.setSize(8);
            ASMJIT_PROPAGATE(cc()->emit(choose(Inst::kIdMovsd, Inst::kIdVmovsd), mem, src.as<Xmm>()));
            ASMJIT_PROPAGATE(cc()->fld(mem));
            break;

          default:
            return DebugUtils::errored(kErrorInvalidAssignment);
        }
      }
    }
  }

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

    // Not handled here...
    if (ret.regType() == RegType::kX86_St)
      continue;

    if (op.isReg()) {
      // Register return value.
      const Reg& reg = op.as<Reg>();
      uint32_t vIndex = Operand::virtIdToIndex(reg.id());

      if (vIndex < Operand::kVirtIdCount) {
        RAWorkReg* workReg;
        ASMJIT_PROPAGATE(_pass->virtIndexAsWorkReg(vIndex, &workReg));

        RegGroup group = workReg->group();
        RegMask inOutRegs = _pass->_availableRegs[group];
        ASMJIT_PROPAGATE(ib.add(workReg, RATiedFlags::kUse | RATiedFlags::kRead, inOutRegs, ret.regId(), 0, inOutRegs, BaseReg::kIdBad, 0));
      }
    }
    else {
      return DebugUtils::errored(kErrorInvalidAssignment);
    }
  }

  return kErrorOk;
}

// x86::X86RAPass - Construction & Destruction
// ===========================================

X86RAPass::X86RAPass() noexcept
  : BaseRAPass() { _iEmitHelper = &_emitHelper; }
X86RAPass::~X86RAPass() noexcept {}

// x86::X86RAPass - OnInit & OnDone
// ================================

void X86RAPass::onInit() noexcept {
  Arch arch = cc()->arch();
  uint32_t baseRegCount = Environment::is32Bit(arch) ? 8u : 16u;
  uint32_t simdRegCount = baseRegCount;

  if (Environment::is64Bit(arch) && _func->frame().isAvx512Enabled())
    simdRegCount = 32u;

  bool avxEnabled = _func->frame().isAvxEnabled();
  bool avx512Enabled = _func->frame().isAvx512Enabled();

  _emitHelper._emitter = _cb;
  _emitHelper._avxEnabled = avxEnabled || avx512Enabled;
  _emitHelper._avx512Enabled = avx512Enabled;

  _archTraits = &ArchTraits::byArch(arch);
  _physRegCount.set(RegGroup::kGp, baseRegCount);
  _physRegCount.set(RegGroup::kVec, simdRegCount);
  _physRegCount.set(RegGroup::kX86_K, 8);
  _physRegCount.set(RegGroup::kX86_MM, 8);
  _buildPhysIndex();

  _availableRegCount = _physRegCount;
  _availableRegs[RegGroup::kGp] = Support::lsbMask<RegMask>(_physRegCount.get(RegGroup::kGp));
  _availableRegs[RegGroup::kVec] = Support::lsbMask<RegMask>(_physRegCount.get(RegGroup::kVec));
  _availableRegs[RegGroup::kX86_K] = Support::lsbMask<RegMask>(_physRegCount.get(RegGroup::kX86_K)) ^ 1u;
  _availableRegs[RegGroup::kX86_MM] = Support::lsbMask<RegMask>(_physRegCount.get(RegGroup::kX86_MM));

  _scratchRegIndexes[0] = uint8_t(Gp::kIdCx);
  _scratchRegIndexes[1] = uint8_t(baseRegCount - 1);

  // The architecture specific setup makes implicitly all registers available. So
  // make unavailable all registers that are special and cannot be used in general.
  bool hasFP = _func->frame().hasPreservedFP();

  makeUnavailable(RegGroup::kGp, Gp::kIdSp);            // ESP|RSP used as a stack-pointer (SP).
  if (hasFP) makeUnavailable(RegGroup::kGp, Gp::kIdBp); // EBP|RBP used as a frame-pointer (FP).

  _sp = cc()->zsp();
  _fp = cc()->zbp();
}

void X86RAPass::onDone() noexcept {}

// x86::X86RAPass - BuildCFG
// =========================

Error X86RAPass::buildCFG() noexcept {
  return RACFGBuilder(this).run();
}

// x86::X86RAPass - Rewrite
// ========================

static InstId transformVexToEvex(InstId instId) {
  switch (instId) {
    case Inst::kIdVbroadcastf128: return Inst::kIdVbroadcastf32x4;
    case Inst::kIdVbroadcasti128: return Inst::kIdVbroadcasti32x4;
    case Inst::kIdVextractf128: return Inst::kIdVextractf32x4;
    case Inst::kIdVextracti128: return Inst::kIdVextracti32x4;
    case Inst::kIdVinsertf128: return Inst::kIdVinsertf32x4;
    case Inst::kIdVinserti128: return Inst::kIdVinserti32x4;
    case Inst::kIdVmovdqa: return Inst::kIdVmovdqa32;
    case Inst::kIdVmovdqu: return Inst::kIdVmovdqu32;
    case Inst::kIdVpand: return Inst::kIdVpandd;
    case Inst::kIdVpandn: return Inst::kIdVpandnd;
    case Inst::kIdVpor: return Inst::kIdVpord;
    case Inst::kIdVpxor: return Inst::kIdVpxord;
    case Inst::kIdVroundpd: return Inst::kIdVrndscalepd;
    case Inst::kIdVroundps: return Inst::kIdVrndscaleps;
    case Inst::kIdVroundsd: return Inst::kIdVrndscalesd;
    case Inst::kIdVroundss: return Inst::kIdVrndscaless;

    default:
      // This should never happen as only transformable instructions should go this path.
      ASMJIT_ASSERT(false);
      return 0;
  }
}

ASMJIT_FAVOR_SPEED Error X86RAPass::_rewrite(BaseNode* first, BaseNode* stop) noexcept {
  uint32_t virtCount = cc()->_vRegArray.size();

  BaseNode* node = first;
  while (node != stop) {
    BaseNode* next = node->next();
    if (node->isInst()) {
      InstNode* inst = node->as<InstNode>();
      RAInst* raInst = node->passData<RAInst>();

      Operand* operands = inst->operands();
      uint32_t opCount = inst->opCount();
      uint32_t maxRegId = 0;

      uint32_t i;

      // Rewrite virtual registers into physical registers.
      if (raInst) {
        // This data is allocated by Zone passed to `runOnFunction()`, which will be reset after the RA pass finishes.
        // So reset this data to prevent having a dead pointer after the RA pass is complete.
        node->resetPassData();

        // If the instruction contains pass data (raInst) then it was a subject for register allocation and must be
        // rewritten to use physical regs.
        RATiedReg* tiedRegs = raInst->tiedRegs();
        uint32_t tiedCount = raInst->tiedCount();

        for (i = 0; i < tiedCount; i++) {
          RATiedReg* tiedReg = &tiedRegs[i];

          Support::BitWordIterator<uint32_t> useIt(tiedReg->useRewriteMask());
          uint32_t useId = tiedReg->useId();
          while (useIt.hasNext()) {
            maxRegId = Support::max(maxRegId, useId);
            inst->rewriteIdAtIndex(useIt.next(), useId);
          }

          Support::BitWordIterator<uint32_t> outIt(tiedReg->outRewriteMask());
          uint32_t outId = tiedReg->outId();
          while (outIt.hasNext()) {
            maxRegId = Support::max(maxRegId, outId);
            inst->rewriteIdAtIndex(outIt.next(), outId);
          }
        }

        // Transform VEX instruction to EVEX when necessary.
        if (raInst->isTransformable()) {
          if (maxRegId > 15) {
            inst->setId(transformVexToEvex(inst->id()));
          }
        }

        // Remove moves that do not do anything.
        //
        // Usually these moves are inserted during code generation and originally they used different registers. If RA
        // allocated these into the same register such redundant mov would appear.
        if (raInst->hasInstRWFlag(InstRWFlags::kMovOp) && !inst->hasExtraReg()) {
          if (inst->opCount() == 2) {
            if (inst->op(0) == inst->op(1)) {
              cc()->removeNode(node);
              goto Next;
            }
          }
        }

        if (ASMJIT_UNLIKELY(node->type() != NodeType::kInst)) {
          // FuncRet terminates the flow, it must either be removed if the exit label is next to it (optimization) or
          // patched to an architecture dependent jump instruction that jumps to the function's exit before the epilog.
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
    }

Next:
    node = next;
  }

  return kErrorOk;
}

// x86::X86RAPass - OnEmit
// =======================

Error X86RAPass::emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
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

Error X86RAPass::emitSwap(uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept {
  RAWorkReg* waReg = workRegById(aWorkId);
  RAWorkReg* wbReg = workRegById(bWorkId);

  bool is64Bit = Support::max(waReg->typeId(), wbReg->typeId()) >= TypeId::kInt64;
  OperandSignature sign = is64Bit ? OperandSignature{RegTraits<RegType::kX86_Gpq>::kSignature}
                                  : OperandSignature{RegTraits<RegType::kX86_Gpd>::kSignature};

#ifndef ASMJIT_NO_LOGGING
  if (hasDiagnosticOption(DiagnosticOptions::kRAAnnotate)) {
    _tmpString.assignFormat("<SWAP> %s, %s", waReg->name(), wbReg->name());
    cc()->setInlineComment(_tmpString.data());
  }
#endif

  return cc()->emit(Inst::kIdXchg, Reg(sign, aPhysId), Reg(sign, bPhysId));
}

Error X86RAPass::emitLoad(uint32_t workId, uint32_t dstPhysId) noexcept {
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

Error X86RAPass::emitSave(uint32_t workId, uint32_t srcPhysId) noexcept {
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

Error X86RAPass::emitJump(const Label& label) noexcept {
  return cc()->jmp(label);
}

Error X86RAPass::emitPreCall(InvokeNode* invokeNode) noexcept {
  if (invokeNode->detail().hasVarArgs() && cc()->is64Bit()) {
    const FuncDetail& fd = invokeNode->detail();
    uint32_t argCount = invokeNode->argCount();

    switch (invokeNode->detail().callConv().id()) {
      case CallConvId::kX64SystemV: {
        // AL register contains the number of arguments passed in XMM register(s).
        uint32_t n = 0;
        for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
          const FuncValuePack& argPack = fd.argPack(argIndex);
          for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
            const FuncValue& arg = argPack[valueIndex];
            if (!arg)
              break;

            if (arg.isReg() && Reg::groupOf(arg.regType()) == RegGroup::kVec)
              n++;
          }
        }

        if (!n)
          ASMJIT_PROPAGATE(cc()->xor_(eax, eax));
        else
          ASMJIT_PROPAGATE(cc()->mov(eax, n));
        break;
      }

      case CallConvId::kX64Windows: {
        // Each double-precision argument passed in XMM must be also passed in GP.
        for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
          const FuncValuePack& argPack = fd.argPack(argIndex);
          for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
            const FuncValue& arg = argPack[valueIndex];
            if (!arg)
              break;

            if (arg.isReg() && Reg::groupOf(arg.regType()) == RegGroup::kVec) {
              Gp dst = gpq(fd.callConv().passedOrder(RegGroup::kGp)[argIndex]);
              Xmm src = xmm(arg.regId());
              ASMJIT_PROPAGATE(cc()->emit(choose(Inst::kIdMovq, Inst::kIdVmovq), dst, src));
            }
          }
        }
        break;
      }

      default:
        return DebugUtils::errored(kErrorInvalidState);
    }
  }

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86 && !ASMJIT_NO_COMPILER
