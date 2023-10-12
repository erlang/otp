// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/funcargscontext_p.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_core
//! \{

FuncArgsContext::FuncArgsContext() noexcept {
  for (RegGroup group : RegGroupVirtValues{})
    _workData[size_t(group)].reset();
}

ASMJIT_FAVOR_SIZE Error FuncArgsContext::initWorkData(const FuncFrame& frame, const FuncArgsAssignment& args, const RAConstraints* constraints) noexcept {
  Arch arch = frame.arch();
  const FuncDetail& func = *args.funcDetail();

  _archTraits = &ArchTraits::byArch(arch);
  _constraints = constraints;
  _arch = arch;

  // Initialize `_archRegs`.
  for (RegGroup group : RegGroupVirtValues{})
    _workData[group]._archRegs = _constraints->availableRegs(group);

  if (frame.hasPreservedFP())
    _workData[size_t(RegGroup::kGp)]._archRegs &= ~Support::bitMask(archTraits().fpRegId());

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

      RegGroup dstGroup = RegGroup::kMaxValue;
      uint32_t dstId = BaseReg::kIdBad;
      WorkData* dstWd = nullptr;

      // Not supported.
      if (src.isIndirect())
        return DebugUtils::errored(kErrorInvalidAssignment);

      if (dst.isReg()) {
        RegType dstType = dst.regType();
        if (ASMJIT_UNLIKELY(!archTraits().hasRegType(dstType)))
          return DebugUtils::errored(kErrorInvalidRegType);

        // Copy TypeId from source if the destination doesn't have it. The RA used by BaseCompiler would never
        // leave TypeId undefined, but users of FuncAPI can just assign phys regs without specifying the type.
        if (!dst.hasTypeId())
          dst.setTypeId(archTraits().regTypeToTypeId(dst.regType()));

        dstGroup = archTraits().regTypeToGroup(dstType);
        if (ASMJIT_UNLIKELY(dstGroup > RegGroup::kMaxVirt))
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

        OperandSignature signature = getSuitableRegForMemToMemMove(arch, dst.typeId(), src.typeId());
        if (ASMJIT_UNLIKELY(!signature.isValid()))
          return DebugUtils::errored(kErrorInvalidState);
        _stackDstMask = uint8_t(_stackDstMask | Support::bitMask(signature.regGroup()));
      }

      if (src.isReg()) {
        uint32_t srcId = src.regId();
        RegGroup srcGroup = archTraits().regTypeToGroup(src.regType());

        if (dstGroup == srcGroup) {
          ASMJIT_ASSERT(dstWd != nullptr);
          dstWd->assign(varId, srcId);

          // The best case, register is allocated where it is expected to be.
          if (dstId == srcId)
            var.markDone();
        }
        else {
          if (ASMJIT_UNLIKELY(srcGroup > RegGroup::kMaxVirt))
            return DebugUtils::errored(kErrorInvalidState);

          WorkData& srcData = _workData[size_t(srcGroup)];
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
  for (RegGroup group : RegGroupVirtValues{}) {
    _workData[group]._workRegs =
      (_workData[group].archRegs() & (frame.dirtyRegs(group) | ~frame.preservedRegs(group))) | _workData[group].dstRegs() | _workData[group].assignedRegs();
  }

  // Create a variable that represents `SARegId` if necessary.
  bool saRegRequired = _hasStackSrc && frame.hasDynamicAlignment() && !frame.hasPreservedFP();

  WorkData& gpRegs = _workData[RegGroup::kGp];
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
    TypeId ptrTypeId = Environment::is32Bit(arch) ? TypeId::kUInt32 : TypeId::kUInt64;
    RegType ptrRegType = Environment::is32Bit(arch) ? RegType::kGp32 : RegType::kGp64;

    _saVarId = uint8_t(varId);
    _hasPreservedFP = frame.hasPreservedFP();

    Var& var = _vars[varId];
    var.reset();

    if (saCurRegId == BaseReg::kIdBad) {
      if (saOutRegId != BaseReg::kIdBad && !gpRegs.isAssigned(saOutRegId)) {
        saCurRegId = saOutRegId;
      }
      else {
        RegMask availableRegs = gpRegs.availableRegs();
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

      RegGroup group = archTraits().regTypeToGroup(var.cur.regType());
      if (group != archTraits().regTypeToGroup(var.out.regType()))
        continue;

      WorkData& wd = _workData[group];
      if (wd.isAssigned(dstId)) {
        Var& other = _vars[wd._physToVarId[dstId]];
        if (archTraits().regTypeToGroup(other.out.regType()) == group && other.out.regId() == srcId) {
          wd._numSwaps++;
          _regSwapsMask = uint8_t(_regSwapsMask | Support::bitMask(group));
        }
      }
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error FuncArgsContext::markDstRegsDirty(FuncFrame& frame) noexcept {
  for (RegGroup group : RegGroupVirtValues{}) {
    WorkData& wd = _workData[group];
    uint32_t regs = wd.usedRegs() | wd._dstShuf;

    wd._workRegs |= regs;
    frame.addDirtyRegs(group, regs);
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error FuncArgsContext::markScratchRegs(FuncFrame& frame) noexcept {
  uint32_t groupMask = 0;

  // Handle stack to stack moves.
  groupMask |= _stackDstMask;

  // Handle register swaps.
  groupMask |= _regSwapsMask & ~Support::bitMask(RegGroup::kGp);

  if (!groupMask)
    return kErrorOk;

  // Selects one dirty register per affected group that can be used as a scratch register.
  for (RegGroup group : RegGroupVirtValues{}) {
    if (Support::bitTest(groupMask, group)) {
      WorkData& wd = _workData[group];

      // Initially, pick some clobbered or dirty register.
      RegMask workRegs = wd.workRegs();
      RegMask regs = workRegs & ~(wd.usedRegs() | wd._dstShuf);

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

      RegMask regMask = Support::blsi(regs);
      wd._workRegs |= regMask;
      frame.addDirtyRegs(group, regMask);
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error FuncArgsContext::markStackArgsReg(FuncFrame& frame) noexcept {
  if (_saVarId != kVarIdNone) {
    const Var& var = _vars[_saVarId];
    frame.setSARegId(var.cur.regId());
  }
  else if (frame.hasPreservedFP()) {
    frame.setSARegId(archTraits().fpRegId());
  }

  return kErrorOk;
}

//! \}
//! \endcond

ASMJIT_END_NAMESPACE
