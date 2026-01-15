// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/archtraits.h"
#include "../core/emithelper_p.h"
#include "../core/formatter.h"
#include "../core/funcargscontext_p.h"
#include "../core/radefs_p.h"

// Can be used for debugging...
// #define ASMJIT_DUMP_ARGS_ASSIGNMENT

ASMJIT_BEGIN_NAMESPACE

// BaseEmitHelper - Formatting
// ===========================

#ifdef ASMJIT_DUMP_ARGS_ASSIGNMENT
static void dumpFuncValue(String& sb, Arch arch, const FuncValue& value) noexcept {
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

static void dumpAssignment(String& sb, const FuncArgsContext& ctx) noexcept {
  typedef FuncArgsContext::Var Var;

  Arch arch = ctx.arch();
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

// BaseEmitHelper - Abstract
// =========================

Error BaseEmitHelper::emitRegMove(const Operand_& dst_, const Operand_& src_, TypeId typeId, const char* comment) {
  DebugUtils::unused(dst_, src_, typeId, comment);
  return DebugUtils::errored(kErrorInvalidState);
}

Error BaseEmitHelper::emitRegSwap(const BaseReg& a, const BaseReg& b, const char* comment) {
  DebugUtils::unused(a, b, comment);
  return DebugUtils::errored(kErrorInvalidState);
}

Error BaseEmitHelper::emitArgMove(const BaseReg& dst_, TypeId dstTypeId, const Operand_& src_, TypeId srcTypeId, const char* comment) {
  DebugUtils::unused(dst_, dstTypeId, src_, srcTypeId, comment);
  return DebugUtils::errored(kErrorInvalidState);
}

// BaseEmitHelper - EmitArgsAssignment
// ===================================

ASMJIT_FAVOR_SIZE Error BaseEmitHelper::emitArgsAssignment(const FuncFrame& frame, const FuncArgsAssignment& args) {
  typedef FuncArgsContext::Var Var;
  typedef FuncArgsContext::WorkData WorkData;

  enum WorkFlags : uint32_t {
    kWorkNone      = 0x00,
    kWorkDidSome   = 0x01,
    kWorkPending   = 0x02,
    kWorkPostponed = 0x04
  };

  Arch arch = frame.arch();
  const ArchTraits& archTraits = ArchTraits::byArch(arch);

  RAConstraints constraints;
  FuncArgsContext ctx;

  ASMJIT_PROPAGATE(constraints.init(arch));
  ASMJIT_PROPAGATE(ctx.initWorkData(frame, args, &constraints));

#ifdef ASMJIT_DUMP_ARGS_ASSIGNMENT
  {
    String sb;
    dumpAssignment(sb, ctx);
    printf("%s\n", sb.data());
  }
#endif

  auto& workData = ctx._workData;
  uint32_t varCount = ctx._varCount;
  uint32_t saVarId = ctx._saVarId;

  BaseReg sp = BaseReg(_emitter->_gpSignature, archTraits.spRegId());
  BaseReg sa = sp;

  if (frame.hasDynamicAlignment()) {
    if (frame.hasPreservedFP())
      sa.setId(archTraits.fpRegId());
    else
      sa.setId(saVarId < varCount ? ctx._vars[saVarId].cur.regId() : frame.saRegId());
  }

  // Register to stack and stack to stack moves must be first as now we have
  // the biggest chance of having as many as possible unassigned registers.

  if (ctx._stackDstMask) {
    // Base address of all arguments passed by stack.
    BaseMem baseArgPtr(sa, int32_t(frame.saOffset(sa.id())));
    BaseMem baseStackPtr(sp, 0);

    for (uint32_t varId = 0; varId < varCount; varId++) {
      Var& var = ctx._vars[varId];

      if (!var.out.isStack())
        continue;

      FuncValue& cur = var.cur;
      FuncValue& out = var.out;

      ASMJIT_ASSERT(cur.isReg() || cur.isStack());
      BaseReg reg;

      BaseMem dstStackPtr = baseStackPtr.cloneAdjusted(out.stackOffset());
      BaseMem srcStackPtr = baseArgPtr.cloneAdjusted(cur.stackOffset());

      if (cur.isIndirect()) {
        if (cur.isStack()) {
          // TODO: Indirect stack.
          return DebugUtils::errored(kErrorInvalidAssignment);
        }
        else {
          srcStackPtr.setBaseId(cur.regId());
        }
      }

      if (cur.isReg() && !cur.isIndirect()) {
        WorkData& wd = workData[archTraits.regTypeToGroup(cur.regType())];
        uint32_t regId = cur.regId();

        reg.setSignatureAndId(archTraits.regTypeToSignature(cur.regType()), regId);
        wd.unassign(varId, regId);
      }
      else {
        // Stack to reg move - tricky since we move stack to stack we can decide which register to use. In general
        // we follow the rule that IntToInt moves will use GP regs with possibility to signature or zero extend,
        // and all other moves will either use GP or VEC regs depending on the size of the move.
        OperandSignature signature = getSuitableRegForMemToMemMove(arch, out.typeId(), cur.typeId());
        if (ASMJIT_UNLIKELY(!signature.isValid()))
          return DebugUtils::errored(kErrorInvalidState);

        WorkData& wd = workData[signature.regGroup()];
        RegMask availableRegs = wd.availableRegs();
        if (ASMJIT_UNLIKELY(!availableRegs))
          return DebugUtils::errored(kErrorInvalidState);

        uint32_t availableId = Support::ctz(availableRegs);
        reg.setSignatureAndId(signature, availableId);

        ASMJIT_PROPAGATE(emitArgMove(reg, out.typeId(), srcStackPtr, cur.typeId()));
      }

      if (cur.isIndirect() && cur.isReg())
        workData[RegGroup::kGp].unassign(varId, cur.regId());

      // Register to stack move.
      ASMJIT_PROPAGATE(emitRegMove(dstStackPtr, reg, cur.typeId()));
      var.markDone();
    }
  }

  // Shuffle all registers that are currently assigned accordingly to target assignment.

  uint32_t workFlags = kWorkNone;
  for (;;) {
    for (uint32_t varId = 0; varId < varCount; varId++) {
      Var& var = ctx._vars[varId];
      if (var.isDone() || !var.cur.isReg())
        continue;

      FuncValue& cur = var.cur;
      FuncValue& out = var.out;

      RegGroup curGroup = archTraits.regTypeToGroup(cur.regType());
      RegGroup outGroup = archTraits.regTypeToGroup(out.regType());

      uint32_t curId = cur.regId();
      uint32_t outId = out.regId();

      if (curGroup != outGroup) {
        // TODO: Conversion is not supported.
        return DebugUtils::errored(kErrorInvalidAssignment);
      }
      else {
        WorkData& wd = workData[outGroup];
        if (!wd.isAssigned(outId) || curId == outId) {
EmitMove:
          ASMJIT_PROPAGATE(
            emitArgMove(
              BaseReg(archTraits.regTypeToSignature(out.regType()), outId), out.typeId(),
              BaseReg(archTraits.regTypeToSignature(cur.regType()), curId), cur.typeId()));

          // Only reassign if this is not a sign/zero extension that happens on the same in/out register.
          if (curId != outId)
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
            // Only few architectures provide swap operations, and only for few register groups.
            if (archTraits.hasInstRegSwap(curGroup)) {
              RegType highestType = Support::max(cur.regType(), altVar.cur.regType());
              if (Support::isBetween(highestType, RegType::kGp8Lo, RegType::kGp16))
                highestType = RegType::kGp32;

              OperandSignature signature = archTraits.regTypeToSignature(highestType);
              ASMJIT_PROPAGATE(
                emitRegSwap(BaseReg(signature, outId), BaseReg(signature, curId)));

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
              RegMask availableRegs = wd.availableRegs();
              if (availableRegs) {
                RegMask inOutRegs = wd.dstRegs();
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

  // Load arguments passed by stack into registers. This is pretty simple and
  // it never requires multiple iterations like the previous phase.

  if (ctx._hasStackSrc) {
    uint32_t iterCount = 1;
    if (frame.hasDynamicAlignment() && !frame.hasPreservedFP())
      sa.setId(saVarId < varCount ? ctx._vars[saVarId].cur.regId() : frame.saRegId());

    // Base address of all arguments passed by stack.
    BaseMem baseArgPtr(sa, int32_t(frame.saOffset(sa.id())));

    for (uint32_t iter = 0; iter < iterCount; iter++) {
      for (uint32_t varId = 0; varId < varCount; varId++) {
        Var& var = ctx._vars[varId];
        if (var.isDone())
          continue;

        if (var.cur.isStack()) {
          ASMJIT_ASSERT(var.out.isReg());

          uint32_t outId = var.out.regId();
          RegType outType = var.out.regType();

          RegGroup group = archTraits.regTypeToGroup(outType);
          WorkData& wd = workData[group];

          if (outId == sa.id() && group == RegGroup::kGp) {
            // This register will be processed last as we still need `saRegId`.
            if (iterCount == 1) {
              iterCount++;
              continue;
            }
            wd.unassign(wd._physToVarId[outId], outId);
          }

          BaseReg dstReg = BaseReg(archTraits.regTypeToSignature(outType), outId);
          BaseMem srcMem = baseArgPtr.cloneAdjusted(var.cur.stackOffset());

          ASMJIT_PROPAGATE(emitArgMove(
            dstReg, var.out.typeId(),
            srcMem, var.cur.typeId()));

          wd.assign(varId, outId);
          var.cur.initReg(outType, outId, var.cur.typeId(), FuncValue::kFlagIsDone);
        }
      }
    }
  }

  return kErrorOk;
}

ASMJIT_END_NAMESPACE
