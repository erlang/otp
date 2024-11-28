// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/archtraits.h"
#include "../core/func.h"
#include "../core/operand.h"
#include "../core/type.h"
#include "../core/funcargscontext_p.h"

#if !defined(ASMJIT_NO_X86)
  #include "../x86/x86func_p.h"
#endif

#if !defined(ASMJIT_NO_AARCH64)
  #include "../arm/a64func_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

// CallConv - Initialization & Reset
// =================================

ASMJIT_FAVOR_SIZE Error CallConv::init(CallConvId ccId, const Environment& environment) noexcept {
  reset();

#if !defined(ASMJIT_NO_X86)
  if (environment.isFamilyX86())
    return x86::FuncInternal::initCallConv(*this, ccId, environment);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (environment.isFamilyAArch64())
    return a64::FuncInternal::initCallConv(*this, ccId, environment);
#endif

  return DebugUtils::errored(kErrorInvalidArgument);
}

// FuncDetail - Init / Reset
// =========================

ASMJIT_FAVOR_SIZE Error FuncDetail::init(const FuncSignature& signature, const Environment& environment) noexcept {
  CallConvId ccId = signature.callConvId();
  uint32_t argCount = signature.argCount();

  if (ASMJIT_UNLIKELY(argCount > Globals::kMaxFuncArgs))
    return DebugUtils::errored(kErrorInvalidArgument);

  CallConv& cc = _callConv;
  ASMJIT_PROPAGATE(cc.init(ccId, environment));

  uint32_t registerSize = Environment::registerSizeFromArch(cc.arch());
  uint32_t deabstractDelta = TypeUtils::deabstractDeltaOfSize(registerSize);

  const TypeId* signatureArgs = signature.args();
  for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
    FuncValuePack& argPack = _args[argIndex];
    argPack[0].initTypeId(TypeUtils::deabstract(signatureArgs[argIndex], deabstractDelta));
  }

  _argCount = uint8_t(argCount);
  _vaIndex = uint8_t(signature.vaIndex());

  TypeId ret = signature.ret();
  if (ret != TypeId::kVoid)
    _rets[0].initTypeId(TypeUtils::deabstract(ret, deabstractDelta));

#if !defined(ASMJIT_NO_X86)
  if (environment.isFamilyX86())
    return x86::FuncInternal::initFuncDetail(*this, signature, registerSize);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (environment.isFamilyAArch64())
    return a64::FuncInternal::initFuncDetail(*this, signature);
#endif

  // We should never bubble here as if `cc.init()` succeeded then there has to be an implementation for the current
  // architecture. However, stay safe.
  return DebugUtils::errored(kErrorInvalidArgument);
}

// FuncFrame - Init
// ================

ASMJIT_FAVOR_SIZE Error FuncFrame::init(const FuncDetail& func) noexcept {
  Arch arch = func.callConv().arch();
  if (!Environment::isValidArch(arch))
    return DebugUtils::errored(kErrorInvalidArch);

  const ArchTraits& archTraits = ArchTraits::byArch(arch);

  // Initializing FuncFrame means making a copy of some properties of `func`. Properties like `_localStackSize` will
  // be set by the user before the frame is finalized.
  reset();

  _arch = arch;
  _spRegId = uint8_t(archTraits.spRegId());
  _saRegId = uint8_t(BaseReg::kIdBad);

  uint32_t naturalStackAlignment = func.callConv().naturalStackAlignment();
  uint32_t minDynamicAlignment = Support::max<uint32_t>(naturalStackAlignment, 16);

  if (minDynamicAlignment == naturalStackAlignment)
    minDynamicAlignment <<= 1;

  _naturalStackAlignment = uint8_t(naturalStackAlignment);
  _minDynamicAlignment = uint8_t(minDynamicAlignment);
  _redZoneSize = uint8_t(func.redZoneSize());
  _spillZoneSize = uint8_t(func.spillZoneSize());
  _finalStackAlignment = uint8_t(_naturalStackAlignment);

  if (func.hasFlag(CallConvFlags::kCalleePopsStack)) {
    _calleeStackCleanup = uint16_t(func.argStackSize());
  }

  // Initial masks of dirty and preserved registers.
  for (RegGroup group : RegGroupVirtValues{}) {
    _dirtyRegs[group] = func.usedRegs(group);
    _preservedRegs[group] = func.preservedRegs(group);
  }

  // Exclude stack pointer - this register is never included in saved GP regs.
  _preservedRegs[RegGroup::kGp] &= ~Support::bitMask(archTraits.spRegId());

  // The size and alignment of save/restore area of registers for each virtual register group
  _saveRestoreRegSize = func.callConv()._saveRestoreRegSize;
  _saveRestoreAlignment = func.callConv()._saveRestoreAlignment;

  return kErrorOk;
}

// FuncFrame - Finalize
// ====================

ASMJIT_FAVOR_SIZE Error FuncFrame::finalize() noexcept {
  if (!Environment::isValidArch(arch()))
    return DebugUtils::errored(kErrorInvalidArch);

  const ArchTraits& archTraits = ArchTraits::byArch(arch());

  uint32_t registerSize = _saveRestoreRegSize[RegGroup::kGp];
  uint32_t vectorSize = _saveRestoreRegSize[RegGroup::kVec];
  uint32_t returnAddressSize = archTraits.hasLinkReg() ? 0u : registerSize;

  // The final stack alignment must be updated accordingly to call and local stack alignments.
  uint32_t stackAlignment = _finalStackAlignment;
  ASMJIT_ASSERT(stackAlignment == Support::max(_naturalStackAlignment,
                                               _callStackAlignment,
                                               _localStackAlignment));

  bool hasFP = hasPreservedFP();
  bool hasDA = hasDynamicAlignment();

  uint32_t kSp = archTraits.spRegId();
  uint32_t kFp = archTraits.fpRegId();
  uint32_t kLr = archTraits.linkRegId();

  // Make frame pointer dirty if the function uses it.
  if (hasFP) {
    _dirtyRegs[RegGroup::kGp] |= Support::bitMask(kFp);

    // Currently required by ARM, if this works differently across architectures we would have to generalize most
    // likely in CallConv.
    if (kLr != BaseReg::kIdBad)
      _dirtyRegs[RegGroup::kGp] |= Support::bitMask(kLr);
  }

  // These two are identical if the function doesn't align its stack dynamically.
  uint32_t saRegId = _saRegId;
  if (saRegId == BaseReg::kIdBad)
    saRegId = kSp;

  // Fix stack arguments base-register from SP to FP in case it was not picked before and the function performs
  // dynamic stack alignment.
  if (hasDA && saRegId == kSp)
    saRegId = kFp;

  // Mark as dirty any register but SP if used as SA pointer.
  if (saRegId != kSp)
    _dirtyRegs[RegGroup::kGp] |= Support::bitMask(saRegId);

  _spRegId = uint8_t(kSp);
  _saRegId = uint8_t(saRegId);

  // Setup stack size used to save preserved registers.
  uint32_t saveRestoreSizes[2] {};
  for (RegGroup group : RegGroupVirtValues{})
    saveRestoreSizes[size_t(!archTraits.hasInstPushPop(group))]
      += Support::alignUp(Support::popcnt(savedRegs(group)) * saveRestoreRegSize(group), saveRestoreAlignment(group));

  _pushPopSaveSize  = uint16_t(saveRestoreSizes[0]);
  _extraRegSaveSize = uint16_t(saveRestoreSizes[1]);

  uint32_t v = 0;                            // The beginning of the stack frame relative to SP after prolog.
  v += callStackSize();                      // Count 'callStackSize'      <- This is used to call functions.
  v  = Support::alignUp(v, stackAlignment);  // Align to function's stack alignment.

  _localStackOffset = v;                     // Store 'localStackOffset'   <- Function's local stack starts here.
  v += localStackSize();                     // Count 'localStackSize'     <- Function's local stack ends here.

  // If the function's stack must be aligned, calculate the alignment necessary to store vector registers, and set
  // `FuncAttributes::kAlignedVecSR` to inform PEI that it can use instructions that perform aligned stores/loads.
  if (stackAlignment >= vectorSize && _extraRegSaveSize) {
    addAttributes(FuncAttributes::kAlignedVecSR);
    v = Support::alignUp(v, vectorSize);     // Align 'extraRegSaveOffset'.
  }

  _extraRegSaveOffset = v;                   // Store 'extraRegSaveOffset' <- Non-GP save/restore starts here.
  v += _extraRegSaveSize;                    // Count 'extraRegSaveSize'   <- Non-GP save/restore ends here.

  // Calculate if dynamic alignment (DA) slot (stored as offset relative to SP) is required and its offset.
  if (hasDA && !hasFP) {
    _daOffset = v;                           // Store 'daOffset'           <- DA pointer would be stored here.
    v += registerSize;                       // Count 'daOffset'.
  }
  else {
    _daOffset = FuncFrame::kTagInvalidOffset;
  }

  // Link Register
  // -------------
  //
  // The stack is aligned after the function call as the return address is stored in a link register. Some
  // architectures may require to always have aligned stack after PUSH/POP operation, which is represented
  // by ArchTraits::stackAlignmentConstraint().
  //
  // No Link Register (X86/X64)
  // --------------------------
  //
  // The return address should be stored after GP save/restore regs. It has the same size as `registerSize`
  // (basically the native register/pointer size). We don't adjust it now as `v` now contains the exact size
  // that the function requires to adjust (call frame + stack frame, vec stack size). The stack (if we consider
  // this size) is misaligned now, as it's always aligned before the function call - when `call()` is executed
  // it pushes the current EIP|RIP onto the stack, and misaligns it by 12 or 8 bytes (depending on the
  // architecture). So count number of bytes needed to align it up to the function's CallFrame (the beginning).
  if (v || hasFuncCalls() || !returnAddressSize)
    v += Support::alignUpDiff(v + pushPopSaveSize() + returnAddressSize, stackAlignment);

  _pushPopSaveOffset = v;                    // Store 'pushPopSaveOffset'  <- Function's push/pop save/restore starts here.
  _stackAdjustment = v;                      // Store 'stackAdjustment'    <- SA used by 'add SP, SA' and 'sub SP, SA'.
  v += _pushPopSaveSize;                     // Count 'pushPopSaveSize'    <- Function's push/pop save/restore ends here.
  _finalStackSize = v;                       // Store 'finalStackSize'     <- Final stack used by the function.

  if (!archTraits.hasLinkReg())
    v += registerSize;                       // Count 'ReturnAddress'      <- As CALL pushes onto stack.

  // If the function performs dynamic stack alignment then the stack-adjustment must be aligned.
  if (hasDA)
    _stackAdjustment = Support::alignUp(_stackAdjustment, stackAlignment);

  // Calculate where the function arguments start relative to SP.
  _saOffsetFromSP = hasDA ? FuncFrame::kTagInvalidOffset : v;

  // Calculate where the function arguments start relative to FP or user-provided register.
  _saOffsetFromSA = hasFP ? returnAddressSize + registerSize      // Return address + frame pointer.
                          : returnAddressSize + _pushPopSaveSize; // Return address + all push/pop regs.

  return kErrorOk;
}

// FuncArgsAssignment - UpdateFuncFrame
// ====================================

ASMJIT_FAVOR_SIZE Error FuncArgsAssignment::updateFuncFrame(FuncFrame& frame) const noexcept {
  Arch arch = frame.arch();
  const FuncDetail* func = funcDetail();

  if (!func)
    return DebugUtils::errored(kErrorInvalidState);

  RAConstraints constraints;
  ASMJIT_PROPAGATE(constraints.init(arch));

  FuncArgsContext ctx;
  ASMJIT_PROPAGATE(ctx.initWorkData(frame, *this, &constraints));
  ASMJIT_PROPAGATE(ctx.markDstRegsDirty(frame));
  ASMJIT_PROPAGATE(ctx.markScratchRegs(frame));
  ASMJIT_PROPAGATE(ctx.markStackArgsReg(frame));
  return kErrorOk;
}

// Func API - Tests
// ================

#if defined(ASMJIT_TEST)
UNIT(func_signature) {
  FuncSignature signature;
  signature.setRetT<int8_t>();
  signature.addArgT<int16_t>();
  signature.addArg(TypeId::kInt32);

  EXPECT_EQ(signature, FuncSignature::build<int8_t, int16_t, int32_t>());
}
#endif

ASMJIT_END_NAMESPACE
