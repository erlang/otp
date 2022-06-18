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

#include "../core/formatter.h"
#include "../core/funcargscontext_p.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../arm/a64emithelper_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// ============================================================================
// [asmjit::a64::EmitHelper - Emit Operations]
// ============================================================================

ASMJIT_FAVOR_SIZE Error EmitHelper::emitRegMove(
  const Operand_& dst_,
  const Operand_& src_, uint32_t typeId, const char* comment) {

  Emitter* emitter = _emitter->as<Emitter>();

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(Type::isValid(typeId) && !Type::isAbstract(typeId));

  emitter->setInlineComment(comment);

  if (dst_.isReg() && src_.isMem()) {
    Reg dst(dst_.as<Reg>());
    Mem src(src_.as<Mem>());

    switch (typeId) {
      case Type::kIdI8:
      case Type::kIdU8:
        return emitter->ldrb(dst.as<Gp>(), src);

      case Type::kIdI16:
      case Type::kIdU16:
        return emitter->ldrh(dst.as<Gp>(), src);

      case Type::kIdI32:
      case Type::kIdU32:
        return emitter->ldr(dst.as<Gp>().w(), src);

      case Type::kIdI64:
      case Type::kIdU64:
        return emitter->ldr(dst.as<Gp>().x(), src);

      default: {
        if (Type::isVec32(typeId))
          return emitter->ldr(dst.as<Vec>().s(), src);

        if (Type::isVec64(typeId))
          return emitter->ldr(dst.as<Vec>().d(), src);

        if (Type::isVec128(typeId))
          return emitter->ldr(dst.as<Vec>().d(), src);

        break;
      }
    }
  }

  if (dst_.isMem() && src_.isReg()) {
    Mem dst(dst_.as<Mem>());
    Reg src(src_.as<Reg>());

    switch (typeId) {
      case Type::kIdI8:
      case Type::kIdU8:
        return emitter->strb(src.as<Gp>(), dst);

      case Type::kIdI16:
      case Type::kIdU16:
        return emitter->strh(src.as<Gp>(), dst);

      case Type::kIdI32:
      case Type::kIdU32:
        return emitter->str(src.as<Gp>().w(), dst);

      case Type::kIdI64:
      case Type::kIdU64:
        return emitter->str(src.as<Gp>().x(), dst);

      default: {
        if (Type::isVec32(typeId))
          return emitter->str(src.as<Vec>().s(), dst);

        if (Type::isVec64(typeId))
          return emitter->str(src.as<Vec>().d(), dst);

        if (Type::isVec128(typeId))
          return emitter->str(src.as<Vec>().d(), dst);

        break;
      }
    }
  }

  if (dst_.isReg() && src_.isReg()) {
    Reg dst(dst_.as<Reg>());
    Reg src(src_.as<Reg>());

    switch (typeId) {
      case Type::kIdI8:
      case Type::kIdU8:
      case Type::kIdI16:
      case Type::kIdU16:
      case Type::kIdI32:
      case Type::kIdU32:
      case Type::kIdI64:
      case Type::kIdU64:
        return emitter->mov(src.as<Gp>().x(), dst.as<Gp>().x());

      default: {
        if (Type::isVec32(typeId))
          return emitter->fmov(dst.as<Vec>().s(), src.as<Vec>().s());

        if (Type::isVec64(typeId))
          return emitter->mov(dst.as<Vec>().b8(), src.as<Vec>().b8());

        if (Type::isVec128(typeId))
          return emitter->mov(dst.as<Vec>().b16(), src.as<Vec>().b16());

        break;
      }
    }
  }

  emitter->setInlineComment(nullptr);
  return DebugUtils::errored(kErrorInvalidState);
}

Error EmitHelper::emitRegSwap(
  const BaseReg& a,
  const BaseReg& b, const char* comment) {

  DebugUtils::unused(a, b, comment);
  return DebugUtils::errored(kErrorInvalidState);
}

Error EmitHelper::emitArgMove(
  const BaseReg& dst_, uint32_t dstTypeId,
  const Operand_& src_, uint32_t srcTypeId, const char* comment) {

  // Deduce optional `dstTypeId`, which may be `Type::kIdVoid` in some cases.
  if (!dstTypeId) {
    const ArchTraits& archTraits = ArchTraits::byArch(_emitter->arch());
    dstTypeId = archTraits.regTypeToTypeId(dst_.type());
  }

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(Type::isValid(dstTypeId) && !Type::isAbstract(dstTypeId));
  ASMJIT_ASSERT(Type::isValid(srcTypeId) && !Type::isAbstract(srcTypeId));

  Reg dst(dst_.as<Reg>());
  Operand src(src_);

  uint32_t dstSize = Type::sizeOf(dstTypeId);
  uint32_t srcSize = Type::sizeOf(srcTypeId);

  // TODO: [ARM] EmitArgMove is unfinished.

  if (Type::isInt(dstTypeId)) {
    if (Type::isInt(srcTypeId)) {
      uint32_t x = dstSize == 8;
      uint32_t instId = Inst::kIdNone;

      switch (srcTypeId) {
        case Type::kIdI8 : instId = Inst::kIdLdrsb; break;
        case Type::kIdU8 : instId = Inst::kIdLdrb; break;

        case Type::kIdI16: instId = Inst::kIdLdrsh; break;
        case Type::kIdU16: instId = Inst::kIdLdrh; break;

        case Type::kIdI32: instId = x ? Inst::kIdLdrsw : Inst::kIdLdr; break;
        case Type::kIdU32: instId = Inst::kIdLdr; x = 0; break;

        case Type::kIdI64: instId = Inst::kIdLdr; break;
        case Type::kIdU64: instId = Inst::kIdLdr; break;
      }

      if (!instId)
        return DebugUtils::errored(kErrorInvalidState);

      dst.setSignature(x ? GpX::kSignature : GpW::kSignature);
      _emitter->setInlineComment(comment);
      return _emitter->emit(instId, dst, src);
    }
  }

  return DebugUtils::errored(kErrorInvalidState);
}

// ============================================================================
// [asmjit::a64::EmitHelper - Emit Prolog & Epilog]
// ============================================================================

struct PrologEpilogInfo {
  struct RegPair {
    uint8_t ids[2];
    uint16_t offset;
  };

  struct GroupData {
    RegPair pairs[16];
    uint32_t pairCount;
  };

  GroupData groups[2];
  uint32_t sizeTotal;

  Error init(const FuncFrame& frame) noexcept {
    uint32_t offset = 0;

    for (uint32_t group = 0; group < 2; group++) {
      GroupData& data = groups[group];
      uint32_t n = 0;
      uint32_t pairCount = 0;
      RegPair* pairs = data.pairs;
      uint32_t slotSize = frame.saveRestoreRegSize(group);

      uint32_t savedRegs = frame.savedRegs(group);
      if (group == 0 && frame.hasPreservedFP()) {
        // Must be at the beginning of the push/pop sequence.
        ASMJIT_ASSERT(pairCount == 0);

        pairs[0].offset = uint16_t(offset);
        pairs[0].ids[0] = Gp::kIdFp;
        pairs[0].ids[1] = Gp::kIdLr;
        offset += slotSize * 2;
        pairCount++;

        savedRegs &= ~Support::bitMask(Gp::kIdFp, Gp::kIdLr);
      }

      Support::BitWordIterator<uint32_t> it(savedRegs);
      while (it.hasNext()) {
        pairs[pairCount].ids[n] = uint8_t(it.next());

        if (++n == 2) {
          pairs[pairCount].offset = uint16_t(offset);
          offset += slotSize * 2;

          n = 0;
          pairCount++;
        }
      }

      if (n == 1) {
        pairs[pairCount].ids[1] = uint8_t(BaseReg::kIdBad);
        pairs[pairCount].offset = uint16_t(offset);
        offset += slotSize * 2;
        pairCount++;
      }

      data.pairCount = pairCount;
    }

    sizeTotal = offset;
    return kErrorOk;
  }
};

// TODO: [ARM] Emit prolog.
ASMJIT_FAVOR_SIZE Error EmitHelper::emitProlog(const FuncFrame& frame) {
  Emitter* emitter = _emitter->as<Emitter>();

  PrologEpilogInfo pei;
  ASMJIT_PROPAGATE(pei.init(frame));

  static const Reg groupRegs[2] = { x0, d0 };
  static const uint32_t groupInsts[2][2] = {
    { Inst::kIdStr  , Inst::kIdStp   },
    { Inst::kIdStr_v, Inst::kIdStp_v }
  };

  uint32_t adjustInitialOffset = pei.sizeTotal;

  for (uint32_t group = 0; group < 2; group++) {
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pairCount = data.pairCount;

    Reg regs[2] = { groupRegs[group], groupRegs[group] };
    Mem mem = ptr(sp);

    const uint32_t* insts = groupInsts[group];

    for (uint32_t i = 0; i < pairCount; i++) {
      const PrologEpilogInfo::RegPair& pair = data.pairs[i];

      regs[0].setId(pair.ids[0]);
      regs[1].setId(pair.ids[1]);
      mem.setOffsetLo32(pair.offset);

      if (pair.offset == 0 && adjustInitialOffset) {
        mem.setOffset(-int(adjustInitialOffset));
        mem.makePreIndex();
      }

      if (pair.ids[1] == BaseReg::kIdBad)
        ASMJIT_PROPAGATE(emitter->emit(insts[0], regs[0], mem));
      else
        ASMJIT_PROPAGATE(emitter->emit(insts[1], regs[0], regs[1], mem));

      mem.resetToFixedOffset();

      if (i == 0 && frame.hasPreservedFP()) {
        ASMJIT_PROPAGATE(emitter->mov(x29, sp));
      }
    }
  }

  if (frame.hasStackAdjustment()) {
    uint32_t adj = frame.stackAdjustment();
    if (adj <= 0xFFFu) {
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj));
    }
    else if (adj <= 0xFFFFFFu)  {
      // TODO: [ARM] Prolog - we must touch the pages otherwise it's undefined.
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj & 0x000FFFu));
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj & 0xFFF000u));
    }
    else {
      return DebugUtils::errored(kErrorInvalidState);
    }
  }

  return kErrorOk;
}

// TODO: [ARM] Emit epilog.
ASMJIT_FAVOR_SIZE Error EmitHelper::emitEpilog(const FuncFrame& frame) {
  Emitter* emitter = _emitter->as<Emitter>();

  PrologEpilogInfo pei;
  ASMJIT_PROPAGATE(pei.init(frame));

  static const Reg groupRegs[2] = { x0, d0 };
  static const uint32_t groupInsts[2][2] = {
    { Inst::kIdLdr  , Inst::kIdLdp   },
    { Inst::kIdLdr_v, Inst::kIdLdp_v }
  };

  uint32_t adjustInitialOffset = pei.sizeTotal;

  if (frame.hasStackAdjustment()) {
    uint32_t adj = frame.stackAdjustment();
    if (adj <= 0xFFFu) {
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj));
    }
    else if (adj <= 0xFFFFFFu)  {
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj & 0x000FFFu));
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj & 0xFFF000u));
    }
    else {
      return DebugUtils::errored(kErrorInvalidState);
    }
  }

  for (int group = 1; group >= 0; group--) {
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pairCount = data.pairCount;

    Reg regs[2] = { groupRegs[group], groupRegs[group] };
    Mem mem = ptr(sp);

    const uint32_t* insts = groupInsts[group];

    for (int i = int(pairCount) - 1; i >= 0; i--) {
      const PrologEpilogInfo::RegPair& pair = data.pairs[i];

      regs[0].setId(pair.ids[0]);
      regs[1].setId(pair.ids[1]);
      mem.setOffsetLo32(pair.offset);

      if (pair.offset == 0 && adjustInitialOffset) {
        mem.setOffset(int(adjustInitialOffset));
        mem.makePostIndex();
      }

      if (pair.ids[1] == BaseReg::kIdBad)
        ASMJIT_PROPAGATE(emitter->emit(insts[0], regs[0], mem));
      else
        ASMJIT_PROPAGATE(emitter->emit(insts[1], regs[0], regs[1], mem));

      mem.resetToFixedOffset();
    }
  }

  ASMJIT_PROPAGATE(emitter->ret(x30));

  return kErrorOk;
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_ARM
