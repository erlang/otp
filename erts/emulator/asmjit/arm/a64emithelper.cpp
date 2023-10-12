// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH64)

#include "../core/formatter.h"
#include "../core/funcargscontext_p.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../arm/a64emithelper_p.h"
#include "../arm/a64formatter_p.h"
#include "../arm/a64instapi_p.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::EmitHelper - Emit Operations
// =================================

ASMJIT_FAVOR_SIZE Error EmitHelper::emitRegMove(
  const Operand_& dst_,
  const Operand_& src_, TypeId typeId, const char* comment) {

  Emitter* emitter = _emitter->as<Emitter>();

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(TypeUtils::isValid(typeId) && !TypeUtils::isAbstract(typeId));

  emitter->setInlineComment(comment);

  if (dst_.isReg() && src_.isMem()) {
    Reg dst(dst_.as<Reg>());
    Mem src(src_.as<Mem>());

    switch (typeId) {
      case TypeId::kInt8:
      case TypeId::kUInt8:
        return emitter->ldrb(dst.as<Gp>(), src);

      case TypeId::kInt16:
      case TypeId::kUInt16:
        return emitter->ldrh(dst.as<Gp>(), src);

      case TypeId::kInt32:
      case TypeId::kUInt32:
        return emitter->ldr(dst.as<Gp>().w(), src);

      case TypeId::kInt64:
      case TypeId::kUInt64:
        return emitter->ldr(dst.as<Gp>().x(), src);

      default: {
        if (TypeUtils::isFloat32(typeId) || TypeUtils::isVec32(typeId))
          return emitter->ldr(dst.as<Vec>().s(), src);

        if (TypeUtils::isFloat64(typeId) || TypeUtils::isVec64(typeId))
          return emitter->ldr(dst.as<Vec>().d(), src);

        if (TypeUtils::isVec128(typeId))
          return emitter->ldr(dst.as<Vec>().q(), src);

        break;
      }
    }
  }

  if (dst_.isMem() && src_.isReg()) {
    Mem dst(dst_.as<Mem>());
    Reg src(src_.as<Reg>());

    switch (typeId) {
      case TypeId::kInt8:
      case TypeId::kUInt8:
        return emitter->strb(src.as<Gp>(), dst);

      case TypeId::kInt16:
      case TypeId::kUInt16:
        return emitter->strh(src.as<Gp>(), dst);

      case TypeId::kInt32:
      case TypeId::kUInt32:
        return emitter->str(src.as<Gp>().w(), dst);

      case TypeId::kInt64:
      case TypeId::kUInt64:
        return emitter->str(src.as<Gp>().x(), dst);

      default: {
        if (TypeUtils::isFloat32(typeId) || TypeUtils::isVec32(typeId))
          return emitter->str(src.as<Vec>().s(), dst);

        if (TypeUtils::isFloat64(typeId) || TypeUtils::isVec64(typeId))
          return emitter->str(src.as<Vec>().d(), dst);

        if (TypeUtils::isVec128(typeId))
          return emitter->str(src.as<Vec>().q(), dst);

        break;
      }
    }
  }

  if (dst_.isReg() && src_.isReg()) {
    Reg dst(dst_.as<Reg>());
    Reg src(src_.as<Reg>());

    switch (typeId) {
      case TypeId::kInt8:
      case TypeId::kUInt8:
      case TypeId::kInt16:
      case TypeId::kUInt16:
      case TypeId::kInt32:
      case TypeId::kUInt32:
      case TypeId::kInt64:
      case TypeId::kUInt64:
        return emitter->mov(dst.as<Gp>().x(), src.as<Gp>().x());

      default: {
        if (TypeUtils::isFloat32(typeId) || TypeUtils::isVec32(typeId))
          return emitter->fmov(dst.as<Vec>().s(), src.as<Vec>().s());

        if (TypeUtils::isFloat64(typeId) || TypeUtils::isVec64(typeId))
          return emitter->mov(dst.as<Vec>().b8(), src.as<Vec>().b8());

        if (TypeUtils::isVec128(typeId))
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

// TODO: [ARM] EmitArgMove is unfinished.
Error EmitHelper::emitArgMove(
  const BaseReg& dst_, TypeId dstTypeId,
  const Operand_& src_, TypeId srcTypeId, const char* comment) {

  // Deduce optional `dstTypeId`, which may be `TypeId::kVoid` in some cases.
  if (dstTypeId == TypeId::kVoid) {
    const ArchTraits& archTraits = ArchTraits::byArch(_emitter->arch());
    dstTypeId = archTraits.regTypeToTypeId(dst_.type());
  }

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(TypeUtils::isValid(dstTypeId) && !TypeUtils::isAbstract(dstTypeId));
  ASMJIT_ASSERT(TypeUtils::isValid(srcTypeId) && !TypeUtils::isAbstract(srcTypeId));

  Reg dst(dst_.as<Reg>());
  Operand src(src_);

  uint32_t dstSize = TypeUtils::sizeOf(dstTypeId);
  uint32_t srcSize = TypeUtils::sizeOf(srcTypeId);

  if (TypeUtils::isInt(dstTypeId)) {
    if (TypeUtils::isInt(srcTypeId)) {
      uint32_t x = dstSize == 8;

      dst.setSignature(OperandSignature{x ? uint32_t(GpX::kSignature) : uint32_t(GpW::kSignature)});
      _emitter->setInlineComment(comment);

      if (src.isReg()) {
        src.setSignature(dst.signature());
        return _emitter->emit(Inst::kIdMov, dst, src);
      }
      else if (src.isMem()) {
        InstId instId = Inst::kIdNone;
          switch (srcTypeId) {
          case TypeId::kInt8: instId = Inst::kIdLdrsb; break;
          case TypeId::kUInt8: instId = Inst::kIdLdrb; break;
          case TypeId::kInt16: instId = Inst::kIdLdrsh; break;
          case TypeId::kUInt16: instId = Inst::kIdLdrh; break;
          case TypeId::kInt32: instId = x ? Inst::kIdLdrsw : Inst::kIdLdr; break;
          case TypeId::kUInt32: instId = Inst::kIdLdr; x = 0; break;
          case TypeId::kInt64: instId = Inst::kIdLdr; break;
          case TypeId::kUInt64: instId = Inst::kIdLdr; break;
          default:
            return DebugUtils::errored(kErrorInvalidState);
        }
        return _emitter->emit(instId, dst, src);
      }
    }
  }

  if (TypeUtils::isFloat(dstTypeId) || TypeUtils::isVec(dstTypeId)) {
    if (TypeUtils::isFloat(srcTypeId) || TypeUtils::isVec(srcTypeId)) {
      switch (srcSize) {
        case 2: dst.as<Vec>().setSignature(OperandSignature{VecH::kSignature}); break;
        case 4: dst.as<Vec>().setSignature(OperandSignature{VecS::kSignature}); break;
        case 8: dst.as<Vec>().setSignature(OperandSignature{VecD::kSignature}); break;
        case 16: dst.as<Vec>().setSignature(OperandSignature{VecV::kSignature}); break;
        default:
          return DebugUtils::errored(kErrorInvalidState);
      }

      _emitter->setInlineComment(comment);

      if (src.isReg()) {
        InstId instId = srcSize <= 4 ? Inst::kIdFmov_v : Inst::kIdMov_v;
        src.setSignature(dst.signature());
        return _emitter->emit(instId, dst, src);
      }
      else if (src.isMem()) {
        return _emitter->emit(Inst::kIdLdr_v, dst, src);
      }
    }
  }

  return DebugUtils::errored(kErrorInvalidState);
}

// a64::EmitHelper - Emit Prolog & Epilog
// ======================================

struct LoadStoreInstructions {
  InstId singleInstId;
  InstId pairInstId;
};

struct PrologEpilogInfo {
  struct RegPair {
    uint8_t ids[2];
    uint16_t offset;
  };

  struct GroupData {
    RegPair pairs[16];
    uint32_t pairCount;
  };

  Support::Array<GroupData, 2> groups;
  uint32_t sizeTotal;

  Error init(const FuncFrame& frame) noexcept {
    uint32_t offset = 0;

    for (RegGroup group : Support::EnumValues<RegGroup, RegGroup::kGp, RegGroup::kVec>{}) {
      GroupData& data = groups[group];

      uint32_t n = 0;
      uint32_t pairCount = 0;
      RegPair* pairs = data.pairs;

      uint32_t slotSize = frame.saveRestoreRegSize(group);
      uint32_t savedRegs = frame.savedRegs(group);

      if (group == RegGroup::kGp && frame.hasPreservedFP()) {
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

ASMJIT_FAVOR_SIZE Error EmitHelper::emitProlog(const FuncFrame& frame) {
  Emitter* emitter = _emitter->as<Emitter>();

  PrologEpilogInfo pei;
  ASMJIT_PROPAGATE(pei.init(frame));

  static const Support::Array<Reg, 2> groupRegs = {{ x0, d0 }};
  static const Support::Array<LoadStoreInstructions, 2> groupInsts = {{
    { Inst::kIdStr  , Inst::kIdStp   },
    { Inst::kIdStr_v, Inst::kIdStp_v }
  }};

  uint32_t adjustInitialOffset = pei.sizeTotal;

  for (RegGroup group : Support::EnumValues<RegGroup, RegGroup::kGp, RegGroup::kVec>{}) {
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pairCount = data.pairCount;

    Reg regs[2] = { groupRegs[group], groupRegs[group] };
    Mem mem = ptr(sp);

    const LoadStoreInstructions& insts = groupInsts[group];
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
        ASMJIT_PROPAGATE(emitter->emit(insts.singleInstId, regs[0], mem));
      else
        ASMJIT_PROPAGATE(emitter->emit(insts.pairInstId, regs[0], regs[1], mem));

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

  static const Support::Array<Reg, 2> groupRegs = {{ x0, d0 }};
  static const Support::Array<LoadStoreInstructions, 2> groupInsts = {{
    { Inst::kIdLdr  , Inst::kIdLdp   },
    { Inst::kIdLdr_v, Inst::kIdLdp_v }
  }};

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

  for (int g = 1; g >= 0; g--) {
    RegGroup group = RegGroup(g);
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pairCount = data.pairCount;

    Reg regs[2] = { groupRegs[group], groupRegs[group] };
    Mem mem = ptr(sp);

    const LoadStoreInstructions& insts = groupInsts[group];

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
        ASMJIT_PROPAGATE(emitter->emit(insts.singleInstId, regs[0], mem));
      else
        ASMJIT_PROPAGATE(emitter->emit(insts.pairInstId, regs[0], regs[1], mem));

      mem.resetToFixedOffset();
    }
  }

  ASMJIT_PROPAGATE(emitter->ret(x30));

  return kErrorOk;
}

static Error ASMJIT_CDECL Emitter_emitProlog(BaseEmitter* emitter, const FuncFrame& frame) {
  EmitHelper emitHelper(emitter);
  return emitHelper.emitProlog(frame);
}

static Error ASMJIT_CDECL Emitter_emitEpilog(BaseEmitter* emitter, const FuncFrame& frame) {
  EmitHelper emitHelper(emitter);
  return emitHelper.emitEpilog(frame);
}

static Error ASMJIT_CDECL Emitter_emitArgsAssignment(BaseEmitter* emitter, const FuncFrame& frame, const FuncArgsAssignment& args) {
  EmitHelper emitHelper(emitter);
  return emitHelper.emitArgsAssignment(frame, args);
}

void assignEmitterFuncs(BaseEmitter* emitter) {
  emitter->_funcs.emitProlog = Emitter_emitProlog;
  emitter->_funcs.emitEpilog = Emitter_emitEpilog;
  emitter->_funcs.emitArgsAssignment = Emitter_emitArgsAssignment;

#ifndef ASMJIT_NO_LOGGING
  emitter->_funcs.formatInstruction = FormatterInternal::formatInstruction;
#endif

#ifndef ASMJIT_NO_VALIDATION
  emitter->_funcs.validate = InstInternal::validate;
#endif
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
