// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>
#if !defined(ASMJIT_NO_AARCH64)

#include <asmjit/axl/bit_set_ops.h>
#include <asmjit/axl/commons.h>
#include <asmjit/axl/inplace_array.h>
#include <asmjit/core/formatter.h>
#include <asmjit/core/func_args_context_p.h>
#include <asmjit/core/globals.h>
#include <asmjit/core/string.h>
#include <asmjit/core/type.h>
#include <asmjit/arm/a64_emit_helper_p.h>
#include <asmjit/arm/a64_formatter_p.h>
#include <asmjit/arm/a64_inst_api_p.h>
#include <asmjit/arm/a64_operand.h>

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64::EmitHelper - Emit Operations
// =================================

ASMJIT_FAVOR_SIZE Error EmitHelper::emit_reg_move(
  const Operand_& dst_,
  const Operand_& src_, TypeId type_id, const char* comment) {

  Emitter* emitter = _emitter->as<Emitter>();

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(TypeUtils::is_valid(type_id) && !TypeUtils::is_abstract(type_id));

  emitter->set_inline_comment(comment);

  if (dst_.is_reg() && src_.is_mem()) {
    Reg dst(dst_.as<Reg>());
    Mem src(src_.as<Mem>());

    switch (type_id) {
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
        if (TypeUtils::is_float32(type_id) || TypeUtils::is_vec32(type_id)) {
          return emitter->ldr(dst.as<Vec>().s(), src);
        }

        if (TypeUtils::is_float64(type_id) || TypeUtils::is_vec64(type_id)) {
          return emitter->ldr(dst.as<Vec>().d(), src);
        }

        if (TypeUtils::is_vec128(type_id)) {
          return emitter->ldr(dst.as<Vec>().q(), src);
        }

        break;
      }
    }
  }

  if (dst_.is_mem() && src_.is_reg()) {
    Mem dst(dst_.as<Mem>());
    Reg src(src_.as<Reg>());

    switch (type_id) {
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
        if (TypeUtils::is_float32(type_id) || TypeUtils::is_vec32(type_id)) {
          return emitter->str(src.as<Vec>().s(), dst);
        }

        if (TypeUtils::is_float64(type_id) || TypeUtils::is_vec64(type_id)) {
          return emitter->str(src.as<Vec>().d(), dst);
        }

        if (TypeUtils::is_vec128(type_id)) {
          return emitter->str(src.as<Vec>().q(), dst);
        }

        break;
      }
    }
  }

  if (dst_.is_reg() && src_.is_reg()) {
    Reg dst(dst_.as<Reg>());
    Reg src(src_.as<Reg>());

    switch (type_id) {
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
        if (TypeUtils::is_float32(type_id) || TypeUtils::is_vec32(type_id)) {
          return emitter->fmov(dst.as<Vec>().s(), src.as<Vec>().s());
        }

        if (TypeUtils::is_float64(type_id) || TypeUtils::is_vec64(type_id)) {
          return emitter->mov(dst.as<Vec>().b8(), src.as<Vec>().b8());
        }

        if (TypeUtils::is_vec128(type_id)) {
          return emitter->mov(dst.as<Vec>().b16(), src.as<Vec>().b16());
        }

        break;
      }
    }
  }

  emitter->set_inline_comment(nullptr);
  return make_error(Error::kInvalidState);
}

Error EmitHelper::emit_reg_swap(
  const Reg& a,
  const Reg& b, const char* comment) {

  axl::maybe_unused(a, b, comment);
  return make_error(Error::kInvalidState);
}

// TODO: [ARM] EmitArgMove is unfinished.
Error EmitHelper::emit_arg_move(
  const Reg& dst_, TypeId dst_type_id,
  const Operand_& src_, TypeId src_type_id, const char* comment) {

  // Deduce optional `dst_type_id`, which may be `TypeId::kVoid` in some cases.
  if (dst_type_id == TypeId::kVoid) {
    dst_type_id = RegUtils::type_id_of(dst_.reg_type());
  }

  // Invalid or abstract TypeIds are not allowed.
  ASMJIT_ASSERT(TypeUtils::is_valid(dst_type_id) && !TypeUtils::is_abstract(dst_type_id));
  ASMJIT_ASSERT(TypeUtils::is_valid(src_type_id) && !TypeUtils::is_abstract(src_type_id));

  Reg dst(dst_.as<Reg>());
  Operand src(src_);

  uint32_t dst_size = TypeUtils::size_of(dst_type_id);
  uint32_t src_size = TypeUtils::size_of(src_type_id);

  if (TypeUtils::is_int(dst_type_id)) {
    if (TypeUtils::is_int(src_type_id)) {
      uint32_t x = uint32_t(dst_size == 8);

      dst.set_signature(OperandSignature{x ? RegTraits<RegType::kGp64>::kSignature : RegTraits<RegType::kGp32>::kSignature});
      _emitter->set_inline_comment(comment);

      if (src.is_reg()) {
        src.set_signature(dst.signature());
        return _emitter->emit(Inst::kIdMov, dst, src);
      }
      else if (src.is_mem()) {
        InstId inst_id = Inst::kIdNone;
          switch (src_type_id) {
          case TypeId::kInt8: inst_id = Inst::kIdLdrsb; break;
          case TypeId::kUInt8: inst_id = Inst::kIdLdrb; break;
          case TypeId::kInt16: inst_id = Inst::kIdLdrsh; break;
          case TypeId::kUInt16: inst_id = Inst::kIdLdrh; break;
          case TypeId::kInt32: inst_id = x ? Inst::kIdLdrsw : Inst::kIdLdr; break;
          case TypeId::kUInt32: inst_id = Inst::kIdLdr; break;
          case TypeId::kInt64: inst_id = Inst::kIdLdr; break;
          case TypeId::kUInt64: inst_id = Inst::kIdLdr; break;
          default:
            return make_error(Error::kInvalidState);
        }
        return _emitter->emit(inst_id, dst, src);
      }
    }
  }

  if (TypeUtils::is_float(dst_type_id) || TypeUtils::is_vec(dst_type_id)) {
    if (TypeUtils::is_float(src_type_id) || TypeUtils::is_vec(src_type_id)) {
      switch (src_size) {
        case 2: dst.as<Vec>().set_signature(RegTraits<RegType::kVec16>::kSignature); break;
        case 4: dst.as<Vec>().set_signature(RegTraits<RegType::kVec32>::kSignature); break;
        case 8: dst.as<Vec>().set_signature(RegTraits<RegType::kVec64>::kSignature); break;
        case 16: dst.as<Vec>().set_signature(RegTraits<RegType::kVec128>::kSignature); break;
        default:
          return make_error(Error::kInvalidState);
      }

      _emitter->set_inline_comment(comment);

      if (src.is_reg()) {
        InstId inst_id = src_size <= 4 ? Inst::kIdFmov_v : Inst::kIdMov_v;
        src.set_signature(dst.signature());
        return _emitter->emit(inst_id, dst, src);
      }
      else if (src.is_mem()) {
        return _emitter->emit(Inst::kIdLdr_v, dst, src);
      }
    }
  }

  return make_error(Error::kInvalidState);
}

// a64::EmitHelper - Emit Prolog & Epilog
// ======================================

struct LoadStoreInstructions {
  InstId single_inst_id;
  InstId pair_inst_id;
};

struct ABIRegPair {
  uint8_t first, second;
};

static const ABIRegPair darwin_gp_pairs[] = { {19, 20}, {21, 22}, {23, 24}, {25, 26}, {27, 28} };
static const ABIRegPair darwin_vec_pairs[] = { {8, 9}, {10, 11}, {12, 13}, {14, 15} };

struct PrologEpilogInfo {
  struct RegPair {
    uint8_t ids[2];
    uint16_t offset;
  };

  struct GroupData {
    RegPair pairs[16];
    uint32_t pair_count;
  };

  InstId pauth_entry;
  InstId pauth_ret;

  axl::InplaceArray<GroupData, 2> groups;
  uint32_t size_total;

  Error init(const FuncFrame& frame) noexcept {
    pauth_entry = Inst::kIdNone;
    pauth_ret = Inst::kIdNone;

    bool has_apple_pauth = frame.has_attribute(FuncAttributes::kAppleABI) &&
                           frame.has_attribute(FuncAttributes::kPtrAuth) &&
                           frame.has_func_calls();

    if (has_apple_pauth) {
      pauth_entry = Inst::kIdPacibsp;
      pauth_ret = Inst::kIdRetab;
    }

    uint32_t offset = 0;

    for (RegGroup group : axl::enumerate(RegGroup::kGp, RegGroup::kVec)) {
      GroupData& data = groups[group];

      uint32_t n = 0;
      uint32_t pair_count = 0;
      RegPair* pairs = data.pairs;

      uint32_t slot_size = frame.save_restore_reg_size(group);
      RegMask saved_regs = frame.saved_regs(group);

      if (group == RegGroup::kGp) {
        if (frame.has_preserved_fp() || (frame.dirty_regs(RegGroup::kGp) & axl::bit_mask<RegMask>(Gp::kIdLr, Gp::kIdFp)) != 0u) {
          // Must be at the beginning of the push/pop sequence.
          ASMJIT_ASSERT(pair_count == 0);

          pairs[0].offset = uint16_t(offset);
          pairs[0].ids[0] = Gp::kIdFp;
          pairs[0].ids[1] = Gp::kIdLr;

          offset += slot_size * 2;
          pair_count++;
          saved_regs &= ~axl::bit_mask<RegMask>(Gp::kIdFp, Gp::kIdLr);
        }
      }

      if (group <= RegGroup::kVec && frame.has_attribute(FuncAttributes::kAppleABI)) {
        const ABIRegPair* abi_pairs = (group == RegGroup::kGp) ? darwin_gp_pairs : darwin_vec_pairs;
        size_t abi_pair_count = (group == RegGroup::kGp) ? ASMJIT_ARRAY_SIZE(darwin_gp_pairs) : ASMJIT_ARRAY_SIZE(darwin_vec_pairs);

        for (size_t i = 0; i < abi_pair_count; i++) {
          RegMask mask = axl::bit_mask<RegMask>(abi_pairs[i].first, abi_pairs[i].second);
          if (saved_regs & mask) {
            pairs[pair_count].offset = uint16_t(offset);
            pairs[pair_count].ids[0] = abi_pairs[i].first;
            pairs[pair_count].ids[1] = abi_pairs[i].second;

            offset += slot_size * 2;
            pair_count++;
            saved_regs &= ~mask;
          }
        }
      }

      axl::BitWordIterator<uint32_t> it(saved_regs);
      while (it.has_next()) {
        pairs[pair_count].ids[n] = uint8_t(it.next());

        if (++n == 2) {
          pairs[pair_count].offset = uint16_t(offset);
          offset += slot_size * 2;

          n = 0;
          pair_count++;
        }
      }

      if (n == 1) {
        pairs[pair_count].ids[1] = uint8_t(Reg::kIdBad);
        pairs[pair_count].offset = uint16_t(offset);
        offset += slot_size * 2;
        pair_count++;
      }

      data.pair_count = pair_count;
    }

    size_total = offset;
    return Error::kOk;
  }
};

ASMJIT_FAVOR_SIZE Error EmitHelper::emit_prolog(const FuncFrame& frame) {
  Emitter* emitter = _emitter->as<Emitter>();

  PrologEpilogInfo pei;
  ASMJIT_PROPAGATE(pei.init(frame));

  static const axl::InplaceArray<Reg, 2> group_regs = {{ x0, d0 }};
  static const axl::InplaceArray<LoadStoreInstructions, 2> group_insts = {{
    { Inst::kIdStr  , Inst::kIdStp   },
    { Inst::kIdStr_v, Inst::kIdStp_v }
  }};

  // Emit: 'bti {jc}' (indirect branch protection).
  if (frame.has_indirect_branch_protection()) {
    ASMJIT_PROPAGATE(emitter->bti(Predicate::BTI::kJC));
  }

  if (pei.pauth_entry != Inst::kIdNone) {
    ASMJIT_PROPAGATE(emitter->emit(pei.pauth_entry));
  }

  uint32_t adjust_initial_offset = pei.size_total;

  for (RegGroup group : axl::enumerate(RegGroup::kGp, RegGroup::kVec)) {
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pair_count = data.pair_count;

    Reg regs[2] = { group_regs[group], group_regs[group] };
    Mem mem = ptr(sp);

    const LoadStoreInstructions& insts = group_insts[group];
    for (uint32_t i = 0; i < pair_count; i++) {
      const PrologEpilogInfo::RegPair& pair = data.pairs[i];

      regs[0].set_id(pair.ids[0]);
      regs[1].set_id(pair.ids[1]);
      mem.set_offset_lo32(pair.offset);

      if (pair.offset == 0 && adjust_initial_offset) {
        mem.set_offset(-int(adjust_initial_offset));
        mem.make_pre_index();
      }

      if (pair.ids[1] == Reg::kIdBad) {
        ASMJIT_PROPAGATE(emitter->emit(insts.single_inst_id, regs[0], mem));
      }
      else {
        ASMJIT_PROPAGATE(emitter->emit(insts.pair_inst_id, regs[0], regs[1], mem));
      }

      mem.reset_offset_mode();

      if (i == 0 && frame.has_preserved_fp()) {
        ASMJIT_PROPAGATE(emitter->mov(x29, sp));
      }
    }
  }

  if (frame.has_stack_adjustment()) {
    uint32_t adj = frame.stack_adjustment();
    if (adj <= 0xFFFu) {
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj));
    }
    else if (adj <= 0xFFFFFFu)  {
      // TODO: [ARM] Prolog - we must touch the pages otherwise it's undefined.
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj & 0x000FFFu));
      ASMJIT_PROPAGATE(emitter->sub(sp, sp, adj & 0xFFF000u));
    }
    else {
      return make_error(Error::kInvalidState);
    }
  }

  return Error::kOk;
}

// TODO: [ARM] Emit epilog.
ASMJIT_FAVOR_SIZE Error EmitHelper::emit_epilog(const FuncFrame& frame) {
  Emitter* emitter = _emitter->as<Emitter>();

  PrologEpilogInfo pei;
  ASMJIT_PROPAGATE(pei.init(frame));

  static const axl::InplaceArray<Reg, 2> group_regs = {{ x0, d0 }};
  static const axl::InplaceArray<LoadStoreInstructions, 2> group_insts = {{
    { Inst::kIdLdr  , Inst::kIdLdp   },
    { Inst::kIdLdr_v, Inst::kIdLdp_v }
  }};

  uint32_t adjust_initial_offset = pei.size_total;

  if (frame.has_stack_adjustment()) {
    uint32_t adj = frame.stack_adjustment();
    if (adj <= 0xFFFu) {
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj));
    }
    else if (adj <= 0xFFFFFFu)  {
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj & 0x000FFFu));
      ASMJIT_PROPAGATE(emitter->add(sp, sp, adj & 0xFFF000u));
    }
    else {
      return make_error(Error::kInvalidState);
    }
  }

  for (int g = 1; g >= 0; g--) {
    RegGroup group = RegGroup(g);
    const PrologEpilogInfo::GroupData& data = pei.groups[group];
    uint32_t pair_count = data.pair_count;

    Reg regs[2] = { group_regs[group], group_regs[group] };
    Mem mem = ptr(sp);

    const LoadStoreInstructions& insts = group_insts[group];

    for (int i = int(pair_count) - 1; i >= 0; i--) {
      const PrologEpilogInfo::RegPair& pair = data.pairs[i];

      regs[0].set_id(pair.ids[0]);
      regs[1].set_id(pair.ids[1]);
      mem.set_offset_lo32(pair.offset);

      if (pair.offset == 0 && adjust_initial_offset) {
        mem.set_offset(int(adjust_initial_offset));
        mem.make_post_index();
      }

      if (pair.ids[1] == Reg::kIdBad) {
        ASMJIT_PROPAGATE(emitter->emit(insts.single_inst_id, regs[0], mem));
      }
      else {
        ASMJIT_PROPAGATE(emitter->emit(insts.pair_inst_id, regs[0], regs[1], mem));
      }

      mem.reset_offset_mode();
    }
  }

  if (pei.pauth_ret != Inst::kIdNone) {
    ASMJIT_PROPAGATE(emitter->emit(pei.pauth_ret));
  }
  else {
    ASMJIT_PROPAGATE(emitter->ret(x30));
  }

  return Error::kOk;
}

static Error ASMJIT_CDECL Emitter_emit_prolog(BaseEmitter* emitter, const FuncFrame& frame) {
  EmitHelper emit_helper(emitter);
  return emit_helper.emit_prolog(frame);
}

static Error ASMJIT_CDECL Emitter_emit_epilog(BaseEmitter* emitter, const FuncFrame& frame) {
  EmitHelper emit_helper(emitter);
  return emit_helper.emit_epilog(frame);
}

static Error ASMJIT_CDECL Emitter_emit_args_assignment(BaseEmitter* emitter, const FuncFrame& frame, const FuncArgsAssignment& args) {
  EmitHelper emit_helper(emitter);
  return emit_helper.emit_args_assignment(frame, args);
}

void init_emitter_funcs(BaseEmitter* emitter) noexcept {
  emitter->_funcs.emit_prolog = Emitter_emit_prolog;
  emitter->_funcs.emit_epilog = Emitter_emit_epilog;
  emitter->_funcs.emit_args_assignment = Emitter_emit_args_assignment;

#ifndef ASMJIT_NO_LOGGING
  emitter->_funcs.format_instruction = FormatterInternal::format_instruction;
#endif

#ifndef ASMJIT_NO_INTROSPECTION
  emitter->_funcs.validate = InstInternal::validate;
#endif
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
