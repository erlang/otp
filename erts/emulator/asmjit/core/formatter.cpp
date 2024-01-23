// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_LOGGING

#include "../core/archtraits.h"
#include "../core/builder.h"
#include "../core/codeholder.h"
#include "../core/compiler.h"
#include "../core/emitter.h"
#include "../core/formatter_p.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/type.h"

#if !defined(ASMJIT_NO_X86)
  #include "../x86/x86formatter_p.h"
#endif

#if !defined(ASMJIT_NO_AARCH64)
  #include "../arm/a64formatter_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

#if defined(ASMJIT_NO_COMPILER)
class VirtReg;
#endif

namespace Formatter {

static const char wordNameTable[][8] = {
  "db",
  "dw",
  "dd",
  "dq",
  "byte",
  "half",
  "word",
  "hword",
  "dword",
  "qword",
  "xword",
  "short",
  "long",
  "quad"
};


Error formatTypeId(String& sb, TypeId typeId) noexcept {
  if (typeId == TypeId::kVoid)
    return sb.append("void");

  if (!TypeUtils::isValid(typeId))
    return sb.append("unknown");

  const char* typeName = nullptr;
  uint32_t typeSize = TypeUtils::sizeOf(typeId);
  TypeId scalarType = TypeUtils::scalarOf(typeId);

  switch (scalarType) {
    case TypeId::kIntPtr : typeName = "intptr" ; break;
    case TypeId::kUIntPtr: typeName = "uintptr"; break;
    case TypeId::kInt8   : typeName = "int8"   ; break;
    case TypeId::kUInt8  : typeName = "uint8"  ; break;
    case TypeId::kInt16  : typeName = "int16"  ; break;
    case TypeId::kUInt16 : typeName = "uint16" ; break;
    case TypeId::kInt32  : typeName = "int32"  ; break;
    case TypeId::kUInt32 : typeName = "uint32" ; break;
    case TypeId::kInt64  : typeName = "int64"  ; break;
    case TypeId::kUInt64 : typeName = "uint64" ; break;
    case TypeId::kFloat32: typeName = "float32"; break;
    case TypeId::kFloat64: typeName = "float64"; break;
    case TypeId::kFloat80: typeName = "float80"; break;
    case TypeId::kMask8  : typeName = "mask8"  ; break;
    case TypeId::kMask16 : typeName = "mask16" ; break;
    case TypeId::kMask32 : typeName = "mask32" ; break;
    case TypeId::kMask64 : typeName = "mask64" ; break;
    case TypeId::kMmx32  : typeName = "mmx32"  ; break;
    case TypeId::kMmx64  : typeName = "mmx64"  ; break;

    default:
      typeName = "unknown";
      break;
  }

  uint32_t baseSize = TypeUtils::sizeOf(scalarType);
  if (typeSize > baseSize) {
    uint32_t count = typeSize / baseSize;
    return sb.appendFormat("%sx%u", typeName, unsigned(count));
  }
  else {
    return sb.append(typeName);
  }
}

Error formatFeature(
  String& sb,
  Arch arch,
  uint32_t featureId) noexcept {

#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::FormatterInternal::formatFeature(sb, featureId);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyARM(arch))
    return arm::FormatterInternal::formatFeature(sb, featureId);
#endif

  return kErrorInvalidArch;
}

Error formatLabel(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  uint32_t labelId) noexcept {

  DebugUtils::unused(formatFlags);

  if (emitter && emitter->code()) {
    const LabelEntry* le = emitter->code()->labelEntry(labelId);
    if (ASMJIT_UNLIKELY(!le))
      return sb.appendFormat("<InvalidLabel:%u>", labelId);

    if (le->hasName()) {
      if (le->hasParent()) {
        uint32_t parentId = le->parentId();
        const LabelEntry* pe = emitter->code()->labelEntry(parentId);

        if (ASMJIT_UNLIKELY(!pe))
          ASMJIT_PROPAGATE(sb.appendFormat("<InvalidLabel:%u>", labelId));
        else if (ASMJIT_UNLIKELY(!pe->hasName()))
          ASMJIT_PROPAGATE(sb.appendFormat("L%u", parentId));
        else
          ASMJIT_PROPAGATE(sb.append(pe->name()));

        ASMJIT_PROPAGATE(sb.append('.'));
      }

      if (le->type() == LabelType::kAnonymous)
        ASMJIT_PROPAGATE(sb.appendFormat("L%u@", labelId));
      return sb.append(le->name());
    }
  }

  return sb.appendFormat("L%u", labelId);
}

Error formatRegister(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t regId) noexcept {

#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::FormatterInternal::formatRegister(sb, formatFlags, emitter, arch, regType, regId);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyARM(arch))
    return arm::FormatterInternal::formatRegister(sb, formatFlags, emitter, arch, regType, regId);
#endif

  return kErrorInvalidArch;
}

Error formatOperand(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept {

#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::FormatterInternal::formatOperand(sb, formatFlags, emitter, arch, op);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyARM(arch))
    return arm::FormatterInternal::formatOperand(sb, formatFlags, emitter, arch, op);
#endif

  return kErrorInvalidArch;
}

ASMJIT_API Error formatDataType(
  String& sb,
  FormatFlags formatFlags,
  Arch arch,
  TypeId typeId) noexcept
{
  DebugUtils::unused(formatFlags);

  if (ASMJIT_UNLIKELY(uint32_t(arch) > uint32_t(Arch::kMaxValue)))
    return DebugUtils::errored(kErrorInvalidArch);

  uint32_t typeSize = TypeUtils::sizeOf(typeId);
  if (typeSize == 0 || typeSize > 8)
    return DebugUtils::errored(kErrorInvalidState);

  uint32_t typeSizeLog2 = Support::ctz(typeSize);
  return sb.append(wordNameTable[size_t(ArchTraits::byArch(arch).typeNameIdByIndex(typeSizeLog2))]);
}

static Error formatDataHelper(String& sb, const char* typeName, uint32_t typeSize, const uint8_t* data, size_t itemCount) noexcept {
  sb.append('.');
  sb.append(typeName);
  sb.append(' ');

  for (size_t i = 0; i < itemCount; i++) {
    uint64_t v = 0;

    if (i != 0)
      ASMJIT_PROPAGATE(sb.append(", ", 2));

    switch (typeSize) {
      case 1: v = data[0]; break;
      case 2: v = Support::readU16u(data); break;
      case 4: v = Support::readU32u(data); break;
      case 8: v = Support::readU64u(data); break;
    }

    ASMJIT_PROPAGATE(sb.appendUInt(v, 16, typeSize * 2, StringFormatFlags::kAlternate));
    data += typeSize;
  }

  return kErrorOk;
}

Error formatData(
  String& sb,
  FormatFlags formatFlags,
  Arch arch,
  TypeId typeId, const void* data, size_t itemCount, size_t repeatCount) noexcept
{
  DebugUtils::unused(formatFlags);

  if (ASMJIT_UNLIKELY(!Environment::isDefinedArch(arch)))
    return DebugUtils::errored(kErrorInvalidArch);

  uint32_t typeSize = TypeUtils::sizeOf(typeId);
  if (typeSize == 0)
    return DebugUtils::errored(kErrorInvalidState);

  if (!Support::isPowerOf2(typeSize)) {
    itemCount *= typeSize;
    typeSize = 1;
  }

  while (typeSize > 8u) {
    typeSize >>= 1;
    itemCount <<= 1;
  }

  uint32_t typeSizeLog2 = Support::ctz(typeSize);
  const char* wordName = wordNameTable[size_t(ArchTraits::byArch(arch).typeNameIdByIndex(typeSizeLog2))];

  if (repeatCount > 1)
    ASMJIT_PROPAGATE(sb.appendFormat(".repeat %zu ", repeatCount));

  return formatDataHelper(sb, wordName, typeSize, static_cast<const uint8_t*>(data), itemCount);
}

Error formatInstruction(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept {

#if !defined(ASMJIT_NO_X86)
  if (Environment::isFamilyX86(arch))
    return x86::FormatterInternal::formatInstruction(sb, formatFlags, emitter, arch, inst, operands, opCount);
#endif

#if !defined(ASMJIT_NO_AARCH64)
  if (Environment::isFamilyAArch64(arch))
    return a64::FormatterInternal::formatInstruction(sb, formatFlags, emitter, arch, inst, operands, opCount);
#endif

  return kErrorInvalidArch;
}

#ifndef ASMJIT_NO_BUILDER

#ifndef ASMJIT_NO_COMPILER
static Error formatFuncValue(String& sb, FormatFlags formatFlags, const BaseEmitter* emitter, FuncValue value) noexcept {
  TypeId typeId = value.typeId();
  ASMJIT_PROPAGATE(formatTypeId(sb, typeId));

  if (value.isAssigned()) {
    ASMJIT_PROPAGATE(sb.append('@'));

    if (value.isIndirect())
      ASMJIT_PROPAGATE(sb.append('['));

    // NOTE: It should be either reg or stack, but never both. We
    // use two IFs on purpose so if the FuncValue is both it would
    // show in logs.
    if (value.isReg()) {
      ASMJIT_PROPAGATE(formatRegister(sb, formatFlags, emitter, emitter->arch(), value.regType(), value.regId()));
    }

    if (value.isStack()) {
      ASMJIT_PROPAGATE(sb.appendFormat("[%d]", int(value.stackOffset())));
    }

    if (value.isIndirect())
      ASMJIT_PROPAGATE(sb.append(']'));
  }

  return kErrorOk;
}

static Error formatFuncValuePack(
  String& sb,
  FormatFlags formatFlags,
  const BaseCompiler* cc,
  const FuncValuePack& pack,
  const RegOnly* vRegs) noexcept {

  size_t count = pack.count();
  if (!count)
    return sb.append("void");

  if (count > 1)
    sb.append('[');

  for (uint32_t valueIndex = 0; valueIndex < count; valueIndex++) {
    const FuncValue& value = pack[valueIndex];
    if (!value)
      break;

    if (valueIndex)
      ASMJIT_PROPAGATE(sb.append(", "));

    ASMJIT_PROPAGATE(formatFuncValue(sb, formatFlags, cc, value));

    if (vRegs) {
      const VirtReg* virtReg = nullptr;
      static const char nullReg[] = "<none>";

      if (vRegs[valueIndex].isReg() && cc->isVirtIdValid(vRegs[valueIndex].id()))
        virtReg = cc->virtRegById(vRegs[valueIndex].id());

      ASMJIT_PROPAGATE(sb.appendFormat(" %s", virtReg ? virtReg->name() : nullReg));
    }
  }

  if (count > 1)
    sb.append(']');

  return kErrorOk;
}

static Error formatFuncRets(
  String& sb,
  FormatFlags formatFlags,
  const BaseCompiler* cc,
  const FuncDetail& fd) noexcept {

  return formatFuncValuePack(sb, formatFlags, cc, fd.retPack(), nullptr);
}

static Error formatFuncArgs(
  String& sb,
  FormatFlags formatFlags,
  const BaseCompiler* cc,
  const FuncDetail& fd,
  const FuncNode::ArgPack* argPacks) noexcept {

  uint32_t argCount = fd.argCount();
  if (!argCount)
    return sb.append("void");

  for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
    if (argIndex)
      ASMJIT_PROPAGATE(sb.append(", "));

    ASMJIT_PROPAGATE(formatFuncValuePack(sb, formatFlags, cc, fd.argPack(argIndex), argPacks[argIndex]._data));
  }

  return kErrorOk;
}
#endif

Error formatNode(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder,
  const BaseNode* node) noexcept {

  if (node->hasPosition() && formatOptions.hasFlag(FormatFlags::kPositions))
    ASMJIT_PROPAGATE(sb.appendFormat("<%05u> ", node->position()));

  size_t startLineIndex = sb.size();

  switch (node->type()) {
    case NodeType::kInst:
    case NodeType::kJump: {
      const InstNode* instNode = node->as<InstNode>();
      ASMJIT_PROPAGATE(builder->_funcs.formatInstruction(sb, formatOptions.flags(), builder,
        builder->arch(),
        instNode->baseInst(), instNode->operands(), instNode->opCount()));
      break;
    }

    case NodeType::kSection: {
      const SectionNode* sectionNode = node->as<SectionNode>();
      if (builder->_code->isSectionValid(sectionNode->id())) {
        const Section* section = builder->_code->sectionById(sectionNode->id());
        ASMJIT_PROPAGATE(sb.appendFormat(".section %s", section->name()));
      }
      break;
    }

    case NodeType::kLabel: {
      const LabelNode* labelNode = node->as<LabelNode>();
      ASMJIT_PROPAGATE(formatLabel(sb, formatOptions.flags(), builder, labelNode->labelId()));
      ASMJIT_PROPAGATE(sb.append(":"));
      break;
    }

    case NodeType::kAlign: {
      const AlignNode* alignNode = node->as<AlignNode>();
      ASMJIT_PROPAGATE(sb.appendFormat(".align %u (%s)",
        alignNode->alignment(),
        alignNode->alignMode() == AlignMode::kCode ? "code" : "data"));
      break;
    }

    case NodeType::kEmbedData: {
      const EmbedDataNode* embedNode = node->as<EmbedDataNode>();
      ASMJIT_PROPAGATE(sb.append('.'));
      ASMJIT_PROPAGATE(formatDataType(sb, formatOptions.flags(), builder->arch(), embedNode->typeId()));
      ASMJIT_PROPAGATE(sb.appendFormat(" {Count=%zu Repeat=%zu TotalSize=%zu}", embedNode->itemCount(), embedNode->repeatCount(), embedNode->dataSize()));
      break;
    }

    case NodeType::kEmbedLabel: {
      const EmbedLabelNode* embedNode = node->as<EmbedLabelNode>();
      ASMJIT_PROPAGATE(sb.append(".label "));
      ASMJIT_PROPAGATE(formatLabel(sb, formatOptions.flags(), builder, embedNode->labelId()));
      break;
    }

    case NodeType::kEmbedLabelDelta: {
      const EmbedLabelDeltaNode* embedNode = node->as<EmbedLabelDeltaNode>();
      ASMJIT_PROPAGATE(sb.append(".label ("));
      ASMJIT_PROPAGATE(formatLabel(sb, formatOptions.flags(), builder, embedNode->labelId()));
      ASMJIT_PROPAGATE(sb.append(" - "));
      ASMJIT_PROPAGATE(formatLabel(sb, formatOptions.flags(), builder, embedNode->baseLabelId()));
      ASMJIT_PROPAGATE(sb.append(")"));
      break;
    }

    case NodeType::kConstPool: {
      const ConstPoolNode* constPoolNode = node->as<ConstPoolNode>();
      ASMJIT_PROPAGATE(sb.appendFormat("[ConstPool Size=%zu Alignment=%zu]", constPoolNode->size(), constPoolNode->alignment()));
      break;
    };

    case NodeType::kComment: {
      const CommentNode* commentNode = node->as<CommentNode>();
      return sb.appendFormat("; %s", commentNode->inlineComment());
    }

    case NodeType::kSentinel: {
      const SentinelNode* sentinelNode = node->as<SentinelNode>();
      const char* sentinelName = nullptr;

      switch (sentinelNode->sentinelType()) {
        case SentinelType::kFuncEnd:
          sentinelName = "[FuncEnd]";
          break;

        default:
          sentinelName = "[Sentinel]";
          break;
      }

      ASMJIT_PROPAGATE(sb.append(sentinelName));
      break;
    }

#ifndef ASMJIT_NO_COMPILER
    case NodeType::kFunc: {
      const FuncNode* funcNode = node->as<FuncNode>();

      if (builder->isCompiler()) {
        ASMJIT_PROPAGATE(formatLabel(sb, formatOptions.flags(), builder, funcNode->labelId()));
        ASMJIT_PROPAGATE(sb.append(": "));

        ASMJIT_PROPAGATE(formatFuncRets(sb, formatOptions.flags(), static_cast<const BaseCompiler*>(builder), funcNode->detail()));
        ASMJIT_PROPAGATE(sb.append(" Func("));
        ASMJIT_PROPAGATE(formatFuncArgs(sb, formatOptions.flags(), static_cast<const BaseCompiler*>(builder), funcNode->detail(), funcNode->argPacks()));
        ASMJIT_PROPAGATE(sb.append(")"));
      }
      break;
    }

    case NodeType::kFuncRet: {
      const FuncRetNode* retNode = node->as<FuncRetNode>();
      ASMJIT_PROPAGATE(sb.append("[FuncRet]"));

      for (uint32_t i = 0; i < 2; i++) {
        const Operand_& op = retNode->op(i);
        if (!op.isNone()) {
          ASMJIT_PROPAGATE(sb.append(i == 0 ? " " : ", "));
          ASMJIT_PROPAGATE(formatOperand(sb, formatOptions.flags(), builder, builder->arch(), op));
        }
      }
      break;
    }

    case NodeType::kInvoke: {
      const InvokeNode* invokeNode = node->as<InvokeNode>();
      ASMJIT_PROPAGATE(builder->_funcs.formatInstruction(sb, formatOptions.flags(), builder,
        builder->arch(),
        invokeNode->baseInst(), invokeNode->operands(), invokeNode->opCount()));
      break;
    }
#endif

    default: {
      ASMJIT_PROPAGATE(sb.appendFormat("[UserNode:%u]", node->type()));
      break;
    }
  }

  if (node->hasInlineComment()) {
    size_t requiredPadding = paddingFromOptions(formatOptions, FormatPaddingGroup::kRegularLine);
    size_t currentPadding = sb.size() - startLineIndex;

    if (currentPadding < requiredPadding)
      ASMJIT_PROPAGATE(sb.appendChars(' ', requiredPadding - currentPadding));

    ASMJIT_PROPAGATE(sb.append("; "));
    ASMJIT_PROPAGATE(sb.append(node->inlineComment()));
  }

  return kErrorOk;
}

Error formatNodeList(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder) noexcept {

  return formatNodeList(sb, formatOptions, builder, builder->firstNode(), nullptr);
}

Error formatNodeList(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder,
  const BaseNode* begin,
  const BaseNode* end) noexcept {

  const BaseNode* node = begin;
  while (node != end) {
    ASMJIT_PROPAGATE(formatNode(sb, formatOptions, builder, node));
    ASMJIT_PROPAGATE(sb.append('\n'));
    node = node->next();
  }
  return kErrorOk;
}
#endif

} // {Formatter}

ASMJIT_END_NAMESPACE

#endif
