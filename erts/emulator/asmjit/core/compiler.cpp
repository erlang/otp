// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/assembler.h"
#include "../core/compiler.h"
#include "../core/cpuinfo.h"
#include "../core/logger.h"
#include "../core/rapass_p.h"
#include "../core/rastack_p.h"
#include "../core/support.h"
#include "../core/type.h"

ASMJIT_BEGIN_NAMESPACE

// GlobalConstPoolPass
// ===================

class GlobalConstPoolPass : public Pass {
public:
  typedef Pass Base;
public:
  ASMJIT_NONCOPYABLE(GlobalConstPoolPass)

  GlobalConstPoolPass() noexcept : Pass("GlobalConstPoolPass") {}

  Error run(Zone* zone, Logger* logger) override {
    DebugUtils::unused(zone, logger);

    // Flush the global constant pool.
    BaseCompiler* compiler = static_cast<BaseCompiler*>(_cb);
    ConstPoolNode* globalConstPool = compiler->_constPools[uint32_t(ConstPoolScope::kGlobal)];

    if (globalConstPool) {
      compiler->addAfter(globalConstPool, compiler->lastNode());
      compiler->_constPools[uint32_t(ConstPoolScope::kGlobal)] = nullptr;
    }

    return kErrorOk;
  }
};

// BaseCompiler - Construction & Destruction
// =========================================

BaseCompiler::BaseCompiler() noexcept
  : BaseBuilder(),
    _func(nullptr),
    _vRegZone(4096 - Zone::kBlockOverhead),
    _vRegArray(),
    _constPools { nullptr, nullptr } {
  _emitterType = EmitterType::kCompiler;
  _validationFlags = ValidationFlags::kEnableVirtRegs;
}
BaseCompiler::~BaseCompiler() noexcept {}

// BaseCompiler - Function Management
// ==================================

Error BaseCompiler::newFuncNode(FuncNode** out, const FuncSignature& signature) {
  *out = nullptr;

  // Create FuncNode together with all the required surrounding nodes.
  FuncNode* funcNode;
  ASMJIT_PROPAGATE(_newNodeT<FuncNode>(&funcNode));
  ASMJIT_PROPAGATE(newLabelNode(&funcNode->_exitNode));
  ASMJIT_PROPAGATE(_newNodeT<SentinelNode>(&funcNode->_end, SentinelType::kFuncEnd));

  // Initialize the function's detail info.
  Error err = funcNode->detail().init(signature, environment());
  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  // If the Target guarantees greater stack alignment than required by the calling convention
  // then override it as we can prevent having to perform dynamic stack alignment
  uint32_t environmentStackAlignment = _environment.stackAlignment();

  if (funcNode->_funcDetail._callConv.naturalStackAlignment() < environmentStackAlignment)
    funcNode->_funcDetail._callConv.setNaturalStackAlignment(environmentStackAlignment);

  // Initialize the function frame.
  err = funcNode->_frame.init(funcNode->_funcDetail);
  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  // Allocate space for function arguments.
  funcNode->_args = nullptr;
  if (funcNode->argCount() != 0) {
    funcNode->_args = _allocator.allocT<FuncNode::ArgPack>(funcNode->argCount() * sizeof(FuncNode::ArgPack));
    if (ASMJIT_UNLIKELY(!funcNode->_args))
      return reportError(DebugUtils::errored(kErrorOutOfMemory));
    memset(funcNode->_args, 0, funcNode->argCount() * sizeof(FuncNode::ArgPack));
  }

  ASMJIT_PROPAGATE(registerLabelNode(funcNode));

  *out = funcNode;
  return kErrorOk;
}

Error BaseCompiler::addFuncNode(FuncNode** out, const FuncSignature& signature) {
  ASMJIT_PROPAGATE(newFuncNode(out, signature));
  ASMJIT_ASSUME(*out != nullptr);

  addFunc(*out);
  return kErrorOk;
}

Error BaseCompiler::newFuncRetNode(FuncRetNode** out, const Operand_& o0, const Operand_& o1) {
  uint32_t opCount = !o1.isNone() ? 2u : !o0.isNone() ? 1u : 0u;
  FuncRetNode* node;

  ASMJIT_PROPAGATE(_newNodeT<FuncRetNode>(&node));
  ASMJIT_ASSUME(node != nullptr);

  node->setOpCount(opCount);
  node->setOp(0, o0);
  node->setOp(1, o1);
  node->resetOpRange(2, node->opCapacity());

  *out = node;
  return kErrorOk;
}

Error BaseCompiler::addFuncRetNode(FuncRetNode** out, const Operand_& o0, const Operand_& o1) {
  ASMJIT_PROPAGATE(newFuncRetNode(out, o0, o1));
  addNode(*out);
  return kErrorOk;
}

FuncNode* BaseCompiler::addFunc(FuncNode* func) {
  _func = func;

  addNode(func);                 // Function node.
  BaseNode* prev = cursor();     // {CURSOR}.
  addNode(func->exitNode());     // Function exit label.
  addNode(func->endNode());      // Function end sentinel.

  _setCursor(prev);
  return func;
}

Error BaseCompiler::endFunc() {
  FuncNode* func = _func;

  if (ASMJIT_UNLIKELY(!func))
    return reportError(DebugUtils::errored(kErrorInvalidState));

  // Add the local constant pool at the end of the function (if exists).
  ConstPoolNode* localConstPool = _constPools[uint32_t(ConstPoolScope::kLocal)];
  if (localConstPool) {
    setCursor(func->endNode()->prev());
    addNode(localConstPool);
    _constPools[uint32_t(ConstPoolScope::kLocal)] = nullptr;
  }

  // Mark as finished.
  _func = nullptr;

  SentinelNode* end = func->endNode();
  setCursor(end);

  return kErrorOk;
}

// BaseCompiler - Function Invocation
// ==================================

Error BaseCompiler::newInvokeNode(InvokeNode** out, InstId instId, const Operand_& o0, const FuncSignature& signature) {
  InvokeNode* node;
  ASMJIT_PROPAGATE(_newNodeT<InvokeNode>(&node, instId, InstOptions::kNone));

  node->setOpCount(1);
  node->setOp(0, o0);
  node->resetOpRange(1, node->opCapacity());

  Error err = node->detail().init(signature, environment());
  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  // Skip the allocation if there are no arguments.
  uint32_t argCount = signature.argCount();
  if (argCount) {
    node->_args = static_cast<InvokeNode::OperandPack*>(_allocator.alloc(argCount * sizeof(InvokeNode::OperandPack)));
    if (!node->_args)
      return reportError(DebugUtils::errored(kErrorOutOfMemory));
    memset(node->_args, 0, argCount * sizeof(InvokeNode::OperandPack));
  }

  *out = node;
  return kErrorOk;
}

Error BaseCompiler::addInvokeNode(InvokeNode** out, InstId instId, const Operand_& o0, const FuncSignature& signature) {
  ASMJIT_PROPAGATE(newInvokeNode(out, instId, o0, signature));
  addNode(*out);
  return kErrorOk;
}

// BaseCompiler - Virtual Registers
// ================================

static void BaseCompiler_assignGenericName(BaseCompiler* self, VirtReg* vReg) {
  uint32_t index = unsigned(Operand::virtIdToIndex(vReg->_id));

  char buf[64];
  int size = snprintf(buf, ASMJIT_ARRAY_SIZE(buf), "%%%u", unsigned(index));

  ASMJIT_ASSERT(size > 0 && size < int(ASMJIT_ARRAY_SIZE(buf)));
  vReg->_name.setData(&self->_dataZone, buf, unsigned(size));
}

Error BaseCompiler::newVirtReg(VirtReg** out, TypeId typeId, OperandSignature signature, const char* name) {
  *out = nullptr;
  uint32_t index = _vRegArray.size();

  if (ASMJIT_UNLIKELY(index >= uint32_t(Operand::kVirtIdCount)))
    return reportError(DebugUtils::errored(kErrorTooManyVirtRegs));

  if (ASMJIT_UNLIKELY(_vRegArray.willGrow(&_allocator) != kErrorOk))
    return reportError(DebugUtils::errored(kErrorOutOfMemory));

  VirtReg* vReg = _vRegZone.allocZeroedT<VirtReg>();
  if (ASMJIT_UNLIKELY(!vReg))
    return reportError(DebugUtils::errored(kErrorOutOfMemory));

  uint32_t size = TypeUtils::sizeOf(typeId);
  uint32_t alignment = Support::min<uint32_t>(size, 64);

  vReg = new(vReg) VirtReg(signature, Operand::indexToVirtId(index), size, alignment, typeId);

#ifndef ASMJIT_NO_LOGGING
  if (name && name[0] != '\0')
    vReg->_name.setData(&_dataZone, name, SIZE_MAX);
  else
    BaseCompiler_assignGenericName(this, vReg);
#else
  DebugUtils::unused(name);
#endif

  _vRegArray.appendUnsafe(vReg);
  *out = vReg;

  return kErrorOk;
}

Error BaseCompiler::_newReg(BaseReg* out, TypeId typeId, const char* name) {
  OperandSignature regSignature;
  out->reset();

  Error err = ArchUtils::typeIdToRegSignature(arch(), typeId, &typeId, &regSignature);
  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  VirtReg* vReg;
  ASMJIT_PROPAGATE(newVirtReg(&vReg, typeId, regSignature, name));
  ASMJIT_ASSUME(vReg != nullptr);

  out->_initReg(regSignature, vReg->id());
  return kErrorOk;
}

Error BaseCompiler::_newRegFmt(BaseReg* out, TypeId typeId, const char* fmt, ...) {
  va_list ap;
  StringTmp<256> sb;

  va_start(ap, fmt);
  sb.appendVFormat(fmt, ap);
  va_end(ap);

  return _newReg(out, typeId, sb.data());
}

Error BaseCompiler::_newReg(BaseReg* out, const BaseReg& ref, const char* name) {
  out->reset();

  OperandSignature regSignature;
  TypeId typeId;

  if (isVirtRegValid(ref)) {
    VirtReg* vRef = virtRegByReg(ref);
    typeId = vRef->typeId();

    // NOTE: It's possible to cast one register type to another if it's the same register group. However, VirtReg
    // always contains the TypeId that was used to create the register. This means that in some cases we may end
    // up having different size of `ref` and `vRef`. In such case we adjust the TypeId to match the `ref` register
    // type instead of the original register type, which should be the expected behavior.
    uint32_t typeSize = TypeUtils::sizeOf(typeId);
    uint32_t refSize = ref.size();

    if (typeSize != refSize) {
      if (TypeUtils::isInt(typeId)) {
        // GP register - change TypeId to match `ref`, but keep sign of `vRef`.
        switch (refSize) {
          case  1: typeId = TypeId(uint32_t(TypeId::kInt8 ) | (uint32_t(typeId) & 1)); break;
          case  2: typeId = TypeId(uint32_t(TypeId::kInt16) | (uint32_t(typeId) & 1)); break;
          case  4: typeId = TypeId(uint32_t(TypeId::kInt32) | (uint32_t(typeId) & 1)); break;
          case  8: typeId = TypeId(uint32_t(TypeId::kInt64) | (uint32_t(typeId) & 1)); break;
          default: typeId = TypeId::kVoid; break;
        }
      }
      else if (TypeUtils::isMmx(typeId)) {
        // MMX register - always use 64-bit.
        typeId = TypeId::kMmx64;
      }
      else if (TypeUtils::isMask(typeId)) {
        // Mask register - change TypeId to match `ref` size.
        switch (refSize) {
          case  1: typeId = TypeId::kMask8; break;
          case  2: typeId = TypeId::kMask16; break;
          case  4: typeId = TypeId::kMask32; break;
          case  8: typeId = TypeId::kMask64; break;
          default: typeId = TypeId::kVoid; break;
        }
      }
      else {
        // Vector register - change TypeId to match `ref` size, keep vector metadata.
        TypeId scalarTypeId = TypeUtils::scalarOf(typeId);
        switch (refSize) {
          case 16: typeId = TypeUtils::scalarToVector(scalarTypeId, TypeId::_kVec128Start); break;
          case 32: typeId = TypeUtils::scalarToVector(scalarTypeId, TypeId::_kVec256Start); break;
          case 64: typeId = TypeUtils::scalarToVector(scalarTypeId, TypeId::_kVec512Start); break;
          default: typeId = TypeId::kVoid; break;
        }
      }

      if (typeId == TypeId::kVoid)
        return reportError(DebugUtils::errored(kErrorInvalidState));
    }
  }
  else {
    typeId = ArchTraits::byArch(arch()).regTypeToTypeId(ref.type());
  }

  Error err = ArchUtils::typeIdToRegSignature(arch(), typeId, &typeId, &regSignature);
  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  VirtReg* vReg;
  ASMJIT_PROPAGATE(newVirtReg(&vReg, typeId, regSignature, name));
  ASMJIT_ASSUME(vReg != nullptr);

  out->_initReg(regSignature, vReg->id());
  return kErrorOk;
}

Error BaseCompiler::_newRegFmt(BaseReg* out, const BaseReg& ref, const char* fmt, ...) {
  va_list ap;
  StringTmp<256> sb;

  va_start(ap, fmt);
  sb.appendVFormat(fmt, ap);
  va_end(ap);

  return _newReg(out, ref, sb.data());
}

Error BaseCompiler::_newStack(BaseMem* out, uint32_t size, uint32_t alignment, const char* name) {
  out->reset();

  if (size == 0)
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  if (alignment == 0)
    alignment = 1;

  if (!Support::isPowerOf2(alignment))
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  if (alignment > 64)
    alignment = 64;

  VirtReg* vReg;
  ASMJIT_PROPAGATE(newVirtReg(&vReg, TypeId::kVoid, OperandSignature{0}, name));
  ASMJIT_ASSUME(vReg != nullptr);

  vReg->_virtSize = size;
  vReg->_isStack = true;
  vReg->_alignment = uint8_t(alignment);

  // Set the memory operand to GPD/GPQ and its id to VirtReg.
  *out = BaseMem(OperandSignature::fromOpType(OperandType::kMem) |
                 OperandSignature::fromMemBaseType(_gpSignature.regType()) |
                 OperandSignature::fromBits(OperandSignature::kMemRegHomeFlag),
                 vReg->id(), 0, 0);
  return kErrorOk;
}

Error BaseCompiler::setStackSize(uint32_t virtId, uint32_t newSize, uint32_t newAlignment) {
  if (!isVirtIdValid(virtId))
    return DebugUtils::errored(kErrorInvalidVirtId);

  if (newAlignment && !Support::isPowerOf2(newAlignment))
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  if (newAlignment > 64)
    newAlignment = 64;

  VirtReg* vReg = virtRegById(virtId);
  if (newSize)
    vReg->_virtSize = newSize;

  if (newAlignment)
    vReg->_alignment = uint8_t(newAlignment);

  // This is required if the RAPass is already running. There is a chance that a stack-slot has been already
  // allocated and in that case it has to be updated as well, otherwise we would allocate wrong amount of memory.
  RAWorkReg* workReg = vReg->_workReg;
  if (workReg && workReg->_stackSlot) {
    workReg->_stackSlot->_size = vReg->_virtSize;
    workReg->_stackSlot->_alignment = vReg->_alignment;
  }

  return kErrorOk;
}

Error BaseCompiler::_newConst(BaseMem* out, ConstPoolScope scope, const void* data, size_t size) {
  out->reset();

  if (uint32_t(scope) > 1)
    return reportError(DebugUtils::errored(kErrorInvalidArgument));

  if (!_constPools[uint32_t(scope)])
    ASMJIT_PROPAGATE(newConstPoolNode(&_constPools[uint32_t(scope)]));

  ConstPoolNode* pool = _constPools[uint32_t(scope)];
  size_t off;
  Error err = pool->add(data, size, off);

  if (ASMJIT_UNLIKELY(err))
    return reportError(err);

  *out = BaseMem(OperandSignature::fromOpType(OperandType::kMem) |
                 OperandSignature::fromMemBaseType(RegType::kLabelTag) |
                 OperandSignature::fromSize(uint32_t(size)),
                 pool->labelId(), 0, int32_t(off));
  return kErrorOk;
}

void BaseCompiler::rename(const BaseReg& reg, const char* fmt, ...) {
  if (!reg.isVirtReg()) return;

  VirtReg* vReg = virtRegById(reg.id());
  if (!vReg) return;

  if (fmt && fmt[0] != '\0') {
    char buf[128];
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(buf, ASMJIT_ARRAY_SIZE(buf), fmt, ap);
    va_end(ap);

    vReg->_name.setData(&_dataZone, buf, SIZE_MAX);
  }
  else {
    BaseCompiler_assignGenericName(this, vReg);
  }
}

// BaseCompiler - Jump Annotations
// ===============================

Error BaseCompiler::newJumpNode(JumpNode** out, InstId instId, InstOptions instOptions, const Operand_& o0, JumpAnnotation* annotation) {
  JumpNode* node = _allocator.allocT<JumpNode>();
  uint32_t opCount = 1;

  *out = node;
  if (ASMJIT_UNLIKELY(!node))
    return reportError(DebugUtils::errored(kErrorOutOfMemory));

  node = new(node) JumpNode(this, instId, instOptions, opCount, annotation);
  node->setOp(0, o0);
  node->resetOpRange(opCount, JumpNode::kBaseOpCapacity);

  return kErrorOk;
}

Error BaseCompiler::emitAnnotatedJump(InstId instId, const Operand_& o0, JumpAnnotation* annotation) {
  InstOptions options = instOptions() | forcedInstOptions();
  RegOnly extra = extraReg();
  const char* comment = inlineComment();

  resetInstOptions();
  resetInlineComment();
  resetExtraReg();

  JumpNode* node;
  ASMJIT_PROPAGATE(newJumpNode(&node, instId, options, o0, annotation));

  node->setExtraReg(extra);
  if (comment)
    node->setInlineComment(static_cast<char*>(_dataZone.dup(comment, strlen(comment), true)));

  addNode(node);
  return kErrorOk;
}

JumpAnnotation* BaseCompiler::newJumpAnnotation() {
  if (_jumpAnnotations.grow(&_allocator, 1) != kErrorOk) {
    reportError(DebugUtils::errored(kErrorOutOfMemory));
    return nullptr;
  }

  uint32_t id = _jumpAnnotations.size();
  JumpAnnotation* jumpAnnotation = _allocator.newT<JumpAnnotation>(this, id);

  if (!jumpAnnotation) {
    reportError(DebugUtils::errored(kErrorOutOfMemory));
    return nullptr;
  }

  _jumpAnnotations.appendUnsafe(jumpAnnotation);
  return jumpAnnotation;
}

// BaseCompiler - Events
// =====================

Error BaseCompiler::onAttach(CodeHolder* code) noexcept {
  ASMJIT_PROPAGATE(Base::onAttach(code));

  const ArchTraits& archTraits = ArchTraits::byArch(code->arch());
  RegType nativeRegType = Environment::is32Bit(code->arch()) ? RegType::kGp32 : RegType::kGp64;
  _gpSignature = archTraits.regTypeToSignature(nativeRegType);

  Error err = addPassT<GlobalConstPoolPass>();
  if (ASMJIT_UNLIKELY(err)) {
    onDetach(code);
    return err;
  }

  return kErrorOk;
}

Error BaseCompiler::onDetach(CodeHolder* code) noexcept {
  _func = nullptr;
  _constPools[uint32_t(ConstPoolScope::kLocal)] = nullptr;
  _constPools[uint32_t(ConstPoolScope::kGlobal)] = nullptr;

  _vRegArray.reset();
  _vRegZone.reset();

  return Base::onDetach(code);
}

// FuncPass - Construction & Destruction
// =====================================

FuncPass::FuncPass(const char* name) noexcept
  : Pass(name) {}

// FuncPass - Run
// ==============

Error FuncPass::run(Zone* zone, Logger* logger) {
  BaseNode* node = cb()->firstNode();
  if (!node) return kErrorOk;

  do {
    if (node->type() == NodeType::kFunc) {
      FuncNode* func = node->as<FuncNode>();
      node = func->endNode();
      ASMJIT_PROPAGATE(runOnFunction(zone, logger, func));
    }

    // Find a function by skipping all nodes that are not `NodeType::kFunc`.
    do {
      node = node->next();
    } while (node && node->type() != NodeType::kFunc);
  } while (node);

  return kErrorOk;
}

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
