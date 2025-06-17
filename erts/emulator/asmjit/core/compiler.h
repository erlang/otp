// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_COMPILER_H_INCLUDED
#define ASMJIT_CORE_COMPILER_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/assembler.h"
#include "../core/builder.h"
#include "../core/constpool.h"
#include "../core/compilerdefs.h"
#include "../core/func.h"
#include "../core/inst.h"
#include "../core/operand.h"
#include "../core/support.h"
#include "../core/zone.h"
#include "../core/zonevector.h"

ASMJIT_BEGIN_NAMESPACE

class JumpAnnotation;
class JumpNode;
class FuncNode;
class FuncRetNode;
class InvokeNode;

//! \addtogroup asmjit_compiler
//! \{

//! Code emitter that uses virtual registers and performs register allocation.
//!
//! Compiler is a high-level code-generation tool that provides register allocation and automatic handling of function
//! calling conventions. It was primarily designed for merging multiple parts of code into a function without worrying
//! about registers and function calling conventions.
//!
//! BaseCompiler can be used, with a minimum effort, to handle 32-bit and 64-bit code generation within a single code
//! base.
//!
//! BaseCompiler is based on BaseBuilder and contains all the features it provides. It means that the code it stores
//! can be modified (removed, added, injected) and analyzed. When the code is finalized the compiler can emit the code
//! into an Assembler to translate the abstract representation into a machine code.
//!
//! Check out architecture specific compilers for more details and examples:
//!
//!   - \ref x86::Compiler - X86/X64 compiler implementation.
//!   - \ref a64::Compiler - AArch64 compiler implementation.
class ASMJIT_VIRTAPI BaseCompiler : public BaseBuilder {
public:
  ASMJIT_NONCOPYABLE(BaseCompiler)
  typedef BaseBuilder Base;

  //! \name Members
  //! \{

  //! Current function.
  FuncNode* _func;
  //! Allocates `VirtReg` objects.
  Zone _vRegZone;
  //! Stores array of `VirtReg` pointers.
  ZoneVector<VirtReg*> _vRegArray;
  //! Stores jump annotations.
  ZoneVector<JumpAnnotation*> _jumpAnnotations;

  //! Local and global constant pools.
  //!
  //! Local constant pool is flushed with each function, global constant pool is flushed only by \ref finalize().
  ConstPoolNode* _constPools[2];

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `BaseCompiler` instance.
  ASMJIT_API BaseCompiler() noexcept;
  //! Destroys the `BaseCompiler` instance.
  ASMJIT_API ~BaseCompiler() noexcept override;

  //! \}

  //! \name Function Management
  //! \{

  //! Creates a new \ref FuncNode.
  ASMJIT_API Error newFuncNode(FuncNode** ASMJIT_NONNULL(out), const FuncSignature& signature);
  //! Creates a new \ref FuncNode adds it to the instruction stream.
  ASMJIT_API Error addFuncNode(FuncNode** ASMJIT_NONNULL(out), const FuncSignature& signature);

  //! Creates a new \ref FuncRetNode.
  ASMJIT_API Error newFuncRetNode(FuncRetNode** ASMJIT_NONNULL(out), const Operand_& o0, const Operand_& o1);
  //! Creates a new \ref FuncRetNode and adds it to the instruction stream.
  ASMJIT_API Error addFuncRetNode(FuncRetNode** ASMJIT_NONNULL(out), const Operand_& o0, const Operand_& o1);

  //! Returns the current function.
  ASMJIT_INLINE_NODEBUG FuncNode* func() const noexcept { return _func; }

  //! Creates a new \ref FuncNode with the given `signature` and returns it.
  inline FuncNode* newFunc(const FuncSignature& signature) {
    FuncNode* node;
    newFuncNode(&node, signature);
    return node;
  }

  //! Creates a new \ref FuncNode with the given `signature`, adds it to the instruction stream by using
  //! the \ref addFunc(FuncNode*) overload, and returns it.
  inline FuncNode* addFunc(const FuncSignature& signature) {
    FuncNode* node;
    addFuncNode(&node, signature);
    return node;
  }

  //! Adds a function `node` to the instruction stream.
  ASMJIT_API FuncNode* addFunc(FuncNode* ASMJIT_NONNULL(func));
  //! Emits a sentinel that marks the end of the current function.
  ASMJIT_API Error endFunc();

#if !defined(ASMJIT_NO_DEPRECATED)
  inline Error _setArg(size_t argIndex, size_t valueIndex, const BaseReg& reg);

  //! Sets a function argument at `argIndex` to `reg`.
  ASMJIT_DEPRECATED("Setting arguments through Compiler is deprecated, use FuncNode->setArg() instead")
  inline Error setArg(size_t argIndex, const BaseReg& reg) { return _setArg(argIndex, 0, reg); }

  //! Sets a function argument at `argIndex` at `valueIndex` to `reg`.
  ASMJIT_DEPRECATED("Setting arguments through Compiler is deprecated, use FuncNode->setArg() instead")
  inline Error setArg(size_t argIndex, size_t valueIndex, const BaseReg& reg) { return _setArg(argIndex, valueIndex, reg); }
#endif

  inline Error addRet(const Operand_& o0, const Operand_& o1) {
    FuncRetNode* node;
    return addFuncRetNode(&node, o0, o1);
  }

  //! \}

  //! \name Function Invocation
  //! \{

  //! Creates a new \ref InvokeNode.
  ASMJIT_API Error newInvokeNode(InvokeNode** ASMJIT_NONNULL(out), InstId instId, const Operand_& o0, const FuncSignature& signature);
  //! Creates a new \ref InvokeNode and adds it to the instruction stream.
  ASMJIT_API Error addInvokeNode(InvokeNode** ASMJIT_NONNULL(out), InstId instId, const Operand_& o0, const FuncSignature& signature);

  //! \}

  //! \name Virtual Registers
  //! \{

  //! Creates a new virtual register representing the given `typeId` and `signature`.
  //!
  //! \note This function is public, but it's not generally recommended to be used by AsmJit users, use architecture
  //! specific `newReg()` functionality instead or functions like \ref _newReg() and \ref _newRegFmt().
  ASMJIT_API Error newVirtReg(VirtReg** ASMJIT_NONNULL(out), TypeId typeId, OperandSignature signature, const char* name);

  //! Creates a new virtual register of the given `typeId` and stores it to `out` operand.
  ASMJIT_API Error _newReg(BaseReg* ASMJIT_NONNULL(out), TypeId typeId, const char* name = nullptr);

  //! Creates a new virtual register of the given `typeId` and stores it to `out` operand.
  //!
  //! \note This version accepts a snprintf() format `fmt` followed by a variadic arguments.
  ASMJIT_API Error _newRegFmt(BaseReg* ASMJIT_NONNULL(out), TypeId typeId, const char* fmt, ...);
  //! \overload
  inline Error _newRegFmt(BaseReg* ASMJIT_NONNULL(out), TypeId typeId) { return _newRegFmt(out, typeId, nullptr); }

  //! Creates a new virtual register compatible with the provided reference register `ref`.
  ASMJIT_API Error _newReg(BaseReg* ASMJIT_NONNULL(out), const BaseReg& ref, const char* name = nullptr);

  //! Creates a new virtual register compatible with the provided reference register `ref`.
  //!
  //! \note This version accepts a snprintf() format `fmt` followed by a variadic arguments.
  ASMJIT_API Error _newRegFmt(BaseReg* ASMJIT_NONNULL(out), const BaseReg& ref, const char* fmt, ...);

  //! Tests whether the given `id` is a valid virtual register id.
  ASMJIT_INLINE_NODEBUG bool isVirtIdValid(uint32_t id) const noexcept {
    uint32_t index = Operand::virtIdToIndex(id);
    return index < _vRegArray.size();
  }
  //! Tests whether the given `reg` is a virtual register having a valid id.
  ASMJIT_INLINE_NODEBUG bool isVirtRegValid(const BaseReg& reg) const noexcept {
    return isVirtIdValid(reg.id());
  }

  //! Returns \ref VirtReg associated with the given `id`.
  inline VirtReg* virtRegById(uint32_t id) const noexcept {
    ASMJIT_ASSERT(isVirtIdValid(id));
    return _vRegArray[Operand::virtIdToIndex(id)];
  }

  //! Returns \ref VirtReg associated with the given `reg`.
  ASMJIT_INLINE_NODEBUG VirtReg* virtRegByReg(const BaseReg& reg) const noexcept { return virtRegById(reg.id()); }

  //! Returns \ref VirtReg associated with the given virtual register `index`.
  //!
  //! \note This is not the same as virtual register id. The conversion between id and its index is implemented
  //! by \ref Operand_::virtIdToIndex() and \ref Operand_::indexToVirtId() functions.
  ASMJIT_INLINE_NODEBUG VirtReg* virtRegByIndex(uint32_t index) const noexcept { return _vRegArray[index]; }

  //! Returns an array of all virtual registers managed by the Compiler.
  ASMJIT_INLINE_NODEBUG const ZoneVector<VirtReg*>& virtRegs() const noexcept { return _vRegArray; }

  //! \name Stack
  //! \{

  //! Creates a new stack of the given `size` and `alignment` and stores it to `out`.
  //!
  //! \note `name` can be used to give the stack a name, for debugging purposes.
  ASMJIT_API Error _newStack(BaseMem* ASMJIT_NONNULL(out), uint32_t size, uint32_t alignment, const char* name = nullptr);

  //! Updates the stack size of a stack created by `_newStack()` by its `virtId`.
  ASMJIT_API Error setStackSize(uint32_t virtId, uint32_t newSize, uint32_t newAlignment = 0);

  //! Updates the stack size of a stack created by `_newStack()`.
  ASMJIT_INLINE_NODEBUG Error setStackSize(const BaseMem& mem, uint32_t newSize, uint32_t newAlignment = 0) {
    return setStackSize(mem.id(), newSize, newAlignment);
  }

  //! \}

  //! \name Constants
  //! \{

  //! Creates a new constant of the given `scope` (see \ref ConstPoolScope).
  //!
  //! This function adds a constant of the given `size` to the built-in \ref ConstPool and stores the reference to that
  //! constant to the `out` operand.
  ASMJIT_API Error _newConst(BaseMem* ASMJIT_NONNULL(out), ConstPoolScope scope, const void* data, size_t size);

  //! \}

  //! \name Miscellaneous
  //! \{

  //! Rename the given virtual register `reg` to a formatted string `fmt`.
  ASMJIT_API void rename(const BaseReg& reg, const char* fmt, ...);

  //! \}

  //! \name Jump Annotations
  //! \{

  ASMJIT_INLINE_NODEBUG const ZoneVector<JumpAnnotation*>& jumpAnnotations() const noexcept {
    return _jumpAnnotations;
  }

  ASMJIT_API Error newJumpNode(JumpNode** ASMJIT_NONNULL(out), InstId instId, InstOptions instOptions, const Operand_& o0, JumpAnnotation* annotation);
  ASMJIT_API Error emitAnnotatedJump(InstId instId, const Operand_& o0, JumpAnnotation* annotation);

  //! Returns a new `JumpAnnotation` instance, which can be used to aggregate possible targets of a jump where the
  //! target is not a label, for example to implement jump tables.
  ASMJIT_API JumpAnnotation* newJumpAnnotation();

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error onAttach(CodeHolder* code) noexcept override;
  ASMJIT_API Error onDetach(CodeHolder* code) noexcept override;

  //! \}
};

//! Jump annotation used to annotate jumps.
//!
//! \ref BaseCompiler allows to emit jumps where the target is either register or memory operand. Such jumps cannot be
//! trivially inspected, so instead of doing heuristics AsmJit allows to annotate such jumps with possible targets.
//! Register allocator then uses the annotation to construct control-flow, which is then used by liveness analysis and
//! other tools to prepare ground for register allocation.
class JumpAnnotation {
public:
  ASMJIT_NONCOPYABLE(JumpAnnotation)

  //! \name Members
  //! \{

  //! Compiler that owns this JumpAnnotation.
  BaseCompiler* _compiler;
  //! Annotation identifier.
  uint32_t _annotationId;
  //! Vector of label identifiers, see \ref labelIds().
  ZoneVector<uint32_t> _labelIds;

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG JumpAnnotation(BaseCompiler* ASMJIT_NONNULL(compiler), uint32_t annotationId) noexcept
    : _compiler(compiler),
      _annotationId(annotationId) {}

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the compiler that owns this JumpAnnotation.
  ASMJIT_INLINE_NODEBUG BaseCompiler* compiler() const noexcept { return _compiler; }
  //! Returns the annotation id.
  ASMJIT_INLINE_NODEBUG uint32_t annotationId() const noexcept { return _annotationId; }
  //! Returns a vector of label identifiers that lists all targets of the jump.
  ASMJIT_INLINE_NODEBUG const ZoneVector<uint32_t>& labelIds() const noexcept { return _labelIds; }

  //! Tests whether the given `label` is a target of this JumpAnnotation.
  ASMJIT_INLINE_NODEBUG bool hasLabel(const Label& label) const noexcept { return hasLabelId(label.id()); }
  //! Tests whether the given `labelId` is a target of this JumpAnnotation.
  ASMJIT_INLINE_NODEBUG bool hasLabelId(uint32_t labelId) const noexcept { return _labelIds.contains(labelId); }

  //! \}

  //! \name Annotation Building API
  //! \{

  //! Adds the `label` to the list of targets of this JumpAnnotation.
  ASMJIT_INLINE_NODEBUG Error addLabel(const Label& label) noexcept { return addLabelId(label.id()); }
  //! Adds the `labelId` to the list of targets of this JumpAnnotation.
  ASMJIT_INLINE_NODEBUG Error addLabelId(uint32_t labelId) noexcept { return _labelIds.append(&_compiler->_allocator, labelId); }

  //! \}
};

//! Jump instruction with \ref JumpAnnotation.
//!
//! \note This node should be only used to represent jump where the jump target cannot be deduced by examining
//! instruction operands. For example if the jump target is register or memory location. This pattern is often
//! used to perform indirect jumps that use jump table, e.g. to implement `switch{}` statement.
class JumpNode : public InstNodeWithOperands<InstNode::kBaseOpCapacity> {
public:
  ASMJIT_NONCOPYABLE(JumpNode)

  //! \name Members
  //! \{

  JumpAnnotation* _annotation;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline JumpNode(BaseCompiler* ASMJIT_NONNULL(cc), InstId instId, InstOptions options, uint32_t opCount, JumpAnnotation* annotation) noexcept
    : InstNodeWithOperands(cc, instId, options, opCount),
      _annotation(annotation) {
    setType(NodeType::kJump);
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether this JumpNode has associated a \ref JumpAnnotation.
  ASMJIT_INLINE_NODEBUG bool hasAnnotation() const noexcept { return _annotation != nullptr; }
  //! Returns the \ref JumpAnnotation associated with this jump, or `nullptr`.
  ASMJIT_INLINE_NODEBUG JumpAnnotation* annotation() const noexcept { return _annotation; }
  //! Sets the \ref JumpAnnotation associated with this jump to `annotation`.
  ASMJIT_INLINE_NODEBUG void setAnnotation(JumpAnnotation* annotation) noexcept { _annotation = annotation; }

  //! \}
};

//! Function node represents a function used by \ref BaseCompiler.
//!
//! A function is composed of the following:
//!
//!   - Function entry, \ref FuncNode acts as a label, so the entry is implicit. To get the entry, simply use
//!     \ref FuncNode::label(), which is the same as \ref LabelNode::label().
//!
//!   - Function exit, which is represented by \ref FuncNode::exitNode(). A helper function
//!     \ref FuncNode::exitLabel() exists and returns an exit label instead of node.
//!
//!   - Function \ref FuncNode::endNode() sentinel. This node marks the end of a function - there should be no
//!     code that belongs to the function after this node, but the Compiler doesn't enforce that at the moment.
//!
//!   - Function detail, see \ref FuncNode::detail().
//!
//!   - Function frame, see \ref FuncNode::frame().
//!
//!   - Function arguments mapped to virtual registers, see \ref FuncNode::argPacks().
//!
//! In a node list, the function and its body looks like the following:
//!
//! \code{.unparsed}
//! [...]       - Anything before the function.
//!
//! [FuncNode]  - Entry point of the function, acts as a label as well.
//!   <Prolog>  - Prolog inserted by the register allocator.
//!   {...}     - Function body - user code basically.
//! [ExitLabel] - Exit label
//!   <Epilog>  - Epilog inserted by the register allocator.
//!   <Return>  - Return inserted by the register allocator.
//!   {...}     - Can contain data or user code (error handling, special cases, ...).
//! [FuncEnd]   - End sentinel
//!
//! [...]       - Anything after the function.
//! \endcode
//!
//! When a function is added to the instruction stream by \ref BaseCompiler::addFunc() it actually inserts 3 nodes
//! (FuncNode, ExitLabel, and FuncEnd) and sets the current cursor to be FuncNode. When \ref BaseCompiler::endFunc()
//! is called the cursor is set to FuncEnd. This guarantees that user can use ExitLabel as a marker after additional
//! code or data can be placed, which is a common practice.
class FuncNode : public LabelNode {
public:
  ASMJIT_NONCOPYABLE(FuncNode)

  //! Arguments pack.
  struct ArgPack {
    RegOnly _data[Globals::kMaxValuePack];

    inline void reset() noexcept {
      for (size_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++)
        _data[valueIndex].reset();
    }

    inline RegOnly& operator[](size_t valueIndex) noexcept { return _data[valueIndex]; }
    inline const RegOnly& operator[](size_t valueIndex) const noexcept { return _data[valueIndex]; }
  };

  //! \name Members
  //! \{

  //! Function detail.
  FuncDetail _funcDetail;
  //! Function frame.
  FuncFrame _frame;
  //! Function exit label.
  LabelNode* _exitNode;
  //! Function end (sentinel).
  SentinelNode* _end;
  //! Argument packs.
  ArgPack* _args;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `FuncNode` instance.
  //!
  //! Always use `BaseCompiler::addFunc()` to create a new `FuncNode`.
  inline FuncNode(BaseBuilder* ASMJIT_NONNULL(cb)) noexcept
    : LabelNode(cb),
      _funcDetail(),
      _frame(),
      _exitNode(nullptr),
      _end(nullptr),
      _args(nullptr) {
    setType(NodeType::kFunc);
  }

  //! \}

  //! \{
  //! \name Accessors

  //! Returns function exit `LabelNode`.
  ASMJIT_INLINE_NODEBUG LabelNode* exitNode() const noexcept { return _exitNode; }
  //! Returns function exit label.
  ASMJIT_INLINE_NODEBUG Label exitLabel() const noexcept { return _exitNode->label(); }

  //! Returns "End of Func" sentinel node.
  ASMJIT_INLINE_NODEBUG SentinelNode* endNode() const noexcept { return _end; }

  //! Returns function detail.
  ASMJIT_INLINE_NODEBUG FuncDetail& detail() noexcept { return _funcDetail; }
  //! Returns function detail.
  ASMJIT_INLINE_NODEBUG const FuncDetail& detail() const noexcept { return _funcDetail; }

  //! Returns function frame.
  ASMJIT_INLINE_NODEBUG FuncFrame& frame() noexcept { return _frame; }
  //! Returns function frame.
  ASMJIT_INLINE_NODEBUG const FuncFrame& frame() const noexcept { return _frame; }

  //! Returns function attributes.
  ASMJIT_INLINE_NODEBUG FuncAttributes attributes() const noexcept { return _frame.attributes(); }
  //! Adds `attrs` to the function attributes.
  ASMJIT_INLINE_NODEBUG void addAttributes(FuncAttributes attrs) noexcept { _frame.addAttributes(attrs); }

  //! Returns arguments count.
  ASMJIT_INLINE_NODEBUG uint32_t argCount() const noexcept { return _funcDetail.argCount(); }
  //! Returns argument packs.
  ASMJIT_INLINE_NODEBUG ArgPack* argPacks() const noexcept { return _args; }

  //! Tests whether the function has a return value.
  ASMJIT_INLINE_NODEBUG bool hasRet() const noexcept { return _funcDetail.hasRet(); }

  //! Returns argument pack at `argIndex`.
  inline ArgPack& argPack(size_t argIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    return _args[argIndex];
  }

  //! Sets argument at `argIndex`.
  inline void setArg(size_t argIndex, const BaseReg& vReg) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][0].init(vReg);
  }

  //! \overload
  inline void setArg(size_t argIndex, const RegOnly& vReg) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][0].init(vReg);
  }

  //! Sets argument at `argIndex` and `valueIndex`.
  inline void setArg(size_t argIndex, size_t valueIndex, const BaseReg& vReg) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][valueIndex].init(vReg);
  }

  //! \overload
  inline void setArg(size_t argIndex, size_t valueIndex, const RegOnly& vReg) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][valueIndex].init(vReg);
  }

  //! Resets argument pack at `argIndex`.
  inline void resetArg(size_t argIndex) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex].reset();
  }

  //! Resets argument pack at `argIndex`.
  inline void resetArg(size_t argIndex, size_t valueIndex) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][valueIndex].reset();
  }

  //! \}
};

//! Function return, used by \ref BaseCompiler.
class FuncRetNode : public InstNodeWithOperands<InstNode::kBaseOpCapacity> {
public:
  ASMJIT_NONCOPYABLE(FuncRetNode)

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `FuncRetNode` instance.
  inline FuncRetNode(BaseBuilder* ASMJIT_NONNULL(cb)) noexcept
    : InstNodeWithOperands(cb, BaseInst::kIdAbstract, InstOptions::kNone, 0) {
    _any._nodeType = NodeType::kFuncRet;
  }

  //! \}
};

//! Function invocation, used by \ref BaseCompiler.
class InvokeNode : public InstNodeWithOperands<InstNode::kBaseOpCapacity> {
public:
  ASMJIT_NONCOPYABLE(InvokeNode)

  //! Operand pack provides multiple operands that can be associated with a single return value of function
  //! argument. Sometimes this is necessary to express an argument or return value that requires multiple
  //! registers, for example 64-bit value in 32-bit mode or passing / returning homogeneous data structures.
  struct OperandPack {
    //! Operands.
    Operand_ _data[Globals::kMaxValuePack];

    //! Reset the pack by resetting all operands in the pack.
    inline void reset() noexcept {
      for (size_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++)
        _data[valueIndex].reset();
    }

    //! Returns an operand at the given `valueIndex`.
    inline Operand& operator[](size_t valueIndex) noexcept {
      ASMJIT_ASSERT(valueIndex < Globals::kMaxValuePack);
      return _data[valueIndex].as<Operand>();
    }

    //! Returns an operand at the given `valueIndex` (const).
    const inline Operand& operator[](size_t valueIndex) const noexcept {
      ASMJIT_ASSERT(valueIndex < Globals::kMaxValuePack);
      return _data[valueIndex].as<Operand>();
    }
  };

  //! \name Members
  //! \{

  //! Function detail.
  FuncDetail _funcDetail;
  //! Function return value(s).
  OperandPack _rets;
  //! Function arguments.
  OperandPack* _args;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `InvokeNode` instance.
  inline InvokeNode(BaseBuilder* ASMJIT_NONNULL(cb), InstId instId, InstOptions options) noexcept
    : InstNodeWithOperands(cb, instId, options, 0),
      _funcDetail(),
      _args(nullptr) {
    setType(NodeType::kInvoke);
    _resetOps();
    _rets.reset();
    addFlags(NodeFlags::kIsRemovable);
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Sets the function signature.
  inline Error init(const FuncSignature& signature, const Environment& environment) noexcept {
    return _funcDetail.init(signature, environment);
  }

  //! Returns the function detail.
  ASMJIT_INLINE_NODEBUG FuncDetail& detail() noexcept { return _funcDetail; }
  //! Returns the function detail.
  ASMJIT_INLINE_NODEBUG const FuncDetail& detail() const noexcept { return _funcDetail; }

  //! Returns the target operand.
  ASMJIT_INLINE_NODEBUG Operand& target() noexcept { return op(0); }
  //! \overload
  ASMJIT_INLINE_NODEBUG const Operand& target() const noexcept { return op(0); }

  //! Returns the number of function return values.
  ASMJIT_INLINE_NODEBUG bool hasRet() const noexcept { return _funcDetail.hasRet(); }
  //! Returns the number of function arguments.
  ASMJIT_INLINE_NODEBUG uint32_t argCount() const noexcept { return _funcDetail.argCount(); }

  //! Returns operand pack representing function return value(s).
  ASMJIT_INLINE_NODEBUG OperandPack& retPack() noexcept { return _rets; }
  //! Returns operand pack representing function return value(s).
  ASMJIT_INLINE_NODEBUG const OperandPack& retPack() const noexcept { return _rets; }

  //! Returns the return value at the given `valueIndex`.
  ASMJIT_INLINE_NODEBUG Operand& ret(size_t valueIndex = 0) noexcept { return _rets[valueIndex]; }
  //! \overload
  ASMJIT_INLINE_NODEBUG const Operand& ret(size_t valueIndex = 0) const noexcept { return _rets[valueIndex]; }

  //! Returns operand pack representing function return value(s).
  inline OperandPack& argPack(size_t argIndex) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    return _args[argIndex];
  }
  //! \overload
  inline const OperandPack& argPack(size_t argIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    return _args[argIndex];
  }

  //! Returns a function argument at the given `argIndex`.
  inline Operand& arg(size_t argIndex, size_t valueIndex) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    return _args[argIndex][valueIndex];
  }
  //! \overload
  inline const Operand& arg(size_t argIndex, size_t valueIndex) const noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    return _args[argIndex][valueIndex];
  }

  //! Sets the function return value at `i` to `op`.
  inline void _setRet(size_t valueIndex, const Operand_& op) noexcept { _rets[valueIndex] = op; }
  //! Sets the function argument at `i` to `op`.
  inline void _setArg(size_t argIndex, size_t valueIndex, const Operand_& op) noexcept {
    ASMJIT_ASSERT(argIndex < argCount());
    _args[argIndex][valueIndex] = op;
  }

  //! Sets the function return value at `valueIndex` to `reg`.
  ASMJIT_INLINE_NODEBUG void setRet(size_t valueIndex, const BaseReg& reg) noexcept { _setRet(valueIndex, reg); }

  //! Sets the first function argument in a value-pack at `argIndex` to `reg`.
  ASMJIT_INLINE_NODEBUG void setArg(size_t argIndex, const BaseReg& reg) noexcept { _setArg(argIndex, 0, reg); }
  //! Sets the first function argument in a value-pack at `argIndex` to `imm`.
  ASMJIT_INLINE_NODEBUG void setArg(size_t argIndex, const Imm& imm) noexcept { _setArg(argIndex, 0, imm); }

  //! Sets the function argument at `argIndex` and `valueIndex` to `reg`.
  ASMJIT_INLINE_NODEBUG void setArg(size_t argIndex, size_t valueIndex, const BaseReg& reg) noexcept { _setArg(argIndex, valueIndex, reg); }
  //! Sets the function argument at `argIndex` and `valueIndex` to `imm`.
  ASMJIT_INLINE_NODEBUG void setArg(size_t argIndex, size_t valueIndex, const Imm& imm) noexcept { _setArg(argIndex, valueIndex, imm); }

  //! \}
};

//! Function pass extends \ref Pass with \ref FuncPass::runOnFunction().
class ASMJIT_VIRTAPI FuncPass : public Pass {
public:
  ASMJIT_NONCOPYABLE(FuncPass)
  typedef Pass Base;

  //! \name Construction & Destruction
  //! \{

  ASMJIT_API FuncPass(const char* name) noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the associated `BaseCompiler`.
  ASMJIT_INLINE_NODEBUG BaseCompiler* cc() const noexcept { return static_cast<BaseCompiler*>(_cb); }

  //! \}

  //! \name Pass Interface
  //! \{

  //! Calls `runOnFunction()` on each `FuncNode` node found.
  ASMJIT_API Error run(Zone* zone, Logger* logger) override;

  //! Called once per `FuncNode`.
  ASMJIT_API virtual Error runOnFunction(Zone* zone, Logger* logger, FuncNode* func);

  //! \}
};

#if !defined(ASMJIT_NO_DEPRECATED)
inline Error BaseCompiler::_setArg(size_t argIndex, size_t valueIndex, const BaseReg& reg) {
  FuncNode* func = _func;

  if (ASMJIT_UNLIKELY(!func))
    return reportError(DebugUtils::errored(kErrorInvalidState));

  func->setArg(argIndex, valueIndex, reg);
  return kErrorOk;
}
#endif

//! \}

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_CORE_COMPILER_H_INCLUDED
