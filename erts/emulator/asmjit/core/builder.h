// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_BUILDER_H_INCLUDED
#define ASMJIT_CORE_BUILDER_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_BUILDER

#include "../core/assembler.h"
#include "../core/codeholder.h"
#include "../core/constpool.h"
#include "../core/formatter.h"
#include "../core/inst.h"
#include "../core/operand.h"
#include "../core/string.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../core/zone.h"
#include "../core/zonevector.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_builder
//! \{

class BaseBuilder;
class Pass;

class BaseNode;
class InstNode;
class SectionNode;
class LabelNode;
class AlignNode;
class EmbedDataNode;
class EmbedLabelNode;
class ConstPoolNode;
class CommentNode;
class SentinelNode;
class LabelDeltaNode;

//! Type of node used by \ref BaseBuilder and \ref BaseCompiler.
enum class NodeType : uint8_t {
  //! Invalid node (internal, don't use).
  kNone = 0,

  // [BaseBuilder]

  //! Node is \ref InstNode or \ref InstExNode.
  kInst = 1,
  //! Node is \ref SectionNode.
  kSection = 2,
  //! Node is \ref LabelNode.
  kLabel = 3,
  //! Node is \ref AlignNode.
  kAlign = 4,
  //! Node is \ref EmbedDataNode.
  kEmbedData = 5,
  //! Node is \ref EmbedLabelNode.
  kEmbedLabel = 6,
  //! Node is \ref EmbedLabelDeltaNode.
  kEmbedLabelDelta = 7,
  //! Node is \ref ConstPoolNode.
  kConstPool = 8,
  //! Node is \ref CommentNode.
  kComment = 9,
  //! Node is \ref SentinelNode.
  kSentinel = 10,

  // [BaseCompiler]

  //! Node is \ref JumpNode (acts as InstNode).
  kJump = 15,
  //! Node is \ref FuncNode (acts as LabelNode).
  kFunc = 16,
  //! Node is \ref FuncRetNode (acts as InstNode).
  kFuncRet = 17,
  //! Node is \ref InvokeNode (acts as InstNode).
  kInvoke = 18,

  // [UserDefined]

  //! First id of a user-defined node.
  kUser = 32
};

//! Node flags, specify what the node is and/or does.
enum class NodeFlags : uint8_t {
  //! No flags.
  kNone = 0,
  //! Node is code that can be executed (instruction, label, align, etc...).
  kIsCode = 0x01u,
  //! Node is data that cannot be executed (data, const-pool, etc...).
  kIsData = 0x02u,
  //! Node is informative, can be removed and ignored.
  kIsInformative = 0x04u,
  //! Node can be safely removed if unreachable.
  kIsRemovable = 0x08u,
  //! Node does nothing when executed (label, align, explicit nop).
  kHasNoEffect = 0x10u,
  //! Node is an instruction or acts as it.
  kActsAsInst = 0x20u,
  //! Node is a label or acts as it.
  kActsAsLabel = 0x40u,
  //! Node is active (part of the code).
  kIsActive = 0x80u
};
ASMJIT_DEFINE_ENUM_FLAGS(NodeFlags)

//! Type of the sentinel (purery informative purpose).
enum class SentinelType : uint8_t {
  //! Type of the sentinel is not known.
  kUnknown = 0u,
  //! This is a sentinel used at the end of \ref FuncNode.
  kFuncEnd = 1u
};

//! Builder interface.
//!
//! `BaseBuilder` interface was designed to be used as a \ref BaseAssembler replacement in case pre-processing or
//! post-processing of the generated code is required. The code can be modified during or after code generation.
//! Pre processing or post processing can be done manually or through a \ref Pass object. \ref BaseBuilder stores
//! the emitted code as a double-linked list of nodes, which allows O(1) insertion and removal during processing.
//!
//! Check out architecture specific builders for more details and examples:
//!
//!   - \ref x86::Builder - X86/X64 builder implementation.
class ASMJIT_VIRTAPI BaseBuilder : public BaseEmitter {
public:
  ASMJIT_NONCOPYABLE(BaseBuilder)
  typedef BaseEmitter Base;

  //! \name Members
  //! \{

  //! Base zone used to allocate nodes and passes.
  Zone _codeZone;
  //! Data zone used to allocate data and names.
  Zone _dataZone;
  //! Pass zone, passed to `Pass::run()`.
  Zone _passZone;
  //! Allocator that uses `_codeZone`.
  ZoneAllocator _allocator;

  //! Array of `Pass` objects.
  ZoneVector<Pass*> _passes {};
  //! Maps section indexes to `LabelNode` nodes.
  ZoneVector<SectionNode*> _sectionNodes {};
  //! Maps label indexes to `LabelNode` nodes.
  ZoneVector<LabelNode*> _labelNodes {};

  //! Current node (cursor).
  BaseNode* _cursor = nullptr;
  //! First node of the current section.
  BaseNode* _firstNode = nullptr;
  //! Last node of the current section.
  BaseNode* _lastNode = nullptr;

  //! Flags assigned to each new node.
  NodeFlags _nodeFlags = NodeFlags::kNone;
  //! The sections links are dirty (used internally).
  bool _dirtySectionLinks = false;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `BaseBuilder` instance.
  ASMJIT_API BaseBuilder() noexcept;
  //! Destroys the `BaseBuilder` instance.
  ASMJIT_API virtual ~BaseBuilder() noexcept;

  //! \}

  //! \name Node Management
  //! \{

  //! Returns the first node.
  inline BaseNode* firstNode() const noexcept { return _firstNode; }
  //! Returns the last node.
  inline BaseNode* lastNode() const noexcept { return _lastNode; }

  //! Allocates and instantiates a new node of type `T` and returns its instance. If the allocation fails `nullptr`
  //! is returned.
  //!
  //! The template argument `T` must be a type that is extends \ref BaseNode.
  //!
  //! \remarks The pointer returned (if non-null) is owned by the Builder or Compiler. When the Builder/Compiler
  //! is destroyed it destroys all nodes it created so no manual memory management is required.
  template<typename T, typename... Args>
  inline Error _newNodeT(T** ASMJIT_NONNULL(out), Args&&... args) {
    *out = _allocator.newT<T>(this, std::forward<Args>(args)...);
    if (ASMJIT_UNLIKELY(!*out))
      return reportError(DebugUtils::errored(kErrorOutOfMemory));
    return kErrorOk;
  }

  //! Creates a new \ref InstNode.
  ASMJIT_API Error newInstNode(InstNode** ASMJIT_NONNULL(out), InstId instId, InstOptions instOptions, uint32_t opCount);
  //! Creates a new \ref LabelNode.
  ASMJIT_API Error newLabelNode(LabelNode** ASMJIT_NONNULL(out));
  //! Creates a new \ref AlignNode.
  ASMJIT_API Error newAlignNode(AlignNode** ASMJIT_NONNULL(out), AlignMode alignMode, uint32_t alignment);
  //! Creates a new \ref EmbedDataNode.
  ASMJIT_API Error newEmbedDataNode(EmbedDataNode** ASMJIT_NONNULL(out), TypeId typeId, const void* data, size_t itemCount, size_t repeatCount = 1);
  //! Creates a new \ref ConstPoolNode.
  ASMJIT_API Error newConstPoolNode(ConstPoolNode** ASMJIT_NONNULL(out));
  //! Creates a new \ref CommentNode.
  ASMJIT_API Error newCommentNode(CommentNode** ASMJIT_NONNULL(out), const char* data, size_t size);

  //! Adds `node` after the current and sets the current node to the given `node`.
  ASMJIT_API BaseNode* addNode(BaseNode* ASMJIT_NONNULL(node)) noexcept;
  //! Inserts the given `node` after `ref`.
  ASMJIT_API BaseNode* addAfter(BaseNode* ASMJIT_NONNULL(node), BaseNode* ASMJIT_NONNULL(ref)) noexcept;
  //! Inserts the given `node` before `ref`.
  ASMJIT_API BaseNode* addBefore(BaseNode* ASMJIT_NONNULL(node), BaseNode* ASMJIT_NONNULL(ref)) noexcept;
  //! Removes the given `node`.
  ASMJIT_API BaseNode* removeNode(BaseNode* ASMJIT_NONNULL(node)) noexcept;
  //! Removes multiple nodes.
  ASMJIT_API void removeNodes(BaseNode* first, BaseNode* last) noexcept;

  //! Returns the cursor.
  //!
  //! When the Builder/Compiler is created it automatically creates a '.text' \ref SectionNode, which will be the
  //! initial one. When instructions are added they are always added after the cursor and the cursor is changed
  //! to be that newly added node. Use `setCursor()` to change where new nodes are inserted.
  inline BaseNode* cursor() const noexcept { return _cursor; }

  //! Sets the current node to `node` and return the previous one.
  ASMJIT_API BaseNode* setCursor(BaseNode* node) noexcept;

  //! Sets the current node without returning the previous node.
  //!
  //! Only use this function if you are concerned about performance and want this inlined (for example if you set
  //! the cursor in a loop, etc...).
  inline void _setCursor(BaseNode* node) noexcept { _cursor = node; }

  //! \}

  //! \name Section Management
  //! \{

  //! Returns a vector of SectionNode objects.
  //!
  //! \note If a section of some id is not associated with the Builder/Compiler it would be null, so always check
  //! for nulls if you iterate over the vector.
  inline const ZoneVector<SectionNode*>& sectionNodes() const noexcept {
    return _sectionNodes;
  }

  //! Tests whether the `SectionNode` of the given `sectionId` was registered.
  inline bool hasRegisteredSectionNode(uint32_t sectionId) const noexcept {
    return sectionId < _sectionNodes.size() && _sectionNodes[sectionId] != nullptr;
  }

  //! Returns or creates a `SectionNode` that matches the given `sectionId`.
  //!
  //! \remarks This function will either get the existing `SectionNode` or create it in case it wasn't created before.
  //! You can check whether a section has a registered `SectionNode` by using `BaseBuilder::hasRegisteredSectionNode()`.
  ASMJIT_API Error sectionNodeOf(SectionNode** ASMJIT_NONNULL(out), uint32_t sectionId);

  ASMJIT_API Error section(Section* ASMJIT_NONNULL(section)) override;

  //! Returns whether the section links of active section nodes are dirty. You can update these links by calling
  //! `updateSectionLinks()` in such case.
  inline bool hasDirtySectionLinks() const noexcept { return _dirtySectionLinks; }

  //! Updates links of all active section nodes.
  ASMJIT_API void updateSectionLinks() noexcept;

  //! \}

  //! \name Label Management
  //! \{

  //! Returns a vector of \ref LabelNode nodes.
  //!
  //! \note If a label of some id is not associated with the Builder/Compiler it would be null, so always check for
  //! nulls if you iterate over the vector.
  inline const ZoneVector<LabelNode*>& labelNodes() const noexcept { return _labelNodes; }

  //! Tests whether the `LabelNode` of the given `labelId` was registered.
  inline bool hasRegisteredLabelNode(uint32_t labelId) const noexcept {
    return labelId < _labelNodes.size() && _labelNodes[labelId] != nullptr;
  }

  //! \overload
  inline bool hasRegisteredLabelNode(const Label& label) const noexcept {
    return hasRegisteredLabelNode(label.id());
  }

  //! Gets or creates a \ref LabelNode that matches the given `labelId`.
  //!
  //! \remarks This function will either get the existing `LabelNode` or create it in case it wasn't created before.
  //! You can check whether a label has a registered `LabelNode` by calling \ref BaseBuilder::hasRegisteredLabelNode().
  ASMJIT_API Error labelNodeOf(LabelNode** ASMJIT_NONNULL(out), uint32_t labelId);

  //! \overload
  inline Error labelNodeOf(LabelNode** ASMJIT_NONNULL(out), const Label& label) {
    return labelNodeOf(out, label.id());
  }

  //! Registers this \ref LabelNode (internal).
  //!
  //! This function is used internally to register a newly created `LabelNode` with this instance of Builder/Compiler.
  //! Use \ref labelNodeOf() functions to get back \ref LabelNode from a label or its identifier.
  ASMJIT_API Error registerLabelNode(LabelNode* ASMJIT_NONNULL(node));

  ASMJIT_API Label newLabel() override;
  ASMJIT_API Label newNamedLabel(const char* name, size_t nameSize = SIZE_MAX, LabelType type = LabelType::kGlobal, uint32_t parentId = Globals::kInvalidId) override;
  ASMJIT_API Error bind(const Label& label) override;

  //! \}

  //! \name Passes
  //! \{

  //! Returns a vector of `Pass` instances that will be executed by `runPasses()`.
  inline const ZoneVector<Pass*>& passes() const noexcept { return _passes; }

  //! Allocates and instantiates a new pass of type `T` and returns its instance. If the allocation fails `nullptr` is
  //! returned.
  //!
  //! The template argument `T` must be a type that is extends \ref Pass.
  //!
  //! \remarks The pointer returned (if non-null) is owned by the Builder or Compiler. When the Builder/Compiler is
  //! destroyed it destroys all passes it created so no manual memory management is required.
  template<typename T>
  inline T* newPassT() noexcept { return _codeZone.newT<T>(); }

  //! \overload
  template<typename T, typename... Args>
  inline T* newPassT(Args&&... args) noexcept { return _codeZone.newT<T>(std::forward<Args>(args)...); }

  template<typename T>
  inline Error addPassT() { return addPass(newPassT<T>()); }

  template<typename T, typename... Args>
  inline Error addPassT(Args&&... args) { return addPass(newPassT<T, Args...>(std::forward<Args>(args)...)); }

  //! Returns `Pass` by name.
  //!
  //! If the pass having the given `name` doesn't exist `nullptr` is returned.
  ASMJIT_API Pass* passByName(const char* name) const noexcept;
  //! Adds `pass` to the list of passes.
  ASMJIT_API Error addPass(Pass* pass) noexcept;
  //! Removes `pass` from the list of passes and delete it.
  ASMJIT_API Error deletePass(Pass* pass) noexcept;

  //! Runs all passes in order.
  ASMJIT_API Error runPasses();

  //! \}

  //! \name Emit
  //! \{

  ASMJIT_API Error _emit(InstId instId, const Operand_& o0, const Operand_& o1, const Operand_& o2, const Operand_* opExt) override;

  //! \}

  //! \name Align
  //! \{

  ASMJIT_API Error align(AlignMode alignMode, uint32_t alignment) override;

  //! \}

  //! \name Embed
  //! \{

  ASMJIT_API Error embed(const void* data, size_t dataSize) override;
  ASMJIT_API Error embedDataArray(TypeId typeId, const void* data, size_t count, size_t repeat = 1) override;
  ASMJIT_API Error embedConstPool(const Label& label, const ConstPool& pool) override;

  ASMJIT_API Error embedLabel(const Label& label, size_t dataSize = 0) override;
  ASMJIT_API Error embedLabelDelta(const Label& label, const Label& base, size_t dataSize = 0) override;

  //! \}

  //! \name Comment
  //! \{

  ASMJIT_API Error comment(const char* data, size_t size = SIZE_MAX) override;

  //! \}

  //! \name Serialization
  //! \{

  //! Serializes everything the given emitter `dst`.
  //!
  //! Although not explicitly required the emitter will most probably be of Assembler type. The reason is that
  //! there is no known use of serializing nodes held by Builder/Compiler into another Builder-like emitter.
  ASMJIT_API Error serializeTo(BaseEmitter* dst);

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error onAttach(CodeHolder* code) noexcept override;
  ASMJIT_API Error onDetach(CodeHolder* code) noexcept override;

  //! \}
};

//! Base node.
//!
//! Every node represents a building-block used by \ref BaseBuilder. It can be instruction, data, label, comment,
//! directive, or any other high-level representation that can be transformed to the building blocks mentioned.
//! Every class that inherits \ref BaseBuilder can define its own high-level nodes that can be later lowered to
//! basic nodes like instructions.
class BaseNode {
public:
  ASMJIT_NONCOPYABLE(BaseNode)

  //! \name Members
  //! \{

  union {
    struct {
      //! Previous node.
      BaseNode* _prev;
      //! Next node.
      BaseNode* _next;
    };
    //! Links (an alternative view to previous and next nodes).
    BaseNode* _links[2];
  };

  //! Data shared between all types of nodes.
  struct AnyData {
    //! Node type.
    NodeType _nodeType;
    //! Node flags.
    NodeFlags _nodeFlags;
    //! Not used by BaseNode.
    uint8_t _reserved0;
    //! Not used by BaseNode.
    uint8_t _reserved1;
  };

  //! Data used by \ref AlignNode.
  struct AlignData {
    //! Node type.
    NodeType _nodeType;
    //! Node flags.
    NodeFlags _nodeFlags;
    //! Align mode.
    AlignMode _alignMode;
    //! Not used by AlignNode.
    uint8_t _reserved;
  };

  //! Data used by \ref InstNode.
  struct InstData {
    //! Node type.
    NodeType _nodeType;
    //! Node flags.
    NodeFlags _nodeFlags;
    //! Instruction operands count (used).
    uint8_t _opCount;
    //! Instruction operands capacity (allocated).
    uint8_t _opCapacity;
  };

  //! Data used by \ref EmbedDataNode.
  struct EmbedData {
    //! Node type.
    NodeType _nodeType;
    //! Node flags.
    NodeFlags _nodeFlags;
    //! Type id.
    TypeId _typeId;
    //! Size of `_typeId`.
    uint8_t _typeSize;
  };

  //! Data used by \ref SentinelNode.
  struct SentinelData {
    //! Node type.
    NodeType _nodeType;
    //! Node flags.
    NodeFlags _nodeFlags;
    //! Sentinel type.
    SentinelType _sentinelType;
    //! Not used by BaseNode.
    uint8_t _reserved1;
  };

  //! Data that can have different meaning dependning on \ref NodeType.
  union {
    //! Data useful by any node type.
    AnyData _any;
    //! Data specific to \ref AlignNode.
    AlignData _alignData;
    //! Data specific to \ref InstNode.
    InstData _inst;
    //! Data specific to \ref EmbedDataNode.
    EmbedData _embed;
    //! Data specific to \ref SentinelNode.
    SentinelData _sentinel;
  };

  //! Node position in code (should be unique).
  uint32_t _position;

  //! Value reserved for AsmJit users never touched by AsmJit itself.
  union {
    //! User data as 64-bit integer.
    uint64_t _userDataU64;
    //! User data as pointer.
    void* _userDataPtr;
  };

  //! Data used exclusively by the current `Pass`.
  void* _passData;

  //! Inline comment/annotation or nullptr if not used.
  const char* _inlineComment;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `BaseNode` - always use `BaseBuilder` to allocate nodes.
  inline BaseNode(BaseBuilder* cb, NodeType nodeType, NodeFlags nodeFlags = NodeFlags::kNone) noexcept {
    _prev = nullptr;
    _next = nullptr;
    _any._nodeType = nodeType;
    _any._nodeFlags = nodeFlags | cb->_nodeFlags;
    _any._reserved0 = 0;
    _any._reserved1 = 0;
    _position = 0;
    _userDataU64 = 0;
    _passData = nullptr;
    _inlineComment = nullptr;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Casts this node to `T*`.
  template<typename T>
  inline T* as() noexcept { return static_cast<T*>(this); }
  //! Casts this node to `const T*`.
  template<typename T>
  inline const T* as() const noexcept { return static_cast<const T*>(this); }

  //! Returns previous node or `nullptr` if this node is either first or not
  //! part of Builder/Compiler node-list.
  inline BaseNode* prev() const noexcept { return _prev; }
  //! Returns next node or `nullptr` if this node is either last or not part
  //! of Builder/Compiler node-list.
  inline BaseNode* next() const noexcept { return _next; }

  //! Returns the type of the node, see `NodeType`.
  inline NodeType type() const noexcept { return _any._nodeType; }

  //! Sets the type of the node, see `NodeType` (internal).
  //!
  //! \remarks You should never set a type of a node to anything else than the initial value. This function is only
  //! provided for users that use custom nodes and need to change the type either during construction or later.
  inline void setType(NodeType type) noexcept { _any._nodeType = type; }

  //! Tests whether this node is either `InstNode` or extends it.
  inline bool isInst() const noexcept { return hasFlag(NodeFlags::kActsAsInst); }
  //! Tests whether this node is `SectionNode`.
  inline bool isSection() const noexcept { return type() == NodeType::kSection; }
  //! Tests whether this node is either `LabelNode` or extends it.
  inline bool isLabel() const noexcept { return hasFlag(NodeFlags::kActsAsLabel); }
  //! Tests whether this node is `AlignNode`.
  inline bool isAlign() const noexcept { return type() == NodeType::kAlign; }
  //! Tests whether this node is `EmbedDataNode`.
  inline bool isEmbedData() const noexcept { return type() == NodeType::kEmbedData; }
  //! Tests whether this node is `EmbedLabelNode`.
  inline bool isEmbedLabel() const noexcept { return type() == NodeType::kEmbedLabel; }
  //! Tests whether this node is `EmbedLabelDeltaNode`.
  inline bool isEmbedLabelDelta() const noexcept { return type() == NodeType::kEmbedLabelDelta; }
  //! Tests whether this node is `ConstPoolNode`.
  inline bool isConstPool() const noexcept { return type() == NodeType::kConstPool; }
  //! Tests whether this node is `CommentNode`.
  inline bool isComment() const noexcept { return type() == NodeType::kComment; }
  //! Tests whether this node is `SentinelNode`.
  inline bool isSentinel() const noexcept { return type() == NodeType::kSentinel; }

  //! Tests whether this node is `FuncNode`.
  inline bool isFunc() const noexcept { return type() == NodeType::kFunc; }
  //! Tests whether this node is `FuncRetNode`.
  inline bool isFuncRet() const noexcept { return type() == NodeType::kFuncRet; }
  //! Tests whether this node is `InvokeNode`.
  inline bool isInvoke() const noexcept { return type() == NodeType::kInvoke; }

  //! Returns the node flags.
  inline NodeFlags flags() const noexcept { return _any._nodeFlags; }
  //! Tests whether the node has the given `flag` set.
  inline bool hasFlag(NodeFlags flag) const noexcept { return Support::test(_any._nodeFlags, flag); }
  //! Replaces node flags with `flags`.
  inline void setFlags(NodeFlags flags) noexcept { _any._nodeFlags = flags; }
  //! Adds the given `flags` to node flags.
  inline void addFlags(NodeFlags flags) noexcept { _any._nodeFlags |= flags; }
  //! Clears the given `flags` from node flags.
  inline void clearFlags(NodeFlags flags) noexcept { _any._nodeFlags &= ~flags; }

  //! Tests whether the node is code that can be executed.
  inline bool isCode() const noexcept { return hasFlag(NodeFlags::kIsCode); }
  //! Tests whether the node is data that cannot be executed.
  inline bool isData() const noexcept { return hasFlag(NodeFlags::kIsData); }
  //! Tests whether the node is informative only (is never encoded like comment, etc...).
  inline bool isInformative() const noexcept { return hasFlag(NodeFlags::kIsInformative); }
  //! Tests whether the node is removable if it's in an unreachable code block.
  inline bool isRemovable() const noexcept { return hasFlag(NodeFlags::kIsRemovable); }
  //! Tests whether the node has no effect when executed (label, .align, nop, ...).
  inline bool hasNoEffect() const noexcept { return hasFlag(NodeFlags::kHasNoEffect); }
  //! Tests whether the node is part of the code.
  inline bool isActive() const noexcept { return hasFlag(NodeFlags::kIsActive); }

  //! Tests whether the node has a position assigned.
  //!
  //! \remarks Returns `true` if node position is non-zero.
  inline bool hasPosition() const noexcept { return _position != 0; }
  //! Returns node position.
  inline uint32_t position() const noexcept { return _position; }
  //! Sets node position.
  //!
  //! Node position is a 32-bit unsigned integer that is used by Compiler to track where the node is relatively to
  //! the start of the function. It doesn't describe a byte position in a binary, instead it's just a pseudo position
  //! used by liveness analysis and other tools around Compiler.
  //!
  //! If you don't use Compiler then you may use `position()` and `setPosition()` freely for your own purposes if
  //! the 32-bit value limit is okay for you.
  inline void setPosition(uint32_t position) noexcept { _position = position; }

  //! Returns user data casted to `T*`.
  //!
  //! User data is decicated to be used only by AsmJit users and not touched by the library. The data has a pointer
  //! size so you can either store a pointer or `intptr_t` value through `setUserDataAsIntPtr()`.
  template<typename T>
  inline T* userDataAsPtr() const noexcept { return static_cast<T*>(_userDataPtr); }
  //! Returns user data casted to `int64_t`.
  inline int64_t userDataAsInt64() const noexcept { return int64_t(_userDataU64); }
  //! Returns user data casted to `uint64_t`.
  inline uint64_t userDataAsUInt64() const noexcept { return _userDataU64; }

  //! Sets user data to `data`.
  template<typename T>
  inline void setUserDataAsPtr(T* data) noexcept { _userDataPtr = static_cast<void*>(data); }
  //! Sets used data to the given 64-bit signed `value`.
  inline void setUserDataAsInt64(int64_t value) noexcept { _userDataU64 = uint64_t(value); }
  //! Sets used data to the given 64-bit unsigned `value`.
  inline void setUserDataAsUInt64(uint64_t value) noexcept { _userDataU64 = value; }

  //! Resets user data to zero / nullptr.
  inline void resetUserData() noexcept { _userDataU64 = 0; }

  //! Tests whether the node has an associated pass data.
  inline bool hasPassData() const noexcept { return _passData != nullptr; }
  //! Returns the node pass data - data used during processing & transformations.
  template<typename T>
  inline T* passData() const noexcept { return (T*)_passData; }
  //! Sets the node pass data to `data`.
  template<typename T>
  inline void setPassData(T* data) noexcept { _passData = (void*)data; }
  //! Resets the node pass data to nullptr.
  inline void resetPassData() noexcept { _passData = nullptr; }

  //! Tests whether the node has an inline comment/annotation.
  inline bool hasInlineComment() const noexcept { return _inlineComment != nullptr; }
  //! Returns an inline comment/annotation string.
  inline const char* inlineComment() const noexcept { return _inlineComment; }
  //! Sets an inline comment/annotation string to `s`.
  inline void setInlineComment(const char* s) noexcept { _inlineComment = s; }
  //! Resets an inline comment/annotation string to nullptr.
  inline void resetInlineComment() noexcept { _inlineComment = nullptr; }

  //! \}
};

//! Instruction node.
//!
//! Wraps an instruction with its options and operands.
class InstNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(InstNode)

  //! \name Constants
  //! \{

  enum : uint32_t {
    //! Count of embedded operands per `InstNode` that are always allocated as a part of the instruction. Minimum
    //! embedded operands is 4, but in 32-bit more pointers are smaller and we can embed 5. The rest (up to 6 operands)
    //! is always stored in `InstExNode`.
    kBaseOpCapacity = uint32_t((128 - sizeof(BaseNode) - sizeof(BaseInst)) / sizeof(Operand_))
  };

  //! \}

  //! \name Members
  //! \{

  //! Base instruction data.
  BaseInst _baseInst;
  //! First 4 or 5 operands (indexed from 0).
  Operand_ _opArray[kBaseOpCapacity];

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `InstNode` instance.
  inline InstNode(BaseBuilder* cb, InstId instId, InstOptions options, uint32_t opCount, uint32_t opCapacity = kBaseOpCapacity) noexcept
    : BaseNode(cb, NodeType::kInst, NodeFlags::kIsCode | NodeFlags::kIsRemovable | NodeFlags::kActsAsInst),
      _baseInst(instId, options) {
    _inst._opCapacity = uint8_t(opCapacity);
    _inst._opCount = uint8_t(opCount);
  }

  //! \cond INTERNAL
  //! Reset all built-in operands, including `extraReg`.
  inline void _resetOps() noexcept {
    _baseInst.resetExtraReg();
    resetOpRange(0, opCapacity());
  }
  //! \endcond

  //! \}

  //! \name Instruction Object
  //! \{

  inline BaseInst& baseInst() noexcept { return _baseInst; }
  inline const BaseInst& baseInst() const noexcept { return _baseInst; }

  //! \}

  //! \name Instruction Id
  //! \{

  //! Returns the instruction id, see `BaseInst::Id`.
  inline InstId id() const noexcept { return _baseInst.id(); }
  //! Returns the instruction real id, see `BaseInst::Id`.
  inline InstId realId() const noexcept { return _baseInst.realId(); }

  //! Sets the instruction id to `id`, see `BaseInst::Id`.
  inline void setId(InstId id) noexcept { _baseInst.setId(id); }

  //! \}

  //! \name Instruction Options
  //! \{

  inline InstOptions options() const noexcept { return _baseInst.options(); }
  inline bool hasOption(InstOptions option) const noexcept { return _baseInst.hasOption(option); }
  inline void setOptions(InstOptions options) noexcept { _baseInst.setOptions(options); }
  inline void addOptions(InstOptions options) noexcept { _baseInst.addOptions(options); }
  inline void clearOptions(InstOptions options) noexcept { _baseInst.clearOptions(options); }
  inline void resetOptions() noexcept { _baseInst.resetOptions(); }

  //! \}

  //! \name Extra Register
  //! \{

  //! Tests whether the node has an extra register operand.
  inline bool hasExtraReg() const noexcept { return _baseInst.hasExtraReg(); }
  //! Returns extra register operand.
  inline RegOnly& extraReg() noexcept { return _baseInst.extraReg(); }
  //! \overload
  inline const RegOnly& extraReg() const noexcept { return _baseInst.extraReg(); }
  //! Sets extra register operand to `reg`.
  inline void setExtraReg(const BaseReg& reg) noexcept { _baseInst.setExtraReg(reg); }
  //! Sets extra register operand to `reg`.
  inline void setExtraReg(const RegOnly& reg) noexcept { _baseInst.setExtraReg(reg); }
  //! Resets extra register operand.
  inline void resetExtraReg() noexcept { _baseInst.resetExtraReg(); }

  //! \}

  //! \name Instruction Operands
  //! \{

  //! Returns operand count.
  inline uint32_t opCount() const noexcept { return _inst._opCount; }
  //! Returns operand capacity.
  inline uint32_t opCapacity() const noexcept { return _inst._opCapacity; }

  //! Sets operand count.
  inline void setOpCount(uint32_t opCount) noexcept { _inst._opCount = uint8_t(opCount); }

  //! Returns operands array.
  inline Operand* operands() noexcept { return (Operand*)_opArray; }
  //! Returns operands array (const).
  inline const Operand* operands() const noexcept { return (const Operand*)_opArray; }

  //! Returns operand at the given `index`.
  inline Operand& op(uint32_t index) noexcept {
    ASMJIT_ASSERT(index < opCapacity());
    return _opArray[index].as<Operand>();
  }

  //! Returns operand at the given `index` (const).
  inline const Operand& op(uint32_t index) const noexcept {
    ASMJIT_ASSERT(index < opCapacity());
    return _opArray[index].as<Operand>();
  }

  //! Sets operand at the given `index` to `op`.
  inline void setOp(uint32_t index, const Operand_& op) noexcept {
    ASMJIT_ASSERT(index < opCapacity());
    _opArray[index].copyFrom(op);
  }

  //! Resets operand at the given `index` to none.
  inline void resetOp(uint32_t index) noexcept {
    ASMJIT_ASSERT(index < opCapacity());
    _opArray[index].reset();
  }

  //! Resets operands at `[start, end)` range.
  inline void resetOpRange(uint32_t start, uint32_t end) noexcept {
    for (uint32_t i = start; i < end; i++)
      _opArray[i].reset();
  }

  //! \}

  //! \name Utilities
  //! \{

  inline bool hasOpType(OperandType opType) const noexcept {
    for (uint32_t i = 0, count = opCount(); i < count; i++)
      if (_opArray[i].opType() == opType)
        return true;
    return false;
  }

  inline bool hasRegOp() const noexcept { return hasOpType(OperandType::kReg); }
  inline bool hasMemOp() const noexcept { return hasOpType(OperandType::kMem); }
  inline bool hasImmOp() const noexcept { return hasOpType(OperandType::kImm); }
  inline bool hasLabelOp() const noexcept { return hasOpType(OperandType::kLabel); }

  inline uint32_t indexOfOpType(OperandType opType) const noexcept {
    uint32_t i = 0;
    uint32_t count = opCount();

    while (i < count) {
      if (_opArray[i].opType() == opType)
        break;
      i++;
    }

    return i;
  }

  inline uint32_t indexOfMemOp() const noexcept { return indexOfOpType(OperandType::kMem); }
  inline uint32_t indexOfImmOp() const noexcept { return indexOfOpType(OperandType::kImm); }
  inline uint32_t indexOfLabelOp() const noexcept { return indexOfOpType(OperandType::kLabel); }

  //! \}

  //! \name Rewriting
  //! \{

  //! \cond INTERNAL
  inline uint32_t* _getRewriteArray() noexcept { return &_baseInst._extraReg._id; }
  inline const uint32_t* _getRewriteArray() const noexcept { return &_baseInst._extraReg._id; }

  inline uint32_t getRewriteIndex(const uint32_t* id) const noexcept {
    const uint32_t* array = _getRewriteArray();
    ASMJIT_ASSERT(array <= id);

    size_t index = (size_t)(id - array);
    ASMJIT_ASSERT(index < 32);

    return uint32_t(index);
  }

  inline void rewriteIdAtIndex(uint32_t index, uint32_t id) noexcept {
    uint32_t* array = _getRewriteArray();
    array[index] = id;
  }
  //! \endcond

  //! \}

  //! \name Static Functions
  //! \{

  //! \cond INTERNAL
  static inline uint32_t capacityOfOpCount(uint32_t opCount) noexcept {
    return opCount <= kBaseOpCapacity ? kBaseOpCapacity : Globals::kMaxOpCount;
  }

  static inline size_t nodeSizeOfOpCapacity(uint32_t opCapacity) noexcept {
    size_t base = sizeof(InstNode) - kBaseOpCapacity * sizeof(Operand);
    return base + opCapacity * sizeof(Operand);
  }
  //! \endcond

  //! \}
};

//! Instruction node with maximum number of operands.
//!
//! This node is created automatically by Builder/Compiler in case that the required number of operands exceeds
//! the default capacity of `InstNode`.
class InstExNode : public InstNode {
public:
  ASMJIT_NONCOPYABLE(InstExNode)

  //! \name Members
  //! \{

  //! Continued `_opArray[]` to hold up to `kMaxOpCount` operands.
  Operand_ _opArrayEx[Globals::kMaxOpCount - kBaseOpCapacity];

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `InstExNode` instance.
  inline InstExNode(BaseBuilder* cb, InstId instId, InstOptions options, uint32_t opCapacity = Globals::kMaxOpCount) noexcept
    : InstNode(cb, instId, options, opCapacity) {}

  //! \}
};

//! Section node.
class SectionNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(SectionNode)

  //! \name Members
  //! \{

  //! Section id.
  uint32_t _id;

  //! Next section node that follows this section.
  //!
  //! This link is only valid when the section is active (is part of the code) and when `Builder::hasDirtySectionLinks()`
  //! returns `false`. If you intend to use this field you should always call `Builder::updateSectionLinks()` before you
  //! do so.
  SectionNode* _nextSection;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `SectionNode` instance.
  inline SectionNode(BaseBuilder* cb, uint32_t secionId = 0) noexcept
    : BaseNode(cb, NodeType::kSection, NodeFlags::kHasNoEffect),
      _id(secionId),
      _nextSection(nullptr) {}

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the section id.
  inline uint32_t id() const noexcept { return _id; }

  //! \}
};

//! Label node.
class LabelNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(LabelNode)

  //! \name Members
  //! \{

  //! Label identifier.
  uint32_t _labelId;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `LabelNode` instance.
  inline LabelNode(BaseBuilder* cb, uint32_t labelId = 0) noexcept
    : BaseNode(cb, NodeType::kLabel, NodeFlags::kHasNoEffect | NodeFlags::kActsAsLabel),
      _labelId(labelId) {}

  //! \}

  //! \name Accessors
  //! \{

  //! Returns \ref Label representation of the \ref LabelNode.
  inline Label label() const noexcept { return Label(_labelId); }
  //! Returns the id of the label.
  inline uint32_t labelId() const noexcept { return _labelId; }

  //! \}
};

//! Align directive (BaseBuilder).
//!
//! Wraps `.align` directive.
class AlignNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(AlignNode)

  //! \name Members
  //! \{

  //! Alignment (in bytes).
  uint32_t _alignment;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `AlignNode` instance.
  inline AlignNode(BaseBuilder* cb, AlignMode alignMode, uint32_t alignment) noexcept
    : BaseNode(cb, NodeType::kAlign, NodeFlags::kIsCode | NodeFlags::kHasNoEffect) {

    _alignData._alignMode = alignMode;
    _alignment = alignment;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns align mode.
  inline AlignMode alignMode() const noexcept { return _alignData._alignMode; }
  //! Sets align mode to `alignMode`.
  inline void setAlignMode(AlignMode alignMode) noexcept { _alignData._alignMode = alignMode; }

  //! Returns align offset in bytes.
  inline uint32_t alignment() const noexcept { return _alignment; }
  //! Sets align offset in bytes to `offset`.
  inline void setAlignment(uint32_t alignment) noexcept { _alignment = alignment; }

  //! \}
};

//! Embed data node.
//!
//! Wraps `.data` directive. The node contains data that will be placed at the node's position in the assembler
//! stream. The data is considered to be RAW; no analysis nor byte-order conversion is performed on RAW data.
class EmbedDataNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(EmbedDataNode)

  //! \cond INTERNAL
  enum : uint32_t {
    kInlineBufferSize = 128 - (sizeof(BaseNode) + sizeof(size_t) * 2)
  };
  //! \endcond

  //! \name Members
  //! \{

  size_t _itemCount;
  size_t _repeatCount;

  union {
    uint8_t* _externalData;
    uint8_t _inlineData[kInlineBufferSize];
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `EmbedDataNode` instance.
  inline EmbedDataNode(BaseBuilder* cb) noexcept
    : BaseNode(cb, NodeType::kEmbedData, NodeFlags::kIsData),
      _itemCount(0),
      _repeatCount(0) {
    _embed._typeId = TypeId::kUInt8;
    _embed._typeSize = uint8_t(1);
    memset(_inlineData, 0, kInlineBufferSize);
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns data type as \ref TypeId.
  inline TypeId typeId() const noexcept { return _embed._typeId; }
  //! Returns the size of a single data element.
  inline uint32_t typeSize() const noexcept { return _embed._typeSize; }

  //! Returns a pointer to the data casted to `uint8_t`.
  inline uint8_t* data() const noexcept {
    return dataSize() <= kInlineBufferSize ? const_cast<uint8_t*>(_inlineData) : _externalData;
  }

  //! Returns a pointer to the data casted to `T`.
  template<typename T>
  inline T* dataAs() const noexcept { return reinterpret_cast<T*>(data()); }

  //! Returns the number of (typed) items in the array.
  inline size_t itemCount() const noexcept { return _itemCount; }

  //! Returns how many times the data is repeated (default 1).
  //!
  //! Repeated data is useful when defining constants for SIMD, for example.
  inline size_t repeatCount() const noexcept { return _repeatCount; }

  //! Returns the size of the data, not considering the number of times it repeats.
  //!
  //! \note The returned value is the same as `typeSize() * itemCount()`.
  inline size_t dataSize() const noexcept { return typeSize() * _itemCount; }

  //! \}
};

//! Label data node.
class EmbedLabelNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(EmbedLabelNode)

  //! \name Members
  //! \{

  uint32_t _labelId;
  uint32_t _dataSize;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `EmbedLabelNode` instance.
  inline EmbedLabelNode(BaseBuilder* cb, uint32_t labelId = 0, uint32_t dataSize = 0) noexcept
    : BaseNode(cb, NodeType::kEmbedLabel, NodeFlags::kIsData),
      _labelId(labelId),
      _dataSize(dataSize) {}

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the label to embed as \ref Label operand.
  inline Label label() const noexcept { return Label(_labelId); }
  //! Returns the id of the label.
  inline uint32_t labelId() const noexcept { return _labelId; }

  //! Sets the label id from `label` operand.
  inline void setLabel(const Label& label) noexcept { setLabelId(label.id()); }
  //! Sets the label id (use with caution, improper use can break a lot of things).
  inline void setLabelId(uint32_t labelId) noexcept { _labelId = labelId; }

  //! Returns the data size.
  inline uint32_t dataSize() const noexcept { return _dataSize; }
  //! Sets the data size.
  inline void setDataSize(uint32_t dataSize) noexcept { _dataSize = dataSize; }

  //! \}
};

//! Label data node.
class EmbedLabelDeltaNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(EmbedLabelDeltaNode)

  //! \name Members
  //! \{

  uint32_t _labelId;
  uint32_t _baseLabelId;
  uint32_t _dataSize;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `EmbedLabelDeltaNode` instance.
  inline EmbedLabelDeltaNode(BaseBuilder* cb, uint32_t labelId = 0, uint32_t baseLabelId = 0, uint32_t dataSize = 0) noexcept
    : BaseNode(cb, NodeType::kEmbedLabelDelta, NodeFlags::kIsData),
      _labelId(labelId),
      _baseLabelId(baseLabelId),
      _dataSize(dataSize) {}

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the label as `Label` operand.
  inline Label label() const noexcept { return Label(_labelId); }
  //! Returns the id of the label.
  inline uint32_t labelId() const noexcept { return _labelId; }

  //! Sets the label id from `label` operand.
  inline void setLabel(const Label& label) noexcept { setLabelId(label.id()); }
  //! Sets the label id.
  inline void setLabelId(uint32_t labelId) noexcept { _labelId = labelId; }

  //! Returns the base label as `Label` operand.
  inline Label baseLabel() const noexcept { return Label(_baseLabelId); }
  //! Returns the id of the base label.
  inline uint32_t baseLabelId() const noexcept { return _baseLabelId; }

  //! Sets the base label id from `label` operand.
  inline void setBaseLabel(const Label& baseLabel) noexcept { setBaseLabelId(baseLabel.id()); }
  //! Sets the base label id.
  inline void setBaseLabelId(uint32_t baseLabelId) noexcept { _baseLabelId = baseLabelId; }

  //! Returns the size of the embedded label address.
  inline uint32_t dataSize() const noexcept { return _dataSize; }
  //! Sets the size of the embedded label address.
  inline void setDataSize(uint32_t dataSize) noexcept { _dataSize = dataSize; }

  //! \}
};

//! A node that wraps `ConstPool`.
class ConstPoolNode : public LabelNode {
public:
  ASMJIT_NONCOPYABLE(ConstPoolNode)

  //! \name Members
  //! \{

  ConstPool _constPool;

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `ConstPoolNode` instance.
  inline ConstPoolNode(BaseBuilder* cb, uint32_t id = 0) noexcept
    : LabelNode(cb, id),
      _constPool(&cb->_codeZone) {

    setType(NodeType::kConstPool);
    addFlags(NodeFlags::kIsData);
    clearFlags(NodeFlags::kIsCode | NodeFlags::kHasNoEffect);
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the constant-pool is empty.
  inline bool empty() const noexcept { return _constPool.empty(); }
  //! Returns the size of the constant-pool in bytes.
  inline size_t size() const noexcept { return _constPool.size(); }
  //! Returns minimum alignment.
  inline size_t alignment() const noexcept { return _constPool.alignment(); }

  //! Returns the wrapped `ConstPool` instance.
  inline ConstPool& constPool() noexcept { return _constPool; }
  //! Returns the wrapped `ConstPool` instance (const).
  inline const ConstPool& constPool() const noexcept { return _constPool; }

  //! \}

  //! \name Utilities
  //! \{

  //! See `ConstPool::add()`.
  inline Error add(const void* data, size_t size, size_t& dstOffset) noexcept {
    return _constPool.add(data, size, dstOffset);
  }

  //! \}
};

//! Comment node.
class CommentNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(CommentNode)

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `CommentNode` instance.
  inline CommentNode(BaseBuilder* cb, const char* comment) noexcept
    : BaseNode(cb, NodeType::kComment, NodeFlags::kIsInformative | NodeFlags::kHasNoEffect | NodeFlags::kIsRemovable) {
    _inlineComment = comment;
  }

  //! \}
};

//! Sentinel node.
//!
//! Sentinel is a marker that is completely ignored by the code builder. It's used to remember a position in a code
//! as it never gets removed by any pass.
class SentinelNode : public BaseNode {
public:
  ASMJIT_NONCOPYABLE(SentinelNode)

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `SentinelNode` instance.
  inline SentinelNode(BaseBuilder* cb, SentinelType sentinelType = SentinelType::kUnknown) noexcept
    : BaseNode(cb, NodeType::kSentinel, NodeFlags::kIsInformative | NodeFlags::kHasNoEffect) {

    _sentinel._sentinelType = sentinelType;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the type of the sentinel.
  inline SentinelType sentinelType() const noexcept {
    return _sentinel._sentinelType;
  }

  //! Sets the type of the sentinel.
  inline void setSentinelType(SentinelType type) noexcept {
    _sentinel._sentinelType = type;
  }

  //! \}
};

//! Pass can be used to implement code transformations, analysis, and lowering.
class ASMJIT_VIRTAPI Pass {
public:
  ASMJIT_BASE_CLASS(Pass)
  ASMJIT_NONCOPYABLE(Pass)

  //! \name Members
  //! \{

  //! BaseBuilder this pass is assigned to.
  BaseBuilder* _cb = nullptr;
  //! Name of the pass.
  const char* _name = nullptr;

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_API Pass(const char* name) noexcept;
  ASMJIT_API virtual ~Pass() noexcept;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns \ref BaseBuilder associated with the pass.
  inline const BaseBuilder* cb() const noexcept { return _cb; }
  //! Returns the name of the pass.
  inline const char* name() const noexcept { return _name; }

  //! \}

  //! \name Pass Interface
  //! \{

  //! Processes the code stored in Builder or Compiler.
  //!
  //! This is the only function that is called by the `BaseBuilder` to process the code. It passes `zone`,
  //! which will be reset after the `run()` finishes.
  virtual Error run(Zone* zone, Logger* logger) = 0;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_BUILDER
#endif // ASMJIT_CORE_BUILDER_H_INCLUDED
