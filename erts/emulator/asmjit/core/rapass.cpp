// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/formatter.h"
#include "../core/ralocal_p.h"
#include "../core/rapass_p.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../core/zonestack.h"

ASMJIT_BEGIN_NAMESPACE

// RABlock - Control Flow
// ======================

Error RABlock::appendSuccessor(RABlock* successor) noexcept {
  RABlock* predecessor = this;

  if (predecessor->hasSuccessor(successor))
    return kErrorOk;

  ASMJIT_PROPAGATE(successor->_predecessors.willGrow(allocator()));
  ASMJIT_PROPAGATE(predecessor->_successors.willGrow(allocator()));

  predecessor->_successors.appendUnsafe(successor);
  successor->_predecessors.appendUnsafe(predecessor);

  return kErrorOk;
}

Error RABlock::prependSuccessor(RABlock* successor) noexcept {
  RABlock* predecessor = this;

  if (predecessor->hasSuccessor(successor))
    return kErrorOk;

  ASMJIT_PROPAGATE(successor->_predecessors.willGrow(allocator()));
  ASMJIT_PROPAGATE(predecessor->_successors.willGrow(allocator()));

  predecessor->_successors.prependUnsafe(successor);
  successor->_predecessors.prependUnsafe(predecessor);

  return kErrorOk;
}

// BaseRAPass - Construction & Destruction
// =======================================

BaseRAPass::BaseRAPass() noexcept : FuncPass("BaseRAPass") {}
BaseRAPass::~BaseRAPass() noexcept {}

// BaseRAPass - RunOnFunction
// ==========================

static void BaseRAPass_reset(BaseRAPass* self, FuncDetail* funcDetail) noexcept {
  ZoneAllocator* allocator = self->allocator();

  self->_blocks.reset();
  self->_exits.reset();
  self->_pov.reset();
  self->_workRegs.reset();
  self->_instructionCount = 0;
  self->_createdBlockCount = 0;

  self->_sharedAssignments.reset();
  self->_lastTimestamp = 0;

  self->_archTraits = nullptr;
  self->_physRegIndex.reset();
  self->_physRegCount.reset();
  self->_physRegTotal = 0;
  self->_scratchRegIndexes.fill(BaseReg::kIdBad);

  self->_availableRegs.reset();
  self->_availableRegCount.reset();
  self->_clobberedRegs.reset();

  self->_workRegs.reset();
  self->_workRegsOfGroup.forEach([](RAWorkRegs& regs) { regs.reset(); });
  self->_strategy.forEach([](RAStrategy& strategy) { strategy.reset(); });
  self->_globalLiveSpans.fill(nullptr);
  self->_globalMaxLiveCount.reset();
  self->_temporaryMem.reset();

  self->_stackAllocator.reset(allocator);
  self->_argsAssignment.reset(funcDetail);
  self->_numStackArgsToStackSlots = 0;
  self->_maxWorkRegNameSize = 0;
}

static void BaseRAPass_resetVirtRegData(BaseRAPass* self) noexcept {
  for (RAWorkReg* wReg : self->_workRegs) {
    VirtReg* vReg = wReg->virtReg();

    // Update the information regarding the stack of the virtual register.
    if (wReg->hasStackSlot()) {
      RAStackSlot* slot = wReg->stackSlot();
      vReg->assignStackSlot(slot->offset());
    }

    // Reset work reg association so it cannot be used by accident (RAWorkReg data will be destroyed).
    vReg->_workReg = nullptr;
  }
}

Error BaseRAPass::runOnFunction(Zone* zone, Logger* logger, FuncNode* func) {
  _allocator.reset(zone);

#ifndef ASMJIT_NO_LOGGING
  _logger = logger;
  _formatOptions.reset();
  _diagnosticOptions = _cb->diagnosticOptions();

  if (logger) {
    _formatOptions = logger->options();
  }
  else {
    _diagnosticOptions &= ~(DiagnosticOptions::kRADebugCFG |
                            DiagnosticOptions::kRADebugUnreachable);
  }
#else
  DebugUtils::unused(logger);
#endif

  // Initialize all core structures to use `zone` and `func`.
  BaseNode* end = func->endNode();
  _func = func;
  _stop = end->next();
  _extraBlock = end;

  BaseRAPass_reset(this, &_func->_funcDetail);

  // Initialize architecture-specific members.
  onInit();

  // Perform all allocation steps required.
  Error err = onPerformAllSteps();

  // Must be called regardless of the allocation status.
  onDone();

  // Reset possible connections introduced by the register allocator.
  BaseRAPass_resetVirtRegData(this);

  // Reset all core structures and everything that depends on the passed `Zone`.
  BaseRAPass_reset(this, nullptr);
  _allocator.reset(nullptr);

#ifndef ASMJIT_NO_LOGGING
  _logger = nullptr;
  _formatOptions.reset();
  _diagnosticOptions = DiagnosticOptions::kNone;
#endif

  _func = nullptr;
  _stop = nullptr;
  _extraBlock = nullptr;

  // Reset `Zone` as nothing should persist between `runOnFunction()` calls.
  zone->reset();

  // We alter the compiler cursor, because it doesn't make sense to reference it after the compilation - some
  // nodes may disappear and the old cursor can go out anyway.
  cc()->_setCursor(cc()->lastNode());

  return err;
}

Error BaseRAPass::onPerformAllSteps() noexcept {
  ASMJIT_PROPAGATE(buildCFG());
  ASMJIT_PROPAGATE(buildCFGViews());
  ASMJIT_PROPAGATE(removeUnreachableCode());

  ASMJIT_PROPAGATE(buildCFGDominators());
  ASMJIT_PROPAGATE(buildLiveness());
  ASMJIT_PROPAGATE(assignArgIndexToWorkRegs());

#ifndef ASMJIT_NO_LOGGING
  if (hasDiagnosticOption(DiagnosticOptions::kRAAnnotate))
    ASMJIT_PROPAGATE(annotateCode());
#endif

  ASMJIT_PROPAGATE(runGlobalAllocator());
  ASMJIT_PROPAGATE(runLocalAllocator());

  ASMJIT_PROPAGATE(updateStackFrame());
  ASMJIT_PROPAGATE(insertPrologEpilog());

  ASMJIT_PROPAGATE(rewrite());

  return kErrorOk;
}

// BaseRAPass - Events
// ===================

void BaseRAPass::onInit() noexcept {}
void BaseRAPass::onDone() noexcept {}

// BaseRAPass - CFG - Basic Block Management
// =========================================

RABlock* BaseRAPass::newBlock(BaseNode* initialNode) noexcept {
  RABlock* block = zone()->newT<RABlock>(this);
  if (ASMJIT_UNLIKELY(!block))
    return nullptr;

  block->setFirst(initialNode);
  block->setLast(initialNode);

  _createdBlockCount++;
  return block;
}

RABlock* BaseRAPass::newBlockOrExistingAt(LabelNode* cbLabel, BaseNode** stoppedAt) noexcept {
  if (cbLabel->hasPassData())
    return cbLabel->passData<RABlock>();

  FuncNode* func = this->func();
  BaseNode* node = cbLabel->prev();
  RABlock* block = nullptr;

  // Try to find some label, but terminate the loop on any code. We try hard to coalesce code that contains two
  // consecutive labels or a combination of non-code nodes between 2 or more labels.
  //
  // Possible cases that would share the same basic block:
  //
  //   1. Two or more consecutive labels:
  //     Label1:
  //     Label2:
  //
  //   2. Two or more labels separated by non-code nodes:
  //     Label1:
  //     ; Some comment...
  //     .align 16
  //     Label2:
  size_t nPendingLabels = 0;

  while (node) {
    if (node->type() == NodeType::kLabel) {
      // Function has a different NodeType, just make sure this was not messed up as we must never associate
      // BasicBlock with a `func` itself.
      ASMJIT_ASSERT(node != func);

      block = node->passData<RABlock>();
      if (block) {
        // Exit node has always a block associated with it. If we went here it means that `cbLabel` passed here
        // is after the end of the function and cannot be merged with the function exit block.
        if (node == func->exitNode())
          block = nullptr;
        break;
      }

      nPendingLabels++;
    }
    else if (node->type() == NodeType::kAlign) {
      // Align node is fine.
    }
    else {
      break;
    }

    node = node->prev();
  }

  if (stoppedAt)
    *stoppedAt = node;

  if (!block) {
    block = newBlock();
    if (ASMJIT_UNLIKELY(!block))
      return nullptr;
  }

  cbLabel->setPassData<RABlock>(block);
  node = cbLabel;

  while (nPendingLabels) {
    node = node->prev();
    for (;;) {
      if (node->type() == NodeType::kLabel) {
        node->setPassData<RABlock>(block);
        nPendingLabels--;
        break;
      }

      node = node->prev();
      ASMJIT_ASSERT(node != nullptr);
    }
  }

  if (!block->first()) {
    block->setFirst(node);
    block->setLast(cbLabel);
  }

  return block;
}

Error BaseRAPass::addBlock(RABlock* block) noexcept {
  ASMJIT_PROPAGATE(_blocks.willGrow(allocator()));

  block->_blockId = blockCount();
  _blocks.appendUnsafe(block);
  return kErrorOk;
}

// BaseRAPass - CFG - Build
// ========================

// [[pure virtual]]
Error BaseRAPass::buildCFG() noexcept {
  return DebugUtils::errored(kErrorInvalidState);
}

Error BaseRAPass::initSharedAssignments(const ZoneVector<uint32_t>& sharedAssignmentsMap) noexcept {
  if (sharedAssignmentsMap.empty())
    return kErrorOk;

  uint32_t count = 0;
  for (RABlock* block : _blocks) {
    if (block->hasSharedAssignmentId()) {
      uint32_t sharedAssignmentId = sharedAssignmentsMap[block->sharedAssignmentId()];
      block->setSharedAssignmentId(sharedAssignmentId);
      count = Support::max(count, sharedAssignmentId + 1);
    }
  }

  ASMJIT_PROPAGATE(_sharedAssignments.resize(allocator(), count));

  // Aggregate all entry scratch GP regs from blocks of the same assignment to the assignment itself. It will then be
  // used instead of RABlock's own scratch regs mask, as shared assignments have precedence.
  for (RABlock* block : _blocks) {
    if (block->hasJumpTable()) {
      const RABlocks& successors = block->successors();
      if (!successors.empty()) {
        RABlock* firstSuccessor = successors[0];
        // NOTE: Shared assignments connect all possible successors so we only need the first to propagate exit scratch
        // GP registers.
        if (firstSuccessor->hasSharedAssignmentId()) {
          RASharedAssignment& sa = _sharedAssignments[firstSuccessor->sharedAssignmentId()];
          sa.addEntryScratchGpRegs(block->exitScratchGpRegs());
        }
        else {
          // This is only allowed if there is a single successor - in that case shared assignment is not necessary.
          ASMJIT_ASSERT(successors.size() == 1u);
        }
      }
    }
    if (block->hasSharedAssignmentId()) {
      RASharedAssignment& sa = _sharedAssignments[block->sharedAssignmentId()];
      sa.addEntryScratchGpRegs(block->_entryScratchGpRegs);
    }
  }

  return kErrorOk;
}

// BaseRAPass - CFG - Views Order
// ==============================

class RABlockVisitItem {
public:
  RABlock* _block {};
  uint32_t _index {};

  inline RABlockVisitItem(RABlock* block, uint32_t index) noexcept
    : _block(block),
      _index(index) {}

  inline RABlockVisitItem(const RABlockVisitItem& other) noexcept = default;
  inline RABlockVisitItem& operator=(const RABlockVisitItem& other) noexcept = default;

  inline RABlock* block() const noexcept { return _block; }
  inline uint32_t index() const noexcept { return _index; }
};

Error BaseRAPass::buildCFGViews() noexcept {
#ifndef ASMJIT_NO_LOGGING
  Logger* logger = getLoggerIf(DiagnosticOptions::kRADebugCFG);
  ASMJIT_RA_LOG_FORMAT("[BuildCFGViews]\n");
#endif

  uint32_t count = blockCount();
  if (ASMJIT_UNLIKELY(!count)) return kErrorOk;

  ASMJIT_PROPAGATE(_pov.reserve(allocator(), count));

  ZoneStack<RABlockVisitItem> stack;
  ASMJIT_PROPAGATE(stack.init(allocator()));

  ZoneBitVector visited;
  ASMJIT_PROPAGATE(visited.resize(allocator(), count));

  RABlock* current = _blocks[0];
  uint32_t i = 0;

  for (;;) {
    for (;;) {
      if (i >= current->successors().size())
        break;

      // Skip if already visited.
      RABlock* child = current->successors()[i++];
      if (visited.bitAt(child->blockId()))
        continue;

      // Mark as visited to prevent visiting the same block multiple times.
      visited.setBit(child->blockId(), true);

      // Add the current block on the stack, we will get back to it later.
      ASMJIT_PROPAGATE(stack.append(RABlockVisitItem(current, i)));
      current = child;
      i = 0;
    }

    current->makeReachable();
    current->_povOrder = _pov.size();
    _pov.appendUnsafe(current);

    if (stack.empty())
      break;

    RABlockVisitItem top = stack.pop();
    current = top.block();
    i = top.index();
  }

  ASMJIT_RA_LOG_COMPLEX({
    StringTmp<1024> sb;
    for (RABlock* block : blocks()) {
      sb.clear();
      if (block->hasSuccessors()) {
        sb.appendFormat("  #%u -> {", block->blockId());
        _dumpBlockIds(sb, block->successors());
        sb.append("}\n");
      }
      else {
        sb.appendFormat("  #%u -> {Exit}\n", block->blockId());
      }
      logger->log(sb);
    }
  });

  visited.release(allocator());
  return kErrorOk;
}

// BaseRAPass - CFG - Dominators
// =============================

static ASMJIT_FORCE_INLINE RABlock* intersectBlocks(RABlock* b1, RABlock* b2) noexcept {
  while (b1 != b2) {
    while (b2->povOrder() > b1->povOrder()) b1 = b1->iDom();
    while (b1->povOrder() > b2->povOrder()) b2 = b2->iDom();
  }
  return b1;
}

// Based on "A Simple, Fast Dominance Algorithm".
Error BaseRAPass::buildCFGDominators() noexcept {
#ifndef ASMJIT_NO_LOGGING
  Logger* logger = getLoggerIf(DiagnosticOptions::kRADebugCFG);
  ASMJIT_RA_LOG_FORMAT("[BuildCFGDominators]\n");
#endif

  if (_blocks.empty())
    return kErrorOk;

  RABlock* entryBlock = this->entryBlock();
  entryBlock->setIDom(entryBlock);

  bool changed = true;

#ifndef ASMJIT_NO_LOGGING
  uint32_t numIters = 0;
#endif

  while (changed) {
    changed = false;

#ifndef ASMJIT_NO_LOGGING
    numIters++;
#endif

    uint32_t i = _pov.size();
    while (i) {
      RABlock* block = _pov[--i];
      if (block == entryBlock)
        continue;

      RABlock* iDom = nullptr;
      const RABlocks& preds = block->predecessors();

      uint32_t j = preds.size();
      while (j) {
        RABlock* p = preds[--j];
        if (!p->iDom())
          continue;
        iDom = !iDom ? p : intersectBlocks(iDom, p);
      }

      if (block->iDom() != iDom) {
        ASMJIT_ASSUME(iDom != nullptr);
        ASMJIT_RA_LOG_FORMAT("  IDom of #%u -> #%u\n", block->blockId(), iDom->blockId());
        block->setIDom(iDom);
        changed = true;
      }
    }
  }

  ASMJIT_RA_LOG_FORMAT("  Done (%u iterations)\n", numIters);
  return kErrorOk;
}

bool BaseRAPass::_strictlyDominates(const RABlock* a, const RABlock* b) const noexcept {
  ASMJIT_ASSERT(a != nullptr); // There must be at least one block if this function is
  ASMJIT_ASSERT(b != nullptr); // called, as both `a` and `b` must be valid blocks.
  ASMJIT_ASSERT(a != b);       // Checked by `dominates()` and `strictlyDominates()`.

  // Nothing strictly dominates the entry block.
  const RABlock* entryBlock = this->entryBlock();
  if (a == entryBlock)
    return false;

  const RABlock* iDom = b->iDom();
  while (iDom != a && iDom != entryBlock)
    iDom = iDom->iDom();

  return iDom != entryBlock;
}

const RABlock* BaseRAPass::_nearestCommonDominator(const RABlock* a, const RABlock* b) const noexcept {
  ASMJIT_ASSERT(a != nullptr); // There must be at least one block if this function is
  ASMJIT_ASSERT(b != nullptr); // called, as both `a` and `b` must be valid blocks.
  ASMJIT_ASSERT(a != b);       // Checked by `dominates()` and `properlyDominates()`.

  if (a == b)
    return a;

  // If `a` strictly dominates `b` then `a` is the nearest common dominator.
  if (_strictlyDominates(a, b))
    return a;

  // If `b` strictly dominates `a` then `b` is the nearest common dominator.
  if (_strictlyDominates(b, a))
    return b;

  const RABlock* entryBlock = this->entryBlock();
  uint64_t timestamp = nextTimestamp();

  // Mark all A's dominators.
  const RABlock* block = a->iDom();
  while (block != entryBlock) {
    block->setTimestamp(timestamp);
    block = block->iDom();
  }

  // Check all B's dominators against marked dominators of A.
  block = b->iDom();
  while (block != entryBlock) {
    if (block->hasTimestamp(timestamp))
      return block;
    block = block->iDom();
  }

  return entryBlock;
}

// BaseRAPass - CFG - Utilities
// ============================

Error BaseRAPass::removeUnreachableCode() noexcept {
  uint32_t numAllBlocks = blockCount();
  uint32_t numReachableBlocks = reachableBlockCount();

  // All reachable -> nothing to do.
  if (numAllBlocks == numReachableBlocks)
    return kErrorOk;

#ifndef ASMJIT_NO_LOGGING
  StringTmp<256> sb;
  Logger* logger = getLoggerIf(DiagnosticOptions::kRADebugUnreachable);
  ASMJIT_RA_LOG_FORMAT("[RemoveUnreachableCode - detected %u of %u unreachable blocks]\n", numAllBlocks - numReachableBlocks, numAllBlocks);
#endif

  for (uint32_t i = 0; i < numAllBlocks; i++) {
    RABlock* block = _blocks[i];
    if (block->isReachable())
      continue;

    ASMJIT_RA_LOG_FORMAT("  Removing code from unreachable block {%u}\n", i);
    BaseNode* first = block->first();
    BaseNode* last = block->last();

    BaseNode* beforeFirst = first->prev();
    BaseNode* afterLast = last->next();

    BaseNode* node = first;
    while (node != afterLast) {
      BaseNode* next = node->next();

      if (node->isCode() || node->isRemovable()) {
#ifndef ASMJIT_NO_LOGGING
        if (logger) {
          sb.clear();
          Formatter::formatNode(sb, _formatOptions, cc(), node);
          logger->logf("    %s\n", sb.data());
        }
#endif
        cc()->removeNode(node);
      }
      node = next;
    }

    if (beforeFirst->next() == afterLast) {
      block->setFirst(nullptr);
      block->setLast(nullptr);
    }
    else {
      block->setFirst(beforeFirst->next());
      block->setLast(afterLast->prev());
    }
  }

  return kErrorOk;
}

BaseNode* BaseRAPass::findSuccessorStartingAt(BaseNode* node) noexcept {
  while (node && (node->isInformative() || node->hasNoEffect()))
    node = node->next();
  return node;
}

bool BaseRAPass::isNextTo(BaseNode* node, BaseNode* target) noexcept {
  for (;;) {
    node = node->next();
    if (node == target)
      return true;

    if (!node)
      return false;

    if (node->isCode() || node->isData())
      return false;
  }
}

// BaseRAPass - Registers - VirtReg / WorkReg Mapping
// ==================================================

Error BaseRAPass::_asWorkReg(VirtReg* vReg, RAWorkReg** out) noexcept {
  // Checked by `asWorkReg()` - must be true.
  ASMJIT_ASSERT(vReg->_workReg == nullptr);

  RegGroup group = vReg->group();
  ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);

  RAWorkRegs& wRegs = workRegs();
  RAWorkRegs& wRegsByGroup = workRegs(group);

  ASMJIT_PROPAGATE(wRegs.willGrow(allocator()));
  ASMJIT_PROPAGATE(wRegsByGroup.willGrow(allocator()));

  RAWorkReg* wReg = zone()->newT<RAWorkReg>(vReg, wRegs.size());
  if (ASMJIT_UNLIKELY(!wReg))
    return DebugUtils::errored(kErrorOutOfMemory);

  vReg->setWorkReg(wReg);
  if (!vReg->isStack())
    wReg->setRegByteMask(Support::lsbMask<uint64_t>(vReg->virtSize()));
  wRegs.appendUnsafe(wReg);
  wRegsByGroup.appendUnsafe(wReg);

  // Only used by RA logging.
  _maxWorkRegNameSize = Support::max(_maxWorkRegNameSize, vReg->nameSize());

  *out = wReg;
  return kErrorOk;
}

RAAssignment::WorkToPhysMap* BaseRAPass::newWorkToPhysMap() noexcept {
  uint32_t count = workRegCount();
  size_t size = WorkToPhysMap::sizeOf(count);

  // If no registers are used it could be zero, in that case return a dummy
  // map instead of NULL.
  if (ASMJIT_UNLIKELY(!size)) {
    static const RAAssignment::WorkToPhysMap nullMap = {{ 0 }};
    return const_cast<RAAssignment::WorkToPhysMap*>(&nullMap);
  }

  WorkToPhysMap* map = zone()->allocT<WorkToPhysMap>(size);
  if (ASMJIT_UNLIKELY(!map))
    return nullptr;

  map->reset(count);
  return map;
}

RAAssignment::PhysToWorkMap* BaseRAPass::newPhysToWorkMap() noexcept {
  uint32_t count = physRegTotal();
  size_t size = PhysToWorkMap::sizeOf(count);

  PhysToWorkMap* map = zone()->allocT<PhysToWorkMap>(size);
  if (ASMJIT_UNLIKELY(!map))
    return nullptr;

  map->reset(count);
  return map;
}

// BaseRAPass - Registers - Liveness Analysis and Statistics
// =========================================================

namespace LiveOps {
  typedef ZoneBitVector::BitWord BitWord;

  struct In {
    static ASMJIT_FORCE_INLINE BitWord op(BitWord dst, BitWord out, BitWord gen, BitWord kill) noexcept {
      DebugUtils::unused(dst);
      return (out | gen) & ~kill;
    }
  };

  template<typename Operator>
  static ASMJIT_FORCE_INLINE bool op(BitWord* dst, const BitWord* a, uint32_t n) noexcept {
    BitWord changed = 0;

    for (uint32_t i = 0; i < n; i++) {
      BitWord before = dst[i];
      BitWord after = Operator::op(before, a[i]);

      dst[i] = after;
      changed |= (before ^ after);
    }

    return changed != 0;
  }

  template<typename Operator>
  static ASMJIT_FORCE_INLINE bool op(BitWord* dst, const BitWord* a, const BitWord* b, uint32_t n) noexcept {
    BitWord changed = 0;

    for (uint32_t i = 0; i < n; i++) {
      BitWord before = dst[i];
      BitWord after = Operator::op(before, a[i], b[i]);

      dst[i] = after;
      changed |= (before ^ after);
    }

    return changed != 0;
  }

  template<typename Operator>
  static ASMJIT_FORCE_INLINE bool op(BitWord* dst, const BitWord* a, const BitWord* b, const BitWord* c, uint32_t n) noexcept {
    BitWord changed = 0;

#if defined(_MSC_VER) && _MSC_VER <= 1938
    // MSVC workaround (see #427).
    //
    // MSVC incorrectly auto-vectorizes this loop when used with <In> operator. For some reason it trashes a content
    // of a register, which causes the result to be incorrect. It's a compiler bug we have to prevent unfortunately.
    #pragma loop(no_vector)
#endif
    for (uint32_t i = 0; i < n; i++) {
      BitWord before = dst[i];
      BitWord after = Operator::op(before, a[i], b[i], c[i]);

      dst[i] = after;
      changed |= (before ^ after);
    }

    return changed != 0;
  }

  static ASMJIT_NOINLINE bool recalcInOut(RABlock* block, uint32_t numBitWords, bool initial = false) noexcept {
    bool changed = initial;

    const RABlocks& successors = block->successors();
    uint32_t numSuccessors = successors.size();

    // Calculate `OUT` based on `IN` of all successors.
    for (uint32_t i = 0; i < numSuccessors; i++)
      changed |= op<Support::Or>(block->liveOut().data(), successors[i]->liveIn().data(), numBitWords);

    // Calculate `IN` based on `OUT`, `GEN`, and `KILL` bits.
    if (changed)
      changed = op<In>(block->liveIn().data(), block->liveOut().data(), block->gen().data(), block->kill().data(), numBitWords);

    return changed;
  }
}

ASMJIT_FAVOR_SPEED Error BaseRAPass::buildLiveness() noexcept {
#ifndef ASMJIT_NO_LOGGING
  Logger* logger = getLoggerIf(DiagnosticOptions::kRADebugLiveness);
  StringTmp<512> sb;
#endif

  ASMJIT_RA_LOG_FORMAT("[BuildLiveness]\n");

  uint32_t i;

  uint32_t numAllBlocks = blockCount();
  uint32_t numReachableBlocks = reachableBlockCount();

  uint32_t numWorkRegs = workRegCount();
  uint32_t numBitWords = ZoneBitVector::_wordsPerBits(numWorkRegs);

  if (!numWorkRegs) {
    ASMJIT_RA_LOG_FORMAT("  Done (no virtual registers)\n");
    return kErrorOk;
  }

  ZoneVector<uint32_t> nUsesPerWorkReg; // Number of USEs of each RAWorkReg.
  ZoneVector<uint32_t> nOutsPerWorkReg; // Number of OUTs of each RAWorkReg.
  ZoneVector<uint32_t> nInstsPerBlock;  // Number of instructions of each RABlock.

  ASMJIT_PROPAGATE(nUsesPerWorkReg.resize(allocator(), numWorkRegs));
  ASMJIT_PROPAGATE(nOutsPerWorkReg.resize(allocator(), numWorkRegs));
  ASMJIT_PROPAGATE(nInstsPerBlock.resize(allocator(), numAllBlocks));

  // Calculate GEN/KILL of Each Block
  // --------------------------------

  for (i = 0; i < numReachableBlocks; i++) {
    RABlock* block = _pov[i];
    ASMJIT_PROPAGATE(block->resizeLiveBits(numWorkRegs));

    BaseNode* node = block->last();
    BaseNode* stop = block->first();

    uint32_t nInsts = 0;
    for (;;) {
      if (node->isInst()) {
        InstNode* inst = node->as<InstNode>();
        RAInst* raInst = inst->passData<RAInst>();
        ASMJIT_ASSERT(raInst != nullptr);

        RATiedReg* tiedRegs = raInst->tiedRegs();
        uint32_t count = raInst->tiedCount();

        for (uint32_t j = 0; j < count; j++) {
          RATiedReg* tiedReg = &tiedRegs[j];
          uint32_t workId = tiedReg->workId();

          // Update `nUses` and `nOuts`.
          nUsesPerWorkReg[workId] += 1u;
          nOutsPerWorkReg[workId] += uint32_t(tiedReg->isWrite());

          // Mark as:
          //   KILL - if this VirtReg is killed afterwards.
          //   LAST - if this VirtReg is last in this basic block.
          if (block->kill().bitAt(workId))
            tiedReg->addFlags(RATiedFlags::kKill);
          else if (!block->gen().bitAt(workId))
            tiedReg->addFlags(RATiedFlags::kLast);

          if (tiedReg->isWriteOnly()) {
            // KILL.
            block->kill().setBit(workId, true);
          }
          else {
            // GEN.
            block->kill().setBit(workId, false);
            block->gen().setBit(workId, true);
          }

          if (tiedReg->isLeadConsecutive()) {
            RAWorkReg* workReg = workRegById(workId);
            workReg->markLeadConsecutive();
          }

          if (tiedReg->hasConsecutiveParent()) {
            RAWorkReg* consecutiveParentReg = workRegById(tiedReg->consecutiveParent());
            ASMJIT_PROPAGATE(consecutiveParentReg->addImmediateConsecutive(allocator(), workId));
          }
        }

        nInsts++;
      }

      if (node == stop)
        break;

      node = node->prev();
      ASMJIT_ASSERT(node != nullptr);
    }

    nInstsPerBlock[block->blockId()] = nInsts;
  }

  // Calculate IN/OUT of Each Block
  // ------------------------------

#ifndef ASMJIT_NO_LOGGING
  uint32_t numVisits = numReachableBlocks;
#endif

  {
    ZoneStack<RABlock*> workList;
    ZoneBitVector workBits;

    ASMJIT_PROPAGATE(workList.init(allocator()));
    ASMJIT_PROPAGATE(workBits.resize(allocator(), blockCount(), true));

    for (i = 0; i < numReachableBlocks; i++) {
      RABlock* block = _pov[i];
      LiveOps::recalcInOut(block, numBitWords, true);
      ASMJIT_PROPAGATE(workList.append(block));
    }

    while (!workList.empty()) {
      RABlock* block = workList.popFirst();
      uint32_t blockId = block->blockId();

      workBits.setBit(blockId, false);
      if (LiveOps::recalcInOut(block, numBitWords)) {
        const RABlocks& predecessors = block->predecessors();
        uint32_t numPredecessors = predecessors.size();

        for (uint32_t j = 0; j < numPredecessors; j++) {
          RABlock* pred = predecessors[j];
          if (!workBits.bitAt(pred->blockId())) {
            workBits.setBit(pred->blockId(), true);
            ASMJIT_PROPAGATE(workList.append(pred));
          }
        }
      }
#ifndef ASMJIT_NO_LOGGING
      numVisits++;
#endif
    }

    workList.reset();
    workBits.release(allocator());
  }

  ASMJIT_RA_LOG_COMPLEX({
    logger->logf("  LiveIn/Out Done (%u visits)\n", numVisits);
    for (i = 0; i < numAllBlocks; i++) {
      RABlock* block = _blocks[i];

      ASMJIT_PROPAGATE(sb.assignFormat("  {#%u}\n", block->blockId()));
      ASMJIT_PROPAGATE(_dumpBlockLiveness(sb, block));

      logger->log(sb);
    }
  });

  // Reserve the space in each `RAWorkReg` for references
  // ----------------------------------------------------

  for (i = 0; i < numWorkRegs; i++) {
    RAWorkReg* workReg = workRegById(i);
    ASMJIT_PROPAGATE(workReg->_refs.reserve(allocator(), nUsesPerWorkReg[i]));
    ASMJIT_PROPAGATE(workReg->_writes.reserve(allocator(), nOutsPerWorkReg[i]));
  }

  // Assign block and instruction positions, build LiveCount and LiveSpans
  // ---------------------------------------------------------------------

  uint32_t position = 2;
  for (i = 0; i < numAllBlocks; i++) {
    RABlock* block = _blocks[i];
    if (!block->isReachable())
      continue;

    BaseNode* node = block->first();
    BaseNode* stop = block->last();

    uint32_t endPosition = position + nInstsPerBlock[i] * 2;
    block->setFirstPosition(position);
    block->setEndPosition(endPosition);

    RALiveCount curLiveCount;
    RALiveCount maxLiveCount;

    // Process LIVE-IN.
    ZoneBitVector::ForEachBitSet it(block->liveIn());
    while (it.hasNext()) {
      RAWorkReg* workReg = _workRegs[uint32_t(it.next())];
      curLiveCount[workReg->group()]++;
      ASMJIT_PROPAGATE(workReg->liveSpans().openAt(allocator(), position, endPosition));
    }

    for (;;) {
      if (node->isInst()) {
        InstNode* inst = node->as<InstNode>();
        RAInst* raInst = inst->passData<RAInst>();
        ASMJIT_ASSERT(raInst != nullptr);

        RATiedReg* tiedRegs = raInst->tiedRegs();
        uint32_t count = raInst->tiedCount();

        inst->setPosition(position);
        raInst->_liveCount = curLiveCount;

        for (uint32_t j = 0; j < count; j++) {
          RATiedReg* tiedReg = &tiedRegs[j];
          uint32_t workId = tiedReg->workId();

          // Create refs and writes.
          RAWorkReg* workReg = workRegById(workId);
          workReg->_refs.appendUnsafe(node);
          if (tiedReg->isWrite())
            workReg->_writes.appendUnsafe(node);

          // We couldn't calculate this in previous steps, but since we know all LIVE-OUT at this point it becomes
          // trivial. If this is the last instruction that uses this `workReg` and it's not LIVE-OUT then it is
          // KILLed here.
          if (tiedReg->isLast() && !block->liveOut().bitAt(workId))
            tiedReg->addFlags(RATiedFlags::kKill);

          LiveRegSpans& liveSpans = workReg->liveSpans();
          bool wasOpen;
          ASMJIT_PROPAGATE(liveSpans.openAt(allocator(), position + !tiedReg->isRead(), endPosition, wasOpen));

          RegGroup group = workReg->group();
          if (!wasOpen) {
            curLiveCount[group]++;
            raInst->_liveCount[group]++;
          }

          if (tiedReg->isKill()) {
            liveSpans.closeAt(position + !tiedReg->isRead() + 1);
            curLiveCount[group]--;
          }

          // Update `RAWorkReg::useIdMask` and `RAWorkReg::hintRegId`.
          if (tiedReg->hasUseId()) {
            uint32_t useId = tiedReg->useId();
            workReg->addUseIdMask(Support::bitMask(useId));
            if (!workReg->hasHintRegId() && !Support::bitTest(raInst->_clobberedRegs[group], useId))
              workReg->setHintRegId(useId);
          }

          if (tiedReg->useRegMask()) {
            workReg->restrictPreferredMask(tiedReg->useRegMask());
            if (workReg->isLeadConsecutive())
              workReg->restrictConsecutiveMask(tiedReg->useRegMask());
          }

          if (tiedReg->outRegMask()) {
            workReg->restrictPreferredMask(tiedReg->outRegMask());
            if (workReg->isLeadConsecutive())
              workReg->restrictConsecutiveMask(tiedReg->outRegMask());
          }

          // Update `RAWorkReg::clobberedSurvivalMask`.
          if (raInst->_clobberedRegs[group] && !tiedReg->isOutOrKill()) {
            workReg->addClobberSurvivalMask(raInst->_clobberedRegs[group]);
          }
        }

        position += 2;
        maxLiveCount.op<Support::Max>(raInst->_liveCount);
      }

      if (node == stop)
        break;

      node = node->next();
      ASMJIT_ASSERT(node != nullptr);
    }

    block->_maxLiveCount = maxLiveCount;
    _globalMaxLiveCount.op<Support::Max>(maxLiveCount);
    ASMJIT_ASSERT(position == block->endPosition());
  }

  // Calculate WorkReg statistics
  // ----------------------------

  for (i = 0; i < numWorkRegs; i++) {
    RAWorkReg* workReg = _workRegs[i];

    LiveRegSpans& spans = workReg->liveSpans();
    uint32_t width = spans.width();
    float freq = width ? float(double(workReg->_refs.size()) / double(width)) : float(0);

    RALiveStats& stats = workReg->liveStats();
    stats._width = width;
    stats._freq = freq;
    stats._priority = freq + float(int(workReg->virtReg()->weight())) * 0.01f;
  }

  ASMJIT_RA_LOG_COMPLEX({
    sb.clear();
    _dumpLiveSpans(sb);
    logger->log(sb);
  });

  nUsesPerWorkReg.release(allocator());
  nOutsPerWorkReg.release(allocator());
  nInstsPerBlock.release(allocator());

  return kErrorOk;
}

Error BaseRAPass::assignArgIndexToWorkRegs() noexcept {
  ZoneBitVector& liveIn = entryBlock()->liveIn();
  uint32_t argCount = func()->argCount();

  for (uint32_t argIndex = 0; argIndex < argCount; argIndex++) {
    for (uint32_t valueIndex = 0; valueIndex < Globals::kMaxValuePack; valueIndex++) {
      // Unassigned argument.
      const RegOnly& regArg = func()->argPack(argIndex)[valueIndex];
      if (!regArg.isReg() || !cc()->isVirtIdValid(regArg.id()))
        continue;

      VirtReg* virtReg = cc()->virtRegById(regArg.id());
      if (!virtReg)
        continue;

      // Unreferenced argument.
      RAWorkReg* workReg = virtReg->workReg();
      if (!workReg)
        continue;

      // Overwritten argument.
      uint32_t workId = workReg->workId();
      if (!liveIn.bitAt(workId))
        continue;

      workReg->setArgIndex(argIndex, valueIndex);
      const FuncValue& arg = func()->detail().arg(argIndex, valueIndex);

      if (arg.isReg() && _archTraits->regTypeToGroup(arg.regType()) == workReg->group()) {
        workReg->setHintRegId(arg.regId());
      }
    }
  }

  return kErrorOk;
}

// BaseRAPass - Allocation - Global
// ================================

#ifndef ASMJIT_NO_LOGGING
static void RAPass_dumpSpans(String& sb, uint32_t index, const LiveRegSpans& liveSpans) noexcept {
  sb.appendFormat("  %02u: ", index);

  for (uint32_t i = 0; i < liveSpans.size(); i++) {
    const LiveRegSpan& liveSpan = liveSpans[i];
    if (i) sb.append(", ");
    sb.appendFormat("[%u:%u@%u]", liveSpan.a, liveSpan.b, liveSpan.id);
  }

  sb.append('\n');
}
#endif

Error BaseRAPass::runGlobalAllocator() noexcept {
  ASMJIT_PROPAGATE(initGlobalLiveSpans());

  for (RegGroup group : RegGroupVirtValues{}) {
    ASMJIT_PROPAGATE(binPack(group));
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SPEED Error BaseRAPass::initGlobalLiveSpans() noexcept {
  for (RegGroup group : RegGroupVirtValues{}) {
    size_t physCount = _physRegCount[group];
    LiveRegSpans* liveSpans = nullptr;

    if (physCount) {
      liveSpans = allocator()->allocT<LiveRegSpans>(physCount * sizeof(LiveRegSpans));
      if (ASMJIT_UNLIKELY(!liveSpans))
        return DebugUtils::errored(kErrorOutOfMemory);

      for (size_t physId = 0; physId < physCount; physId++)
        new(Support::PlacementNew{&liveSpans[physId]}) LiveRegSpans();
    }

    _globalLiveSpans[group] = liveSpans;
  }

  return kErrorOk;
}

struct RAConsecutiveReg {
  RAWorkReg* workReg;
  RAWorkReg* parentReg;
};

ASMJIT_FAVOR_SPEED Error BaseRAPass::binPack(RegGroup group) noexcept {
  if (workRegCount(group) == 0)
    return kErrorOk;

#ifndef ASMJIT_NO_LOGGING
  Logger* logger = getLoggerIf(DiagnosticOptions::kRADebugAssignment);
  StringTmp<512> sb;

  ASMJIT_RA_LOG_FORMAT("[BinPack] Available=%u (0x%08X) Count=%u RegGroup=%u\n",
    Support::popcnt(_availableRegs[group]),
    _availableRegs[group],
    workRegCount(group),
    uint32_t(group));
#endif

  uint32_t i;
  uint32_t physCount = _physRegCount[group];

  RAWorkRegs workRegs;
  ZoneVector<RAConsecutiveReg> consecutiveRegs;
  LiveRegSpans tmpSpans;

  ASMJIT_PROPAGATE(workRegs.concat(allocator(), this->workRegs(group)));
  workRegs.sort([](const RAWorkReg* a, const RAWorkReg* b) noexcept {
    return b->liveStats().priority() - a->liveStats().priority();
  });

  uint32_t numWorkRegs = workRegs.size();
  RegMask availableRegs = _availableRegs[group];
  RegMask preservedRegs = func()->frame().preservedRegs(group);

  // First try to pack everything that provides register-id hint as these are most likely function arguments and fixed
  // (precolored) virtual registers.
  if (!workRegs.empty()) {
    uint32_t dstIndex = 0;

    for (i = 0; i < numWorkRegs; i++) {
      RAWorkReg* workReg = workRegs[i];

      if (workReg->isLeadConsecutive()) {
        ASMJIT_PROPAGATE(consecutiveRegs.append(allocator(), RAConsecutiveReg{workReg, nullptr}));
        workReg->markProcessedConsecutive();
      }

      if (workReg->hasHintRegId()) {
        uint32_t physId = workReg->hintRegId();
        if (Support::bitTest(availableRegs, physId)) {
          LiveRegSpans& live = _globalLiveSpans[group][physId];
          Error err = tmpSpans.nonOverlappingUnionOf(allocator(), live, workReg->liveSpans(), LiveRegData(workReg->virtId()));

          if (err == kErrorOk) {
            live.swap(tmpSpans);
            workReg->setHomeRegId(physId);
            workReg->markAllocated();
            continue;
          }

          if (err != 0xFFFFFFFFu)
            return err;
        }
      }

      workRegs[dstIndex++] = workReg;
    }

    workRegs._setSize(dstIndex);
    numWorkRegs = dstIndex;
  }

  // Allocate consecutive registers - both leads and all consecutives. This is important and prioritized over the rest,
  // because once a lead is allocated we really need to allocate its consecutives, otherwise we may bin pack other
  // registers into their places, which would result in wrong hints to the local allocator, and then into many moves
  // or spills.
  if (!consecutiveRegs.empty()) {
    // This loop appends all other consecutive registers into `consecutiveRegs` array. Leads are at the beginning,
    // non-leads follow.
    i = 0;
    for (;;) {
      uint32_t stop = consecutiveRegs.size();
      if (i == stop)
        break;

      while (i < stop) {
        RAWorkReg* workReg = consecutiveRegs[i].workReg;
        if (workReg->hasImmediateConsecutives()) {
          ZoneBitVector::ForEachBitSet it(workReg->immediateConsecutives());
          while (it.hasNext()) {
            uint32_t consecutiveWorkId = uint32_t(it.next());
            RAWorkReg* consecutiveReg = workRegById(consecutiveWorkId);
            if (!consecutiveReg->isProcessedConsecutive()) {
              ASMJIT_PROPAGATE(consecutiveRegs.append(allocator(), RAConsecutiveReg{consecutiveReg, workReg}));
              consecutiveReg->markProcessedConsecutive();
            }
          }
        }
        i++;
      }
    }

    uint32_t numConsecutiveRegs = consecutiveRegs.size();
    for (i = 0; i < numConsecutiveRegs; i++) {
      RAWorkReg* workReg = consecutiveRegs[i].workReg;
      if (workReg->isAllocated())
        continue;

      RAWorkReg* parentReg = consecutiveRegs[i].parentReg;
      RegMask physRegs = 0;

      if (!parentReg) {
        physRegs = availableRegs & workReg->preferredMask();
        if (!physRegs) {
          physRegs = availableRegs & workReg->consecutiveMask();

          // NOTE: This should never be true as it would mean we would never allocate this virtual register
          // (not here, and not later when local register allocator processes RATiedReg sets).
          if (ASMJIT_UNLIKELY(!physRegs))
            return DebugUtils::errored(kErrorConsecutiveRegsAllocation);
        }
      }
      else if (parentReg->hasHomeRegId()) {
        uint32_t consecutiveId = parentReg->homeRegId() + 1;

        // NOTE: We don't support wrapping. If this goes beyond all allocable registers there is something wrong.
        if (consecutiveId > 31 || !Support::bitTest(availableRegs, consecutiveId))
          return DebugUtils::errored(kErrorConsecutiveRegsAllocation);

        workReg->setHintRegId(consecutiveId);
        physRegs = Support::bitMask(consecutiveId);
      }

      while (physRegs) {
        uint32_t physId = Support::bitSizeOf<RegMask>() - 1 - Support::clz(physRegs);

        LiveRegSpans& live = _globalLiveSpans[group][physId];
        Error err = tmpSpans.nonOverlappingUnionOf(allocator(), live, workReg->liveSpans(), LiveRegData(workReg->virtId()));

        if (err == kErrorOk) {
          workReg->setHomeRegId(physId);
          workReg->markAllocated();
          live.swap(tmpSpans);
          break;
        }

        if (ASMJIT_UNLIKELY(err != 0xFFFFFFFFu))
          return err;

        physRegs ^= Support::bitMask(physId);
      }
    }
  }

  // Try to pack the rest.
  if (!workRegs.empty()) {
    uint32_t dstIndex = 0;

    for (i = 0; i < numWorkRegs; i++) {
      RAWorkReg* workReg = workRegs[i];

      if (workReg->isAllocated())
        continue;

      RegMask remainingPhysRegs = availableRegs;
      if (remainingPhysRegs & workReg->preferredMask())
        remainingPhysRegs &= workReg->preferredMask();

      RegMask physRegs = remainingPhysRegs & ~preservedRegs;
      remainingPhysRegs &= preservedRegs;

      for (;;) {
        if (!physRegs) {
          if (!remainingPhysRegs)
            break;
          physRegs = remainingPhysRegs;
          remainingPhysRegs = 0;
        }

        uint32_t physId = Support::ctz(physRegs);

        if (workReg->clobberSurvivalMask()) {
          RegMask preferredMask = (physRegs | remainingPhysRegs) & workReg->clobberSurvivalMask();
          if (preferredMask) {
            if (preferredMask & ~remainingPhysRegs)
              preferredMask &= ~remainingPhysRegs;
            physId = Support::ctz(preferredMask);
          }
        }

        LiveRegSpans& live = _globalLiveSpans[group][physId];
        Error err = tmpSpans.nonOverlappingUnionOf(allocator(), live, workReg->liveSpans(), LiveRegData(workReg->virtId()));

        if (err == kErrorOk) {
          workReg->setHomeRegId(physId);
          workReg->markAllocated();
          live.swap(tmpSpans);
          break;
        }

        if (ASMJIT_UNLIKELY(err != 0xFFFFFFFFu))
          return err;

        physRegs &= ~Support::bitMask(physId);
        remainingPhysRegs &= ~Support::bitMask(physId);
      }

      // Keep it in `workRegs` if it was not allocated.
      if (!physRegs)
        workRegs[dstIndex++] = workReg;
    }

    workRegs._setSize(dstIndex);
    numWorkRegs = dstIndex;
  }

  ASMJIT_RA_LOG_COMPLEX({
    for (uint32_t physId = 0; physId < physCount; physId++) {
      LiveRegSpans& live = _globalLiveSpans[group][physId];
      if (live.empty())
        continue;

      sb.clear();
      RAPass_dumpSpans(sb, physId, live);
      logger->log(sb);
    }
  });

  // Maybe unused if logging is disabled.
  DebugUtils::unused(physCount);

  if (workRegs.empty()) {
    ASMJIT_RA_LOG_FORMAT("  Completed.\n");
  }
  else {
    _strategy[group].setType(RAStrategyType::kComplex);
    for (RAWorkReg* workReg : workRegs)
      workReg->markStackPreferred();

    ASMJIT_RA_LOG_COMPLEX({
      uint32_t count = workRegs.size();
      sb.clear();
      sb.appendFormat("  Unassigned (%u): ", count);
      for (i = 0; i < numWorkRegs; i++) {
        RAWorkReg* workReg = workRegs[i];
        if (i) sb.append(", ");
        sb.append(workReg->name());
      }
      sb.append('\n');
      logger->log(sb);
    });
  }

  return kErrorOk;
}

// BaseRAPass - Allocation - Local
// ===============================

Error BaseRAPass::runLocalAllocator() noexcept {
  RALocalAllocator lra(this);
  ASMJIT_PROPAGATE(lra.init());

  if (!blockCount())
    return kErrorOk;

  // The allocation is done when this reaches zero.
  uint32_t blocksRemaining = reachableBlockCount();

  // Current block.
  uint32_t blockId = 0;
  RABlock* block = _blocks[blockId];

  // The first block (entry) must always be reachable.
  ASMJIT_ASSERT(block->isReachable());

  // Assign function arguments for the initial block. The `lra` is valid now.
  lra.makeInitialAssignment();
  ASMJIT_PROPAGATE(setBlockEntryAssignment(block, block, lra._curAssignment));

  // The loop starts from the first block and iterates blocks in order, however, the algorithm also allows to jump to
  // any other block when finished if it's a jump target. In-order iteration just makes sure that all blocks are visited.
  for (;;) {
    BaseNode* first = block->first();
    BaseNode* last = block->last();
    BaseNode* terminator = block->hasTerminator() ? last : nullptr;

    BaseNode* beforeFirst = first->prev();
    BaseNode* afterLast = last->next();

    bool unconditionalJump = false;
    RABlock* consecutive = nullptr;

    if (block->hasSuccessors())
      consecutive = block->successors()[0];

    lra.setBlock(block);
    block->makeAllocated();

    BaseNode* node = first;
    while (node != afterLast) {
      BaseNode* next = node->next();
      if (node->isInst()) {
        InstNode* inst = node->as<InstNode>();

        if (ASMJIT_UNLIKELY(inst == terminator)) {
          const RABlocks& successors = block->successors();
          if (block->hasConsecutive()) {
            ASMJIT_PROPAGATE(lra.allocBranch(inst, successors.last(), successors.first()));

            node = next;
            continue;
          }
          else if (successors.size() > 1) {
            RABlock* cont = block->hasConsecutive() ? successors.first() : nullptr;
            ASMJIT_PROPAGATE(lra.allocJumpTable(inst, successors, cont));

            node = next;
            continue;
          }
          else {
            // Otherwise this is an unconditional jump, special handling isn't required.
            unconditionalJump = true;
          }
        }

        ASMJIT_PROPAGATE(lra.allocInst(inst));
        if (inst->type() == NodeType::kInvoke)
          ASMJIT_PROPAGATE(emitPreCall(inst->as<InvokeNode>()));
        else
          ASMJIT_PROPAGATE(lra.spillAfterAllocation(inst));
      }
      node = next;
    }

    if (consecutive) {
      BaseNode* prev = afterLast ? afterLast->prev() : cc()->lastNode();
      cc()->_setCursor(unconditionalJump ? prev->prev() : prev);

      if (consecutive->hasEntryAssignment()) {
        ASMJIT_PROPAGATE(lra.switchToAssignment(consecutive->entryPhysToWorkMap(), consecutive->liveIn(), consecutive->isAllocated(), false));
      }
      else {
        ASMJIT_PROPAGATE(lra.spillRegsBeforeEntry(consecutive));
        ASMJIT_PROPAGATE(setBlockEntryAssignment(consecutive, block, lra._curAssignment));
        lra._curAssignment.copyFrom(consecutive->entryPhysToWorkMap());
      }
    }

    // Important as the local allocator can insert instructions before
    // and after any instruction within the basic block.
    block->setFirst(beforeFirst->next());
    block->setLast(afterLast ? afterLast->prev() : cc()->lastNode());

    if (--blocksRemaining == 0)
      break;

    // Switch to the next consecutive block, if any.
    if (consecutive) {
      block = consecutive;
      if (!block->isAllocated())
        continue;
    }

    // Get the next block.
    for (;;) {
      if (++blockId >= blockCount())
        blockId = 0;

      block = _blocks[blockId];
      if (!block->isReachable() || block->isAllocated() || !block->hasEntryAssignment())
        continue;

      break;
    }

    // If we switched to some block we have to update the local allocator.
    lra.replaceAssignment(block->entryPhysToWorkMap());
  }

  _clobberedRegs.op<Support::Or>(lra._clobberedRegs);
  return kErrorOk;
}

Error BaseRAPass::setBlockEntryAssignment(RABlock* block, const RABlock* fromBlock, const RAAssignment& fromAssignment) noexcept {
  if (block->hasSharedAssignmentId()) {
    uint32_t sharedAssignmentId = block->sharedAssignmentId();

    // Shouldn't happen. Entry assignment of a block that has a shared-state will assign to all blocks
    // with the same sharedAssignmentId. It's a bug if the shared state has been already assigned.
    if (!_sharedAssignments[sharedAssignmentId].empty())
      return DebugUtils::errored(kErrorInvalidState);

    return setSharedAssignment(sharedAssignmentId, fromAssignment);
  }

  PhysToWorkMap* physToWorkMap = clonePhysToWorkMap(fromAssignment.physToWorkMap());
  if (ASMJIT_UNLIKELY(!physToWorkMap))
    return DebugUtils::errored(kErrorOutOfMemory);

  block->setEntryAssignment(physToWorkMap);

  // True if this is the first (entry) block, nothing to do in this case.
  if (block == fromBlock) {
    // Entry block should never have a shared state.
    if (block->hasSharedAssignmentId())
      return DebugUtils::errored(kErrorInvalidState);

    return kErrorOk;
  }

  const ZoneBitVector& liveOut = fromBlock->liveOut();
  const ZoneBitVector& liveIn = block->liveIn();

  // It's possible that `fromBlock` has LIVE-OUT regs that `block` doesn't
  // have in LIVE-IN, these have to be unassigned.
  {
    ZoneBitVector::ForEachBitOp<Support::AndNot> it(liveOut, liveIn);
    while (it.hasNext()) {
      uint32_t workId = uint32_t(it.next());
      RAWorkReg* workReg = workRegById(workId);

      RegGroup group = workReg->group();
      uint32_t physId = fromAssignment.workToPhysId(group, workId);

      if (physId != RAAssignment::kPhysNone)
        physToWorkMap->unassign(group, physId, _physRegIndex.get(group) + physId);
    }
  }

  return blockEntryAssigned(physToWorkMap);
}

Error BaseRAPass::setSharedAssignment(uint32_t sharedAssignmentId, const RAAssignment& fromAssignment) noexcept {
  ASMJIT_ASSERT(_sharedAssignments[sharedAssignmentId].empty());

  PhysToWorkMap* physToWorkMap = clonePhysToWorkMap(fromAssignment.physToWorkMap());
  if (ASMJIT_UNLIKELY(!physToWorkMap))
    return DebugUtils::errored(kErrorOutOfMemory);

  _sharedAssignments[sharedAssignmentId].assignPhysToWorkMap(physToWorkMap);

  ZoneBitVector& sharedLiveIn = _sharedAssignments[sharedAssignmentId]._liveIn;
  ASMJIT_PROPAGATE(sharedLiveIn.resize(allocator(), workRegCount()));

  Support::Array<uint32_t, Globals::kNumVirtGroups> sharedAssigned {};
  for (RABlock* block : blocks()) {
    if (block->sharedAssignmentId() == sharedAssignmentId) {
      ASMJIT_ASSERT(!block->hasEntryAssignment());

      PhysToWorkMap* entryPhysToWorkMap = clonePhysToWorkMap(fromAssignment.physToWorkMap());
      if (ASMJIT_UNLIKELY(!entryPhysToWorkMap))
        return DebugUtils::errored(kErrorOutOfMemory);

      block->setEntryAssignment(entryPhysToWorkMap);

      const ZoneBitVector& liveIn = block->liveIn();
      sharedLiveIn.or_(liveIn);

      for (RegGroup group : RegGroupVirtValues{}) {
        sharedAssigned[group] |= entryPhysToWorkMap->assigned[group];

        uint32_t physBaseIndex = _physRegIndex.get(group);
        Support::BitWordIterator<RegMask> it(entryPhysToWorkMap->assigned[group]);

        while (it.hasNext()) {
          uint32_t physId = it.next();
          uint32_t workId = entryPhysToWorkMap->workIds[physBaseIndex + physId];

          if (!liveIn.bitAt(workId))
            entryPhysToWorkMap->unassign(group, physId, physBaseIndex + physId);
        }
      }
    }
  }

  for (RegGroup group : RegGroupVirtValues{}) {
    uint32_t physBaseIndex = _physRegIndex.get(group);
    Support::BitWordIterator<RegMask> it(_availableRegs[group] & ~sharedAssigned[group]);

    while (it.hasNext()) {
      uint32_t physId = it.next();
      if (Support::bitTest(physToWorkMap->assigned[group], physId))
        physToWorkMap->unassign(group, physId, physBaseIndex + physId);
    }
  }

  return blockEntryAssigned(physToWorkMap);
}

Error BaseRAPass::blockEntryAssigned(const PhysToWorkMap* physToWorkMap) noexcept {
  // Complex allocation strategy requires to record register assignments upon block entry (or per shared state).
  for (RegGroup group : RegGroupVirtValues{}) {
    if (!_strategy[group].isComplex())
      continue;

    uint32_t physBaseIndex = _physRegIndex[group];
    Support::BitWordIterator<RegMask> it(physToWorkMap->assigned[group]);

    while (it.hasNext()) {
      uint32_t physId = it.next();
      uint32_t workId = physToWorkMap->workIds[physBaseIndex + physId];

      RAWorkReg* workReg = workRegById(workId);
      workReg->addAllocatedMask(Support::bitMask(physId));
    }
  }

  return kErrorOk;
}

// BaseRAPass - Allocation - Utilities
// ===================================

Error BaseRAPass::useTemporaryMem(BaseMem& out, uint32_t size, uint32_t alignment) noexcept {
  ASMJIT_ASSERT(alignment <= 64);

  if (_temporaryMem.isNone()) {
    ASMJIT_PROPAGATE(cc()->_newStack(&_temporaryMem.as<BaseMem>(), size, alignment));
  }
  else {
    ASMJIT_ASSERT(_temporaryMem.as<BaseMem>().isRegHome());

    uint32_t virtId = _temporaryMem.as<BaseMem>().baseId();
    VirtReg* virtReg = cc()->virtRegById(virtId);

    cc()->setStackSize(virtId, Support::max(virtReg->virtSize(), size),
                               Support::max(virtReg->alignment(), alignment));
  }

  out = _temporaryMem.as<BaseMem>();
  return kErrorOk;
}

// BaseRAPass - Allocation - Prolog & Epilog
// =========================================

Error BaseRAPass::updateStackFrame() noexcept {
  // Update some StackFrame information that we updated during allocation. The only information we don't have at the
  // moment is final local stack size, which is calculated last.
  FuncFrame& frame = func()->frame();
  for (RegGroup group : RegGroupVirtValues{})
    frame.addDirtyRegs(group, _clobberedRegs[group]);
  frame.setLocalStackAlignment(_stackAllocator.alignment());

  // If there are stack arguments that are not assigned to registers upon entry and the function doesn't require
  // dynamic stack alignment we keep these arguments where they are. This will also mark all stack slots that match
  // these arguments as allocated.
  if (_numStackArgsToStackSlots)
    ASMJIT_PROPAGATE(_markStackArgsToKeep());

  // Calculate offsets of all stack slots and update StackSize to reflect the calculated local stack size.
  ASMJIT_PROPAGATE(_stackAllocator.calculateStackFrame());
  frame.setLocalStackSize(_stackAllocator.stackSize());

  // Update the stack frame based on `_argsAssignment` and finalize it. Finalization means to apply final calculation
  // to the stack layout.
  ASMJIT_PROPAGATE(_argsAssignment.updateFuncFrame(frame));
  ASMJIT_PROPAGATE(frame.finalize());

  // StackAllocator allocates all stots starting from [0], adjust them when necessary.
  if (frame.localStackOffset() != 0)
    ASMJIT_PROPAGATE(_stackAllocator.adjustSlotOffsets(int32_t(frame.localStackOffset())));

  // Again, if there are stack arguments allocated in function's stack we have to handle them. This handles all cases
  // (either regular or dynamic stack alignment).
  if (_numStackArgsToStackSlots)
    ASMJIT_PROPAGATE(_updateStackArgs());

  return kErrorOk;
}

Error BaseRAPass::_markStackArgsToKeep() noexcept {
  FuncFrame& frame = func()->frame();
  bool hasSAReg = frame.hasPreservedFP() || !frame.hasDynamicAlignment();

  RAWorkRegs& workRegs = _workRegs;
  uint32_t numWorkRegs = workRegCount();

  for (uint32_t workId = 0; workId < numWorkRegs; workId++) {
    RAWorkReg* workReg = workRegs[workId];
    if (workReg->hasFlag(RAWorkRegFlags::kStackArgToStack)) {
      ASMJIT_ASSERT(workReg->hasArgIndex());
      const FuncValue& srcArg = _func->detail().arg(workReg->argIndex());

      // If the register doesn't have stack slot then we failed. It doesn't make much sense as it was marked as
      // `kFlagStackArgToStack`, which requires the WorkReg was live-in upon function entry.
      RAStackSlot* slot = workReg->stackSlot();
      if (ASMJIT_UNLIKELY(!slot))
        return DebugUtils::errored(kErrorInvalidState);

      if (hasSAReg && srcArg.isStack() && !srcArg.isIndirect()) {
        uint32_t typeSize = TypeUtils::sizeOf(srcArg.typeId());
        if (typeSize == slot->size()) {
          slot->addFlags(RAStackSlot::kFlagStackArg);
          continue;
        }
      }

      // NOTE: Update StackOffset here so when `_argsAssignment.updateFuncFrame()` is called it will take into
      // consideration moving to stack slots. Without this we may miss some scratch registers later.
      FuncValue& dstArg = _argsAssignment.arg(workReg->argIndex(), workReg->argValueIndex());
      dstArg.assignStackOffset(0);
    }
  }

  return kErrorOk;
}

Error BaseRAPass::_updateStackArgs() noexcept {
  FuncFrame& frame = func()->frame();
  RAWorkRegs& workRegs = _workRegs;
  uint32_t numWorkRegs = workRegCount();

  for (uint32_t workId = 0; workId < numWorkRegs; workId++) {
    RAWorkReg* workReg = workRegs[workId];
    if (workReg->hasFlag(RAWorkRegFlags::kStackArgToStack)) {
      ASMJIT_ASSERT(workReg->hasArgIndex());
      RAStackSlot* slot = workReg->stackSlot();

      if (ASMJIT_UNLIKELY(!slot))
        return DebugUtils::errored(kErrorInvalidState);

      if (slot->isStackArg()) {
        const FuncValue& srcArg = _func->detail().arg(workReg->argIndex());
        if (frame.hasPreservedFP()) {
          slot->setBaseRegId(_fp.id());
          slot->setOffset(int32_t(frame.saOffsetFromSA()) + srcArg.stackOffset());
        }
        else {
          slot->setOffset(int32_t(frame.saOffsetFromSP()) + srcArg.stackOffset());
        }
      }
      else {
        FuncValue& dstArg = _argsAssignment.arg(workReg->argIndex(), workReg->argValueIndex());
        dstArg.setStackOffset(slot->offset());
      }
    }
  }

  return kErrorOk;
}

Error BaseRAPass::insertPrologEpilog() noexcept {
  FuncFrame& frame = _func->frame();

  cc()->_setCursor(func());
  ASMJIT_PROPAGATE(cc()->emitProlog(frame));
  ASMJIT_PROPAGATE(_iEmitHelper->emitArgsAssignment(frame, _argsAssignment));

  cc()->_setCursor(func()->exitNode());
  ASMJIT_PROPAGATE(cc()->emitEpilog(frame));

  return kErrorOk;
}

// BaseRAPass - Rewriter
// =====================

Error BaseRAPass::rewrite() noexcept {
  return _rewrite(_func, _stop);
}

// [[pure virtual]]
Error BaseRAPass::_rewrite(BaseNode* first, BaseNode* stop) noexcept {
  DebugUtils::unused(first, stop);
  return DebugUtils::errored(kErrorInvalidState);
}

// BaseRAPass - Emit
// =================

// [[pure virtual]]
Error BaseRAPass::emitMove(uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
  DebugUtils::unused(workId, dstPhysId, srcPhysId);
  return DebugUtils::errored(kErrorInvalidState);
}

// [[pure virtual]]
Error BaseRAPass::emitSwap(uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept {
  DebugUtils::unused(aWorkId, aPhysId, bWorkId, bPhysId);
  return DebugUtils::errored(kErrorInvalidState);
}

// [[pure virtual]]
Error BaseRAPass::emitLoad(uint32_t workId, uint32_t dstPhysId) noexcept {
  DebugUtils::unused(workId, dstPhysId);
  return DebugUtils::errored(kErrorInvalidState);
}

// [[pure virtual]]
Error BaseRAPass::emitSave(uint32_t workId, uint32_t srcPhysId) noexcept {
  DebugUtils::unused(workId, srcPhysId);
  return DebugUtils::errored(kErrorInvalidState);
}

// [[pure virtual]]
Error BaseRAPass::emitJump(const Label& label) noexcept {
  DebugUtils::unused(label);
  return DebugUtils::errored(kErrorInvalidState);
}

Error BaseRAPass::emitPreCall(InvokeNode* invokeNode) noexcept {
  DebugUtils::unused(invokeNode);
  return DebugUtils::errored(kErrorOk);
}

// BaseRAPass - Logging
// ====================

#ifndef ASMJIT_NO_LOGGING
static void RAPass_formatLiveness(BaseRAPass* pass, String& sb, const RAInst* raInst) noexcept {
  const RATiedReg* tiedRegs = raInst->tiedRegs();
  uint32_t tiedCount = raInst->tiedCount();

  for (uint32_t i = 0; i < tiedCount; i++) {
    const RATiedReg& tiedReg = tiedRegs[i];

    if (i != 0)
      sb.append(' ');

    sb.appendFormat("%s{", pass->workRegById(tiedReg.workId())->name());
    sb.append(tiedReg.isReadWrite() ? 'X' :
              tiedReg.isRead()      ? 'R' :
              tiedReg.isWrite()     ? 'W' : '?');

    if (tiedReg.isLeadConsecutive())
      sb.appendFormat("|Lead[%u]", tiedReg.consecutiveData() + 1u);

    if (tiedReg.hasUseId())
      sb.appendFormat("|Use=%u", tiedReg.useId());
    else if (tiedReg.isUse())
      sb.append("|Use");

    if (tiedReg.isUseConsecutive() && !tiedReg.isLeadConsecutive())
      sb.appendFormat("+%u", tiedReg.consecutiveData());

    if (tiedReg.hasOutId())
      sb.appendFormat("|Out=%u", tiedReg.outId());
    else if (tiedReg.isOut())
      sb.append("|Out");

    if (tiedReg.isOutConsecutive() && !tiedReg.isLeadConsecutive())
      sb.appendFormat("+%u", tiedReg.consecutiveData());

    if (tiedReg.isLast())
      sb.append("|Last");

    if (tiedReg.isKill())
      sb.append("|Kill");

    sb.append("}");
  }
}

ASMJIT_FAVOR_SIZE Error BaseRAPass::annotateCode() noexcept {
  StringTmp<1024> sb;

  for (const RABlock* block : _blocks) {
    BaseNode* node = block->first();
    if (!node) continue;

    BaseNode* last = block->last();
    for (;;) {
      sb.clear();
      Formatter::formatNode(sb, _formatOptions, cc(), node);

      if (hasDiagnosticOption(DiagnosticOptions::kRADebugLiveness) && node->isInst() && node->hasPassData()) {
        const RAInst* raInst = node->passData<RAInst>();
        if (raInst->tiedCount() > 0) {
          sb.padEnd(40);
          sb.append(" | ");
          RAPass_formatLiveness(this, sb, raInst);
        }
      }

      node->setInlineComment(static_cast<char*>(cc()->_dataZone.dup(sb.data(), sb.size(), true)));
      if (node == last)
        break;
      node = node->next();
    }
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error BaseRAPass::_dumpBlockIds(String& sb, const RABlocks& blocks) noexcept {
  for (uint32_t i = 0, size = blocks.size(); i < size; i++) {
    const RABlock* block = blocks[i];
    if (i != 0)
      ASMJIT_PROPAGATE(sb.appendFormat(", #%u", block->blockId()));
    else
      ASMJIT_PROPAGATE(sb.appendFormat("#%u", block->blockId()));
  }
  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error BaseRAPass::_dumpBlockLiveness(String& sb, const RABlock* block) noexcept {
  for (uint32_t liveType = 0; liveType < RABlock::kLiveCount; liveType++) {
    const char* bitsName = liveType == RABlock::kLiveIn  ? "IN  " :
                           liveType == RABlock::kLiveOut ? "OUT " :
                           liveType == RABlock::kLiveGen ? "GEN " : "KILL";

    const ZoneBitVector& bits = block->_liveBits[liveType];
    uint32_t size = bits.size();
    ASMJIT_ASSERT(size <= workRegCount());

    uint32_t n = 0;
    for (uint32_t workId = 0; workId < size; workId++) {
      if (bits.bitAt(workId)) {
        RAWorkReg* wReg = workRegById(workId);

        if (!n)
          sb.appendFormat("    %s [", bitsName);
        else
          sb.append(", ");

        sb.append(wReg->name());
        n++;
      }
    }

    if (n)
      sb.append("]\n");
  }

  return kErrorOk;
}

ASMJIT_FAVOR_SIZE Error BaseRAPass::_dumpLiveSpans(String& sb) noexcept {
  uint32_t numWorkRegs = _workRegs.size();
  uint32_t maxSize = _maxWorkRegNameSize;

  for (uint32_t workId = 0; workId < numWorkRegs; workId++) {
    RAWorkReg* workReg = _workRegs[workId];

    sb.append("  ");

    size_t oldSize = sb.size();
    sb.append(workReg->name());
    sb.padEnd(oldSize + maxSize);

    RALiveStats& stats = workReg->liveStats();
    sb.appendFormat(" {id:%04u width: %-4u freq: %0.4f priority=%0.4f}",
      workReg->virtId(),
      stats.width(),
      stats.freq(),
      stats.priority());
    sb.append(": ");

    LiveRegSpans& liveSpans = workReg->liveSpans();
    for (uint32_t x = 0; x < liveSpans.size(); x++) {
      const LiveRegSpan& liveSpan = liveSpans[x];
      if (x)
        sb.append(", ");
      sb.appendFormat("[%u:%u]", liveSpan.a, liveSpan.b);
    }

    sb.append('\n');
  }

  return kErrorOk;
}
#endif

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
