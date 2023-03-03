// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_RALOCAL_P_H_INCLUDED
#define ASMJIT_CORE_RALOCAL_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/raassignment_p.h"
#include "../core/radefs_p.h"
#include "../core/rapass_p.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_ra
//! \{

//! Local register allocator.
class RALocalAllocator {
public:
  ASMJIT_NONCOPYABLE(RALocalAllocator)

  typedef RAAssignment::PhysToWorkMap PhysToWorkMap;
  typedef RAAssignment::WorkToPhysMap WorkToPhysMap;

  //! Link to `BaseRAPass`.
  BaseRAPass* _pass;
  //! Link to `BaseCompiler`.
  BaseCompiler* _cc;

  //! Architecture traits.
  const ArchTraits* _archTraits;
  //! Registers available to the allocator.
  RARegMask _availableRegs;
  //! Registers clobbered by the allocator.
  RARegMask _clobberedRegs;

  //! Register assignment (current).
  RAAssignment _curAssignment;
  //! Register assignment used temporarily during assignment switches.
  RAAssignment _tmpAssignment;

  //! Link to the current `RABlock`.
  RABlock* _block;
  //! InstNode.
  InstNode* _node;
  //! RA instruction.
  RAInst* _raInst;

  //! Count of all TiedReg's.
  uint32_t _tiedTotal;
  //! TiedReg's total counter.
  RARegCount _tiedCount;

  //! Temporary workToPhysMap that can be used freely by the allocator.
  WorkToPhysMap* _tmpWorkToPhysMap;

  //! \name Construction & Destruction
  //! \{

  inline RALocalAllocator(BaseRAPass* pass) noexcept
    : _pass(pass),
      _cc(pass->cc()),
      _archTraits(pass->_archTraits),
      _availableRegs(pass->_availableRegs),
      _clobberedRegs(),
      _curAssignment(),
      _block(nullptr),
      _node(nullptr),
      _raInst(nullptr),
      _tiedTotal(),
      _tiedCount() {}

  Error init() noexcept;

  //! \}

  //! \name Accessors
  //! \{

  inline RAWorkReg* workRegById(uint32_t workId) const noexcept { return _pass->workRegById(workId); }
  inline PhysToWorkMap* physToWorkMap() const noexcept { return _curAssignment.physToWorkMap(); }
  inline WorkToPhysMap* workToPhysMap() const noexcept { return _curAssignment.workToPhysMap(); }

  //! Returns the currently processed block.
  inline RABlock* block() const noexcept { return _block; }
  //! Sets the currently processed block.
  inline void setBlock(RABlock* block) noexcept { _block = block; }

  //! Returns the currently processed `InstNode`.
  inline InstNode* node() const noexcept { return _node; }
  //! Returns the currently processed `RAInst`.
  inline RAInst* raInst() const noexcept { return _raInst; }

  //! Returns all tied regs as `RATiedReg` array.
  inline RATiedReg* tiedRegs() const noexcept { return _raInst->tiedRegs(); }
  //! Returns tied registers grouped by the given `group`.
  inline RATiedReg* tiedRegs(RegGroup group) const noexcept { return _raInst->tiedRegs(group); }

  //! Returns count of all TiedRegs used by the instruction.
  inline uint32_t tiedCount() const noexcept { return _tiedTotal; }
  //! Returns count of TiedRegs used by the given register `group`.
  inline uint32_t tiedCount(RegGroup group) const noexcept { return _tiedCount.get(group); }

  inline bool isGroupUsed(RegGroup group) const noexcept { return _tiedCount[group] != 0; }

  //! \}

  //! \name Assignment
  //! \{

  Error makeInitialAssignment() noexcept;

  Error replaceAssignment(const PhysToWorkMap* physToWorkMap) noexcept;

  //! Switch to the given assignment by reassigning all register and emitting code that reassigns them.
  //! This is always used to switch to a previously stored assignment.
  //!
  //! If `tryMode` is true then the final assignment doesn't have to be exactly same as specified by `dstPhysToWorkMap`
  //! and `dstWorkToPhysMap`. This mode is only used before conditional jumps that already have assignment to generate
  //! a code sequence that is always executed regardless of the flow.
  Error switchToAssignment(PhysToWorkMap* dstPhysToWorkMap, const ZoneBitVector& liveIn, bool dstReadOnly, bool tryMode) noexcept;

  inline Error spillRegsBeforeEntry(RABlock* block) noexcept {
    return spillScratchGpRegsBeforeEntry(block->entryScratchGpRegs());
  }

  Error spillScratchGpRegsBeforeEntry(uint32_t scratchRegs) noexcept;

  //! \}

  //! \name Allocation
  //! \{

  Error allocInst(InstNode* node) noexcept;
  Error spillAfterAllocation(InstNode* node) noexcept;

  Error allocBranch(InstNode* node, RABlock* target, RABlock* cont) noexcept;
  Error allocJumpTable(InstNode* node, const RABlocks& targets, RABlock* cont) noexcept;

  //! \}

  //! \name Decision Making
  //! \{

  enum CostModel : uint32_t {
    kCostOfFrequency = 1048576,
    kCostOfDirtyFlag = kCostOfFrequency / 4
  };

  inline uint32_t costByFrequency(float freq) const noexcept {
    return uint32_t(int32_t(freq * float(kCostOfFrequency)));
  }

  inline uint32_t calculateSpillCost(RegGroup group, uint32_t workId, uint32_t assignedId) const noexcept {
    RAWorkReg* workReg = workRegById(workId);
    uint32_t cost = costByFrequency(workReg->liveStats().freq());

    if (_curAssignment.isPhysDirty(group, assignedId))
      cost += kCostOfDirtyFlag;

    return cost;
  }

  //! Decides on register assignment.
  uint32_t decideOnAssignment(RegGroup group, uint32_t workId, uint32_t assignedId, RegMask allocableRegs) const noexcept;

  //! Decides on whether to MOVE or SPILL the given WorkReg, because it's allocated in a physical register that have
  //! to be used by another WorkReg.
  //!
  //! The function must return either `RAAssignment::kPhysNone`, which means that the WorkReg of `workId` should be
  //! spilled, or a valid physical register ID, which means that the register should be moved to that physical register
  //! instead.
  uint32_t decideOnReassignment(RegGroup group, uint32_t workId, uint32_t assignedId, RegMask allocableRegs) const noexcept;

  //! Decides on best spill given a register mask `spillableRegs`
  uint32_t decideOnSpillFor(RegGroup group, uint32_t workId, RegMask spillableRegs, uint32_t* spillWorkId) const noexcept;

  //! \}

  //! \name Emit
  //! \{

  //! Emits a move between a destination and source register, and fixes the
  //! register assignment.
  inline Error onMoveReg(RegGroup group, uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
    if (dstPhysId == srcPhysId) return kErrorOk;
    _curAssignment.reassign(group, workId, dstPhysId, srcPhysId);
    return _pass->emitMove(workId, dstPhysId, srcPhysId);
  }

  //! Emits a swap between two physical registers and fixes their assignment.
  //!
  //! \note Target must support this operation otherwise this would ASSERT.
  inline Error onSwapReg(RegGroup group, uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept {
    _curAssignment.swap(group, aWorkId, aPhysId, bWorkId, bPhysId);
    return _pass->emitSwap(aWorkId, aPhysId, bWorkId, bPhysId);
  }

  //! Emits a load from [VirtReg/WorkReg]'s spill slot to a physical register
  //! and makes it assigned and clean.
  inline Error onLoadReg(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    _curAssignment.assign(group, workId, physId, RAAssignment::kClean);
    return _pass->emitLoad(workId, physId);
  }

  //! Emits a save a physical register to a [VirtReg/WorkReg]'s spill slot,
  //! keeps it assigned, and makes it clean.
  inline Error onSaveReg(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    ASMJIT_ASSERT(_curAssignment.workToPhysId(group, workId) == physId);
    ASMJIT_ASSERT(_curAssignment.physToWorkId(group, physId) == workId);

    _curAssignment.makeClean(group, workId, physId);
    return _pass->emitSave(workId, physId);
  }

  //! Assigns a register, the content of it is undefined at this point.
  inline Error onAssignReg(RegGroup group, uint32_t workId, uint32_t physId, bool dirty) noexcept {
    _curAssignment.assign(group, workId, physId, dirty);
    return kErrorOk;
  }

  //! Spills a variable/register, saves the content to the memory-home if modified.
  inline Error onSpillReg(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    if (_curAssignment.isPhysDirty(group, physId))
      ASMJIT_PROPAGATE(onSaveReg(group, workId, physId));
    return onKillReg(group, workId, physId);
  }

  inline Error onDirtyReg(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    _curAssignment.makeDirty(group, workId, physId);
    return kErrorOk;
  }

  inline Error onKillReg(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    _curAssignment.unassign(group, workId, physId);
    return kErrorOk;
  }

  //! \}
};

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_CORE_RALOCAL_P_H_INCLUDED
