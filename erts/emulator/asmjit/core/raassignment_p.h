// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_RAASSIGNMENT_P_H_INCLUDED
#define ASMJIT_CORE_RAASSIGNMENT_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_COMPILER

#include "../core/radefs_p.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_ra
//! \{

//! Holds the current register assignment.
//!
//! Has two purposes:
//!
//!   1. Holds register assignment of a local register allocator (see \ref RALocalAllocator).
//!   2. Holds register assignment of the entry of basic blocks (see \ref RABlock).
class RAAssignment {
public:
  ASMJIT_NONCOPYABLE(RAAssignment)

  enum Ids : uint32_t {
    kPhysNone = 0xFF,
    kWorkNone = RAWorkReg::kIdNone
  };

  enum DirtyBit : uint32_t {
    kClean = 0,
    kDirty = 1
  };

  struct Layout {
    //! Index of architecture registers per group.
    RARegIndex physIndex;
    //! Count of architecture registers per group.
    RARegCount physCount;
    //! Count of physical registers of all groups.
    uint32_t physTotal;
    //! Count of work registers.
    uint32_t workCount;
    //! WorkRegs data (vector).
    const RAWorkRegs* workRegs;

    inline void reset() noexcept {
      physIndex.reset();
      physCount.reset();
      physTotal = 0;
      workCount = 0;
      workRegs = nullptr;
    }
  };

  struct PhysToWorkMap {
    //! Assigned registers (each bit represents one physical reg).
    RARegMask assigned;
    //! Dirty registers (spill slot out of sync or no spill slot).
    RARegMask dirty;
    //! PhysReg to WorkReg mapping.
    uint32_t workIds[1 /* ... */];

    static inline size_t sizeOf(size_t count) noexcept {
      return sizeof(PhysToWorkMap) - sizeof(uint32_t) + count * sizeof(uint32_t);
    }

    inline void reset(size_t count) noexcept {
      assigned.reset();
      dirty.reset();

      for (size_t i = 0; i < count; i++)
        workIds[i] = kWorkNone;
    }

    inline void copyFrom(const PhysToWorkMap* other, size_t count) noexcept {
      size_t size = sizeOf(count);
      memcpy(this, other, size);
    }

    inline void unassign(RegGroup group, uint32_t physId, uint32_t indexInWorkIds) noexcept {
      assigned.clear(group, Support::bitMask(physId));
      dirty.clear(group, Support::bitMask(physId));
      workIds[indexInWorkIds] = kWorkNone;
    }
  };

  struct WorkToPhysMap {
    //! WorkReg to PhysReg mapping
    uint8_t physIds[1 /* ... */];

    static inline size_t sizeOf(size_t count) noexcept {
      return size_t(count) * sizeof(uint8_t);
    }

    inline void reset(size_t count) noexcept {
      for (size_t i = 0; i < count; i++)
        physIds[i] = kPhysNone;
    }

    inline void copyFrom(const WorkToPhysMap* other, size_t count) noexcept {
      size_t size = sizeOf(count);
      if (ASMJIT_LIKELY(size))
        memcpy(this, other, size);
    }
  };

  //! \name Members
  //! \{

  //! Physical registers layout.
  Layout _layout;
  //! WorkReg to PhysReg mapping.
  WorkToPhysMap* _workToPhysMap;
  //! PhysReg to WorkReg mapping and assigned/dirty bits.
  PhysToWorkMap* _physToWorkMap;
  //! Optimization to translate PhysRegs to WorkRegs faster.
  Support::Array<uint32_t*, Globals::kNumVirtGroups> _physToWorkIds;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline RAAssignment() noexcept {
    _layout.reset();
    resetMaps();
  }

  ASMJIT_FORCE_INLINE void initLayout(const RARegCount& physCount, const RAWorkRegs& workRegs) noexcept {
    // Layout must be initialized before data.
    ASMJIT_ASSERT(_physToWorkMap == nullptr);
    ASMJIT_ASSERT(_workToPhysMap == nullptr);

    _layout.physIndex.buildIndexes(physCount);
    _layout.physCount = physCount;
    _layout.physTotal = uint32_t(_layout.physIndex[RegGroup::kMaxVirt]) +
                        uint32_t(_layout.physCount[RegGroup::kMaxVirt]) ;
    _layout.workCount = workRegs.size();
    _layout.workRegs = &workRegs;
  }

  ASMJIT_FORCE_INLINE void initMaps(PhysToWorkMap* physToWorkMap, WorkToPhysMap* workToPhysMap) noexcept {
    _physToWorkMap = physToWorkMap;
    _workToPhysMap = workToPhysMap;
    for (RegGroup group : RegGroupVirtValues{})
      _physToWorkIds[group] = physToWorkMap->workIds + _layout.physIndex.get(group);
  }

  ASMJIT_FORCE_INLINE void resetMaps() noexcept {
    _physToWorkMap = nullptr;
    _workToPhysMap = nullptr;
    _physToWorkIds.fill(nullptr);
  }

  //! \}

  //! \name Accessors
  //! \{

  inline PhysToWorkMap* physToWorkMap() const noexcept { return _physToWorkMap; }
  inline WorkToPhysMap* workToPhysMap() const noexcept { return _workToPhysMap; }

  inline RARegMask& assigned() noexcept { return _physToWorkMap->assigned; }
  inline const RARegMask& assigned() const noexcept { return _physToWorkMap->assigned; }
  inline uint32_t assigned(RegGroup group) const noexcept { return _physToWorkMap->assigned[group]; }

  inline RARegMask& dirty() noexcept { return _physToWorkMap->dirty; }
  inline const RARegMask& dirty() const noexcept { return _physToWorkMap->dirty; }
  inline RegMask dirty(RegGroup group) const noexcept { return _physToWorkMap->dirty[group]; }

  inline uint32_t workToPhysId(RegGroup group, uint32_t workId) const noexcept {
    DebugUtils::unused(group);
    ASMJIT_ASSERT(workId != kWorkNone);
    ASMJIT_ASSERT(workId < _layout.workCount);
    return _workToPhysMap->physIds[workId];
  }

  inline uint32_t physToWorkId(RegGroup group, uint32_t physId) const noexcept {
    ASMJIT_ASSERT(physId < Globals::kMaxPhysRegs);
    return _physToWorkIds[group][physId];
  }

  inline bool isPhysAssigned(RegGroup group, uint32_t physId) const noexcept {
    ASMJIT_ASSERT(physId < Globals::kMaxPhysRegs);
    return Support::bitTest(_physToWorkMap->assigned[group], physId);
  }

  inline bool isPhysDirty(RegGroup group, uint32_t physId) const noexcept {
    ASMJIT_ASSERT(physId < Globals::kMaxPhysRegs);
    return Support::bitTest(_physToWorkMap->dirty[group], physId);
  }

  //! \}

  //! \name Assignment
  //!
  //! These are low-level allocation helpers that are used to update the current mappings between physical and
  //! virt/work registers and also to update masks that represent allocated and dirty registers. These functions
  //! don't emit any code; they are only used to update and keep all mappings in sync.
  //!
  //! \{

  //! Assign [VirtReg/WorkReg] to a physical register.
  inline void assign(RegGroup group, uint32_t workId, uint32_t physId, bool dirty) noexcept {
    ASMJIT_ASSERT(workToPhysId(group, workId) == kPhysNone);
    ASMJIT_ASSERT(physToWorkId(group, physId) == kWorkNone);
    ASMJIT_ASSERT(!isPhysAssigned(group, physId));
    ASMJIT_ASSERT(!isPhysDirty(group, physId));

    _workToPhysMap->physIds[workId] = uint8_t(physId);
    _physToWorkIds[group][physId] = workId;

    RegMask regMask = Support::bitMask(physId);
    _physToWorkMap->assigned[group] |= regMask;
    _physToWorkMap->dirty[group] |= regMask & Support::bitMaskFromBool<RegMask>(dirty);

    verify();
  }

  //! Reassign [VirtReg/WorkReg] to `dstPhysId` from `srcPhysId`.
  inline void reassign(RegGroup group, uint32_t workId, uint32_t dstPhysId, uint32_t srcPhysId) noexcept {
    ASMJIT_ASSERT(dstPhysId != srcPhysId);
    ASMJIT_ASSERT(workToPhysId(group, workId) == srcPhysId);
    ASMJIT_ASSERT(physToWorkId(group, srcPhysId) == workId);
    ASMJIT_ASSERT(isPhysAssigned(group, srcPhysId) == true);
    ASMJIT_ASSERT(isPhysAssigned(group, dstPhysId) == false);

    _workToPhysMap->physIds[workId] = uint8_t(dstPhysId);
    _physToWorkIds[group][srcPhysId] = kWorkNone;
    _physToWorkIds[group][dstPhysId] = workId;

    RegMask srcMask = Support::bitMask(srcPhysId);
    RegMask dstMask = Support::bitMask(dstPhysId);

    bool dirty = (_physToWorkMap->dirty[group] & srcMask) != 0;
    RegMask regMask = dstMask | srcMask;

    _physToWorkMap->assigned[group] ^= regMask;
    _physToWorkMap->dirty[group] ^= regMask & Support::bitMaskFromBool<RegMask>(dirty);

    verify();
  }

  inline void swap(RegGroup group, uint32_t aWorkId, uint32_t aPhysId, uint32_t bWorkId, uint32_t bPhysId) noexcept {
    ASMJIT_ASSERT(aPhysId != bPhysId);
    ASMJIT_ASSERT(workToPhysId(group, aWorkId) == aPhysId);
    ASMJIT_ASSERT(workToPhysId(group, bWorkId) == bPhysId);
    ASMJIT_ASSERT(physToWorkId(group, aPhysId) == aWorkId);
    ASMJIT_ASSERT(physToWorkId(group, bPhysId) == bWorkId);
    ASMJIT_ASSERT(isPhysAssigned(group, aPhysId));
    ASMJIT_ASSERT(isPhysAssigned(group, bPhysId));

    _workToPhysMap->physIds[aWorkId] = uint8_t(bPhysId);
    _workToPhysMap->physIds[bWorkId] = uint8_t(aPhysId);
    _physToWorkIds[group][aPhysId] = bWorkId;
    _physToWorkIds[group][bPhysId] = aWorkId;

    RegMask aMask = Support::bitMask(aPhysId);
    RegMask bMask = Support::bitMask(bPhysId);
    RegMask flipMask = Support::bitMaskFromBool<RegMask>(((_physToWorkMap->dirty[group] & aMask) != 0) ^ ((_physToWorkMap->dirty[group] & bMask) != 0));
    RegMask regMask = aMask | bMask;
    _physToWorkMap->dirty[group] ^= regMask & flipMask;

    verify();
  }

  //! Unassign [VirtReg/WorkReg] from a physical register.
  inline void unassign(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    ASMJIT_ASSERT(physId < Globals::kMaxPhysRegs);
    ASMJIT_ASSERT(workToPhysId(group, workId) == physId);
    ASMJIT_ASSERT(physToWorkId(group, physId) == workId);
    ASMJIT_ASSERT(isPhysAssigned(group, physId));

    _workToPhysMap->physIds[workId] = kPhysNone;
    _physToWorkIds[group][physId] = kWorkNone;

    RegMask regMask = Support::bitMask(physId);
    _physToWorkMap->assigned[group] &= ~regMask;
    _physToWorkMap->dirty[group] &= ~regMask;

    verify();
  }

  inline void makeClean(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    DebugUtils::unused(workId);
    RegMask regMask = Support::bitMask(physId);
    _physToWorkMap->dirty[group] &= ~regMask;
  }

  inline void makeDirty(RegGroup group, uint32_t workId, uint32_t physId) noexcept {
    DebugUtils::unused(workId);
    RegMask regMask = Support::bitMask(physId);
    _physToWorkMap->dirty[group] |= regMask;
  }

  //! \}

  //! \name Utilities
  //! \{

  ASMJIT_FORCE_INLINE void swap(RAAssignment& other) noexcept {
    std::swap(_workToPhysMap, other._workToPhysMap);
    std::swap(_physToWorkMap, other._physToWorkMap);
    _physToWorkIds.swap(other._physToWorkIds);
  }

  inline void assignWorkIdsFromPhysIds() noexcept {
    memset(_workToPhysMap, uint8_t(BaseReg::kIdBad), WorkToPhysMap::sizeOf(_layout.workCount));

    for (RegGroup group : RegGroupVirtValues{}) {
      uint32_t physBaseIndex = _layout.physIndex[group];
      Support::BitWordIterator<RegMask> it(_physToWorkMap->assigned[group]);

      while (it.hasNext()) {
        uint32_t physId = it.next();
        uint32_t workId = _physToWorkMap->workIds[physBaseIndex + physId];

        ASMJIT_ASSERT(workId != kWorkNone);
        _workToPhysMap->physIds[workId] = uint8_t(physId);
      }
    }
  }

  inline void copyFrom(const PhysToWorkMap* physToWorkMap) noexcept {
    memcpy(_physToWorkMap, physToWorkMap, PhysToWorkMap::sizeOf(_layout.physTotal));
    assignWorkIdsFromPhysIds();
  }

  inline void copyFrom(const PhysToWorkMap* physToWorkMap, const WorkToPhysMap* workToPhysMap) noexcept {
    memcpy(_physToWorkMap, physToWorkMap, PhysToWorkMap::sizeOf(_layout.physTotal));
    memcpy(_workToPhysMap, workToPhysMap, WorkToPhysMap::sizeOf(_layout.workCount));
  }

  inline void copyFrom(const RAAssignment& other) noexcept {
    copyFrom(other.physToWorkMap(), other.workToPhysMap());
  }

  // Not really useful outside of debugging.
  bool equals(const RAAssignment& other) const noexcept {
    // Layout should always match.
    if (_layout.physIndex != other._layout.physIndex ||
        _layout.physCount != other._layout.physCount ||
        _layout.physTotal != other._layout.physTotal ||
        _layout.workCount != other._layout.workCount ||
        _layout.workRegs  != other._layout.workRegs)
      return false;

    uint32_t physTotal = _layout.physTotal;
    uint32_t workCount = _layout.workCount;

    for (uint32_t physId = 0; physId < physTotal; physId++) {
      uint32_t thisWorkId = _physToWorkMap->workIds[physId];
      uint32_t otherWorkId = other._physToWorkMap->workIds[physId];
      if (thisWorkId != otherWorkId)
        return false;
    }

    for (uint32_t workId = 0; workId < workCount; workId++) {
      uint32_t thisPhysId = _workToPhysMap->physIds[workId];
      uint32_t otherPhysId = other._workToPhysMap->physIds[workId];
      if (thisPhysId != otherPhysId)
        return false;
    }

    if (_physToWorkMap->assigned != other._physToWorkMap->assigned ||
        _physToWorkMap->dirty    != other._physToWorkMap->dirty    )
      return false;

    return true;
  }

#if defined(ASMJIT_BUILD_DEBUG)
  ASMJIT_NOINLINE void verify() noexcept {
    // Verify WorkToPhysMap.
    {
      for (uint32_t workId = 0; workId < _layout.workCount; workId++) {
        uint32_t physId = _workToPhysMap->physIds[workId];
        if (physId != kPhysNone) {
          const RAWorkReg* workReg = _layout.workRegs->at(workId);
          RegGroup group = workReg->group();
          ASMJIT_ASSERT(_physToWorkIds[group][physId] == workId);
        }
      }
    }

    // Verify PhysToWorkMap.
    {
      for (RegGroup group : RegGroupVirtValues{}) {
        uint32_t physCount = _layout.physCount[group];
        for (uint32_t physId = 0; physId < physCount; physId++) {
          uint32_t workId = _physToWorkIds[group][physId];
          if (workId != kWorkNone) {
            ASMJIT_ASSERT(_workToPhysMap->physIds[workId] == physId);
          }
        }
      }
    }
  }
#else
  inline void verify() noexcept {}
#endif

  //! \}
};

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_COMPILER
#endif // ASMJIT_CORE_RAASSIGNMENT_P_H_INCLUDED
