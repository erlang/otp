// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_RADEFS_P_H_INCLUDED
#define ASMJIT_CORE_RADEFS_P_H_INCLUDED

#include "../core/api-config.h"
#include "../core/archtraits.h"
#include "../core/compilerdefs.h"
#include "../core/logger.h"
#include "../core/operand.h"
#include "../core/support.h"
#include "../core/type.h"
#include "../core/zone.h"
#include "../core/zonevector.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_ra
//! \{

#ifndef ASMJIT_NO_LOGGING
# define ASMJIT_RA_LOG_FORMAT(...)  \
  do {                              \
    if (logger)                     \
      logger->logf(__VA_ARGS__);    \
  } while (0)
# define ASMJIT_RA_LOG_COMPLEX(...) \
  do {                              \
    if (logger) {                   \
      __VA_ARGS__                   \
    }                               \
  } while (0)
#else
# define ASMJIT_RA_LOG_FORMAT(...) ((void)0)
# define ASMJIT_RA_LOG_COMPLEX(...) ((void)0)
#endif

class BaseRAPass;
class RABlock;
class BaseNode;
struct RAStackSlot;

typedef ZoneVector<RABlock*> RABlocks;
typedef ZoneVector<RAWorkReg*> RAWorkRegs;

//! Maximum number of consecutive registers aggregated from all supported backends.
static constexpr uint32_t kMaxConsecutiveRegs = 4;

//! Provides architecture constraints used by register allocator.
class RAConstraints {
public:
  //! \name Members
  //! \{

  Support::Array<RegMask, Globals::kNumVirtGroups> _availableRegs {};

  //! \}

  ASMJIT_NOINLINE Error init(Arch arch) noexcept {
    switch (arch) {
      case Arch::kX86:
      case Arch::kX64: {
        uint32_t registerCount = arch == Arch::kX86 ? 8 : 16;
        _availableRegs[RegGroup::kGp] = Support::lsbMask<RegMask>(registerCount) & ~Support::bitMask(4u);
        _availableRegs[RegGroup::kVec] = Support::lsbMask<RegMask>(registerCount);
        _availableRegs[RegGroup::kExtraVirt2] = Support::lsbMask<RegMask>(8);
        _availableRegs[RegGroup::kExtraVirt3] = Support::lsbMask<RegMask>(8);
        return kErrorOk;
      }

      case Arch::kAArch64: {
        _availableRegs[RegGroup::kGp] = 0xFFFFFFFFu & ~Support::bitMask(18, 31u);
        _availableRegs[RegGroup::kVec] = 0xFFFFFFFFu;
        _availableRegs[RegGroup::kExtraVirt2] = 0;
        _availableRegs[RegGroup::kExtraVirt3] = 0;
        return kErrorOk;
      }

      default:
        return DebugUtils::errored(kErrorInvalidArch);
    }
  }

  inline RegMask availableRegs(RegGroup group) const noexcept { return _availableRegs[group]; }
};

enum class RAStrategyType : uint8_t {
  kSimple  = 0,
  kComplex = 1
};
ASMJIT_DEFINE_ENUM_COMPARE(RAStrategyType)

enum class RAStrategyFlags : uint8_t {
  kNone = 0
};
ASMJIT_DEFINE_ENUM_FLAGS(RAStrategyFlags)

//! Register allocation strategy.
//!
//! The idea is to select the best register allocation strategy for each virtual register group based on the
//! complexity of the code.
struct RAStrategy {
  //! \name Members
  //! \{

  RAStrategyType _type = RAStrategyType::kSimple;
  RAStrategyFlags _flags = RAStrategyFlags::kNone;

  //! \}

  //! \name Accessors
  //! \{

  inline void reset() noexcept {
    _type = RAStrategyType::kSimple;
    _flags = RAStrategyFlags::kNone;
  }

  inline RAStrategyType type() const noexcept { return _type; }
  inline void setType(RAStrategyType type) noexcept { _type = type; }

  inline bool isSimple() const noexcept { return _type == RAStrategyType::kSimple; }
  inline bool isComplex() const noexcept { return _type >= RAStrategyType::kComplex; }

  inline RAStrategyFlags flags() const noexcept { return _flags; }
  inline bool hasFlag(RAStrategyFlags flag) const noexcept { return Support::test(_flags, flag); }
  inline void addFlags(RAStrategyFlags flags) noexcept { _flags |= flags; }

  //! \}
};

//! Count of virtual or physical registers per group.
//!
//! \note This class uses 8-bit integers to represent counters, it's only used in places where this is sufficient,
//! for example total count of machine's physical registers, count of virtual registers per instruction, etc...
//! There is also `RALiveCount`, which uses 32-bit integers and is indeed much safer.
struct RARegCount {
  //! \name Members
  //! \{

  union {
    uint8_t _regs[4];
    uint32_t _packed;
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Resets all counters to zero.
  inline void reset() noexcept { _packed = 0; }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline uint8_t& operator[](RegGroup group) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _regs[size_t(group)];
  }

  inline const uint8_t& operator[](RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    return _regs[size_t(group)];
  }

  inline bool operator==(const RARegCount& other) const noexcept { return _packed == other._packed; }
  inline bool operator!=(const RARegCount& other) const noexcept { return _packed != other._packed; }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the count of registers by the given register `group`.
  inline uint32_t get(RegGroup group) const noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);

    uint32_t shift = Support::byteShiftOfDWordStruct(uint32_t(group));
    return (_packed >> shift) & uint32_t(0xFF);
  }

  //! Sets the register count by a register `group`.
  inline void set(RegGroup group, uint32_t n) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    ASMJIT_ASSERT(n <= 0xFF);

    uint32_t shift = Support::byteShiftOfDWordStruct(uint32_t(group));
    _packed = (_packed & ~uint32_t(0xFF << shift)) + (n << shift);
  }

  //! Adds the register count by a register `group`.
  inline void add(RegGroup group, uint32_t n = 1) noexcept {
    ASMJIT_ASSERT(group <= RegGroup::kMaxVirt);
    ASMJIT_ASSERT(0xFF - uint32_t(_regs[size_t(group)]) >= n);

    uint32_t shift = Support::byteShiftOfDWordStruct(uint32_t(group));
    _packed += n << shift;
  }

  //! \}
};

//! Provides mapping that can be used to fast index architecture register groups.
struct RARegIndex : public RARegCount {
  //! Build register indexes based on the given `count` of registers.
  ASMJIT_FORCE_INLINE void buildIndexes(const RARegCount& count) noexcept {
    uint32_t x = uint32_t(count._regs[0]);
    uint32_t y = uint32_t(count._regs[1]) + x;
    uint32_t z = uint32_t(count._regs[2]) + y;

    ASMJIT_ASSERT(y <= 0xFF);
    ASMJIT_ASSERT(z <= 0xFF);
    _packed = Support::bytepack32_4x8(0, x, y, z);
  }
};

//! Registers mask.
struct RARegMask {
  //! \name Members
  //! \{

  Support::Array<RegMask, Globals::kNumVirtGroups> _masks;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline void init(const RARegMask& other) noexcept { _masks = other._masks; }
  //! Reset all register masks to zero.
  inline void reset() noexcept { _masks.fill(0); }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline bool operator==(const RARegMask& other) const noexcept { return _masks == other._masks; }
  inline bool operator!=(const RARegMask& other) const noexcept { return _masks != other._masks; }

  template<typename Index>
  inline uint32_t& operator[](const Index& index) noexcept { return _masks[index]; }

  template<typename Index>
  inline const uint32_t& operator[](const Index& index) const noexcept { return _masks[index]; }

  //! \}

  //! \name Utilities
  //! \{

  //! Tests whether all register masks are zero (empty).
  inline bool empty() const noexcept {
    return _masks.aggregate<Support::Or>() == 0;
  }

  inline bool has(RegGroup group, RegMask mask = 0xFFFFFFFFu) const noexcept {
    return (_masks[group] & mask) != 0;
  }

  template<class Operator>
  inline void op(const RARegMask& other) noexcept {
    _masks.combine<Operator>(other._masks);
  }

  template<class Operator>
  inline void op(RegGroup group, RegMask mask) noexcept {
    _masks[group] = Operator::op(_masks[group], mask);
  }

  inline void clear(RegGroup group, RegMask mask) noexcept {
    _masks[group] = _masks[group] & ~mask;
  }

  //! \}
};

//! Information associated with each instruction, propagated to blocks, loops, and the whole function. This
//! information can be used to do minor decisions before the register allocator tries to do its job. For
//! example to use fast register allocation inside a block or loop it cannot have clobbered and/or fixed
//! registers, etc...
class RARegsStats {
public:
  //! \name Constants
  //! \{

  enum Index : uint32_t {
    kIndexUsed       = 0,
    kIndexFixed      = 8,
    kIndexClobbered  = 16
  };

  enum Mask : uint32_t {
    kMaskUsed        = 0xFFu << kIndexUsed,
    kMaskFixed       = 0xFFu << kIndexFixed,
    kMaskClobbered   = 0xFFu << kIndexClobbered
  };

  //! \}

  //! \name Members
  //! \{

  uint32_t _packed = 0;

  //! \}

  //! \name Accessors
  //! \{

  inline void reset() noexcept { _packed = 0; }
  inline void combineWith(const RARegsStats& other) noexcept { _packed |= other._packed; }

  inline bool hasUsed() const noexcept { return (_packed & kMaskUsed) != 0u; }
  inline bool hasUsed(RegGroup group) const noexcept { return (_packed & Support::bitMask(kIndexUsed + uint32_t(group))) != 0u; }
  inline void makeUsed(RegGroup group) noexcept { _packed |= Support::bitMask(kIndexUsed + uint32_t(group)); }

  inline bool hasFixed() const noexcept { return (_packed & kMaskFixed) != 0u; }
  inline bool hasFixed(RegGroup group) const noexcept { return (_packed & Support::bitMask(kIndexFixed + uint32_t(group))) != 0u; }
  inline void makeFixed(RegGroup group) noexcept { _packed |= Support::bitMask(kIndexFixed + uint32_t(group)); }

  inline bool hasClobbered() const noexcept { return (_packed & kMaskClobbered) != 0u; }
  inline bool hasClobbered(RegGroup group) const noexcept { return (_packed & Support::bitMask(kIndexClobbered + uint32_t(group))) != 0u; }
  inline void makeClobbered(RegGroup group) noexcept { _packed |= Support::bitMask(kIndexClobbered + uint32_t(group)); }

  //! \}
};

//! Count of live registers, per group.
class RALiveCount {
public:
  //! \name Members
  //! \{

  Support::Array<uint32_t, Globals::kNumVirtGroups> n {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline RALiveCount() noexcept = default;
  inline RALiveCount(const RALiveCount& other) noexcept = default;

  inline void init(const RALiveCount& other) noexcept { n = other.n; }
  inline void reset() noexcept { n.fill(0); }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline RALiveCount& operator=(const RALiveCount& other) noexcept = default;

  inline uint32_t& operator[](RegGroup group) noexcept { return n[group]; }
  inline const uint32_t& operator[](RegGroup group) const noexcept { return n[group]; }

  //! \}

  //! \name Utilities
  //! \{

  template<class Operator>
  inline void op(const RALiveCount& other) noexcept { n.combine<Operator>(other.n); }

  //! \}
};

struct RALiveInterval {
  //! \name Constants
  //! \{

  enum : uint32_t {
    kNaN = 0,
    kInf = 0xFFFFFFFFu
  };

  //! \}

  //! \name Members
  //! \{

  uint32_t a, b;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline RALiveInterval() noexcept : a(0), b(0) {}
  inline RALiveInterval(uint32_t a, uint32_t b) noexcept : a(a), b(b) {}
  inline RALiveInterval(const RALiveInterval& other) noexcept : a(other.a), b(other.b) {}

  inline void init(uint32_t aVal, uint32_t bVal) noexcept {
    a = aVal;
    b = bVal;
  }
  inline void init(const RALiveInterval& other) noexcept { init(other.a, other.b); }
  inline void reset() noexcept { init(0, 0); }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline RALiveInterval& operator=(const RALiveInterval& other) = default;

  //! \}

  //! \name Accessors
  //! \{

  inline bool isValid() const noexcept { return a < b; }
  inline uint32_t width() const noexcept { return b - a; }

  //! \}
};

//! Live span with payload of type `T`.
template<typename T>
class RALiveSpan : public RALiveInterval, public T {
public:
  //! \name Types
  //! \{

  typedef T DataType;

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline RALiveSpan() noexcept : RALiveInterval(), T() {}
  inline RALiveSpan(const RALiveSpan<T>& other) noexcept : RALiveInterval(other), T() {}
  inline RALiveSpan(const RALiveInterval& interval, const T& data) noexcept : RALiveInterval(interval), T(data) {}
  inline RALiveSpan(uint32_t a, uint32_t b) noexcept : RALiveInterval(a, b), T() {}
  inline RALiveSpan(uint32_t a, uint32_t b, const T& data) noexcept : RALiveInterval(a, b), T(data) {}

  inline void init(const RALiveSpan<T>& other) noexcept {
    RALiveInterval::init(static_cast<const RALiveInterval&>(other));
    T::init(static_cast<const T&>(other));
  }

  inline void init(const RALiveSpan<T>& span, const T& data) noexcept {
    RALiveInterval::init(static_cast<const RALiveInterval&>(span));
    T::init(data);
  }

  inline void init(const RALiveInterval& interval, const T& data) noexcept {
    RALiveInterval::init(interval);
    T::init(data);
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline RALiveSpan& operator=(const RALiveSpan& other) {
    init(other);
    return *this;
  }

  //! \}
};

//! Vector of `RALiveSpan<T>` with additional convenience API.
template<typename T>
class RALiveSpans {
public:
  ASMJIT_NONCOPYABLE(RALiveSpans)

  typedef typename T::DataType DataType;
  ZoneVector<T> _data;

  //! \name Construction & Destruction
  //! \{

  inline RALiveSpans() noexcept : _data() {}

  inline void reset() noexcept { _data.reset(); }
  inline void release(ZoneAllocator* allocator) noexcept { _data.release(allocator); }

  //! \}

  //! \name Accessors
  //! \{

  inline bool empty() const noexcept { return _data.empty(); }
  inline uint32_t size() const noexcept { return _data.size(); }

  inline T* data() noexcept { return _data.data(); }
  inline const T* data() const noexcept { return _data.data(); }

  inline bool isOpen() const noexcept {
    uint32_t size = _data.size();
    return size > 0 && _data[size - 1].b == RALiveInterval::kInf;
  }

  //! \}

  //! \name Utilities
  //! \{

  inline void swap(RALiveSpans<T>& other) noexcept { _data.swap(other._data); }

  //! Open the current live span.
  ASMJIT_FORCE_INLINE Error openAt(ZoneAllocator* allocator, uint32_t start, uint32_t end) noexcept {
    bool wasOpen;
    return openAt(allocator, start, end, wasOpen);
  }

  ASMJIT_FORCE_INLINE Error openAt(ZoneAllocator* allocator, uint32_t start, uint32_t end, bool& wasOpen) noexcept {
    uint32_t size = _data.size();
    wasOpen = false;

    if (size > 0) {
      T& last = _data[size - 1];
      if (last.b >= start) {
        wasOpen = last.b > start;
        last.b = end;
        return kErrorOk;
      }
    }

    return _data.append(allocator, T(start, end));
  }

  ASMJIT_FORCE_INLINE void closeAt(uint32_t end) noexcept {
    ASMJIT_ASSERT(!empty());

    uint32_t size = _data.size();
    _data[size - 1].b = end;
  }

  //! Returns the sum of width of all spans.
  //!
  //! \note Don't overuse, this iterates over all spans so it's O(N). It should be only called once and then cached.
  inline uint32_t width() const noexcept {
    uint32_t width = 0;
    for (const T& span : _data)
      width += span.width();
    return width;
  }

  inline T& operator[](uint32_t index) noexcept { return _data[index]; }
  inline const T& operator[](uint32_t index) const noexcept { return _data[index]; }

  inline bool intersects(const RALiveSpans<T>& other) const noexcept {
    return intersects(*this, other);
  }

  ASMJIT_FORCE_INLINE Error nonOverlappingUnionOf(ZoneAllocator* allocator, const RALiveSpans<T>& x, const RALiveSpans<T>& y, const DataType& yData) noexcept {
    uint32_t finalSize = x.size() + y.size();
    ASMJIT_PROPAGATE(_data.reserve(allocator, finalSize));

    T* dstPtr = _data.data();
    const T* xSpan = x.data();
    const T* ySpan = y.data();

    const T* xEnd = xSpan + x.size();
    const T* yEnd = ySpan + y.size();

    // Loop until we have intersection or either `xSpan == xEnd` or `ySpan == yEnd`, which means that there is no
    // intersection. We advance either `xSpan` or `ySpan` depending on their ranges.
    if (xSpan != xEnd && ySpan != yEnd) {
      uint32_t xa, ya;
      xa = xSpan->a;
      for (;;) {
        while (ySpan->b <= xa) {
          dstPtr->init(*ySpan, yData);
          dstPtr++;
          if (++ySpan == yEnd)
            goto Done;
        }

        ya = ySpan->a;
        while (xSpan->b <= ya) {
          *dstPtr++ = *xSpan;
          if (++xSpan == xEnd)
            goto Done;
        }

        // We know that `xSpan->b > ySpan->a`, so check if `ySpan->b > xSpan->a`.
        xa = xSpan->a;
        if (ySpan->b > xa)
          return 0xFFFFFFFFu;
      }
    }

  Done:
    while (xSpan != xEnd) {
      *dstPtr++ = *xSpan++;
    }

    while (ySpan != yEnd) {
      dstPtr->init(*ySpan, yData);
      dstPtr++;
      ySpan++;
    }

    _data._setEndPtr(dstPtr);
    return kErrorOk;
  }

  static ASMJIT_FORCE_INLINE bool intersects(const RALiveSpans<T>& x, const RALiveSpans<T>& y) noexcept {
    const T* xSpan = x.data();
    const T* ySpan = y.data();

    const T* xEnd = xSpan + x.size();
    const T* yEnd = ySpan + y.size();

    // Loop until we have intersection or either `xSpan == xEnd` or `ySpan == yEnd`, which means that there is no
    // intersection. We advance either `xSpan` or `ySpan` depending on their end positions.
    if (xSpan == xEnd || ySpan == yEnd)
      return false;

    uint32_t xa, ya;
    xa = xSpan->a;

    for (;;) {
      while (ySpan->b <= xa)
        if (++ySpan == yEnd)
          return false;

      ya = ySpan->a;
      while (xSpan->b <= ya)
        if (++xSpan == xEnd)
          return false;

      // We know that `xSpan->b > ySpan->a`, so check if `ySpan->b > xSpan->a`.
      xa = xSpan->a;
      if (ySpan->b > xa)
        return true;
    }
  }

  //! \}
};

//! Statistics about a register liveness.
class RALiveStats {
public:
  uint32_t _width = 0;
  float _freq = 0.0f;
  float _priority = 0.0f;

  //! \name Accessors
  //! \{

  inline uint32_t width() const noexcept { return _width; }
  inline float freq() const noexcept { return _freq; }
  inline float priority() const noexcept { return _priority; }

  //! \}
};

struct LiveRegData {
  uint32_t id;

  inline explicit LiveRegData(uint32_t id = BaseReg::kIdBad) noexcept : id(id) {}
  inline LiveRegData(const LiveRegData& other) noexcept = default;

  inline void init(const LiveRegData& other) noexcept { id = other.id; }

  inline bool operator==(const LiveRegData& other) const noexcept { return id == other.id; }
  inline bool operator!=(const LiveRegData& other) const noexcept { return id != other.id; }
};

typedef RALiveSpan<LiveRegData> LiveRegSpan;
typedef RALiveSpans<LiveRegSpan> LiveRegSpans;

//! Flags used by \ref RATiedReg.
//!
//! Register access information is encoded in 4 flags in total:
//!
//!   - `kRead`  - Register is Read    (ReadWrite if combined with `kWrite`).
//!   - `kWrite` - Register is Written (ReadWrite if combined with `kRead`).
//!   - `kUse`   - Encoded as Read or ReadWrite.
//!   - `kOut`   - Encoded as WriteOnly.
//!
//! Let's describe all of these on two X86 instructions:
//!
//!   - ADD x{R|W|Use},  x{R|Use}              -> {x:R|W|Use            }
//!   - LEA x{  W|Out}, [x{R|Use} + x{R|Out}]  -> {x:R|W|Use|Out        }
//!   - ADD x{R|W|Use},  y{R|Use}              -> {x:R|W|Use     y:R|Use}
//!   - LEA x{  W|Out}, [x{R|Use} + y{R|Out}]  -> {x:R|W|Use|Out y:R|Use}
//!
//! It should be obvious from the example above how these flags get created. Each operand contains READ/WRITE
//! information, which is then merged to RATiedReg's flags. However, we also need to represent the possitility
//! to view the operation as two independent operations - USE and OUT, because the register allocator first
//! allocates USE registers, and then assigns OUT registers independently of USE registers.
enum class RATiedFlags : uint32_t {
  //! No flags.
  kNone = 0,

  // Access Flags
  // ------------

  //! Register is read.
  kRead = uint32_t(OpRWFlags::kRead),
  //! Register is written.
  kWrite = uint32_t(OpRWFlags::kWrite),
  //! Register both read and written.
  kRW = uint32_t(OpRWFlags::kRW),

  // Use / Out Flags
  // ---------------

  //! Register has a USE slot (read/rw).
  kUse = 0x00000004u,
  //! Register has an OUT slot (write-only).
  kOut = 0x00000008u,
  //! Register in USE slot can be patched to memory.
  kUseRM = 0x00000010u,
  //! Register in OUT slot can be patched to memory.
  kOutRM = 0x00000020u,

  //! Register has a fixed USE slot.
  kUseFixed = 0x00000040u,
  //! Register has a fixed OUT slot.
  kOutFixed = 0x00000080u,
  //! Register USE slot has been allocated.
  kUseDone = 0x00000100u,
  //! Register OUT slot has been allocated.
  kOutDone = 0x00000200u,

  // Consecutive Flags / Data
  // ------------------------

  kUseConsecutive = 0x00000400u,
  kOutConsecutive = 0x00000800u,
  kLeadConsecutive = 0x00001000u,
  kConsecutiveData = 0x00006000u,

  // Liveness Flags
  // --------------

  //! Register must be duplicated (function call only).
  kDuplicate = 0x00010000u,
  //! Last occurrence of this VirtReg in basic block.
  kLast = 0x00020000u,
  //! Kill this VirtReg after use.
  kKill = 0x00040000u,

  // X86 Specific Flags
  // ------------------

  // Architecture specific flags are used during RATiedReg building to ensure that architecture-specific constraints
  // are handled properly. These flags are not really needed after RATiedReg[] is built and copied to `RAInst`.

  //! This RATiedReg references GPB-LO or GPB-HI.
  kX86_Gpb = 0x01000000u,

  // Instruction Flags (Never used by RATiedReg)
  // -------------------------------------------

  //! Instruction is transformable to another instruction if necessary.
  //!
  //! This is flag that is only used by \ref RAInst to inform register allocator that the instruction has some
  //! constraints that can only be solved by transforming the instruction into another instruction, most likely
  //! by changing its InstId.
  kInst_IsTransformable = 0x80000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(RATiedFlags)

static_assert(uint32_t(RATiedFlags::kRead ) == 0x1, "RATiedFlags::kRead must be 0x1");
static_assert(uint32_t(RATiedFlags::kWrite) == 0x2, "RATiedFlags::kWrite must be 0x2");
static_assert(uint32_t(RATiedFlags::kRW   ) == 0x3, "RATiedFlags::kRW must be 0x3");

//! Tied register merges one ore more register operand into a single entity. It contains information about its access
//! (Read|Write) and allocation slots (Use|Out) that are used by the register allocator and liveness analysis.
struct RATiedReg {
  //! \name Members
  //! \{

  //! WorkReg id.
  uint32_t _workId;
  //! WorkReg id that is an immediate consecutive parent of this register, or Globals::kInvalidId if it has no parent.
  uint32_t _consecutiveParent;
  //! Allocation flags.
  RATiedFlags _flags;

  union {
    struct {
      //! How many times the VirtReg is referenced in all operands.
      uint8_t _refCount;
      //! Size of a memory operand in case that it's use instead of the register.
      uint8_t _rmSize;
      //! Physical register for use operation (ReadOnly / ReadWrite).
      uint8_t _useId;
      //! Physical register for out operation (WriteOnly).
      uint8_t _outId;
    };
    //! Packed data.
    uint32_t _packed;
  };

  //! Registers where inputs {R|X} can be allocated to.
  RegMask _useRegMask;
  //! Registers where outputs {W} can be allocated to.
  RegMask _outRegMask;
  //! Indexes used to rewrite USE regs.
  uint32_t _useRewriteMask;
  //! Indexes used to rewrite OUT regs.
  uint32_t _outRewriteMask;

  //! \}

  //! \name Statics
  //! \{

  static inline RATiedFlags consecutiveDataToFlags(uint32_t offset) noexcept {
    ASMJIT_ASSERT(offset < 4);
    constexpr uint32_t kOffsetShift = Support::ConstCTZ<uint32_t(RATiedFlags::kConsecutiveData)>::value;
    return (RATiedFlags)(offset << kOffsetShift);
  }

  static inline uint32_t consecutiveDataFromFlags(RATiedFlags flags) noexcept {
    constexpr uint32_t kOffsetShift = Support::ConstCTZ<uint32_t(RATiedFlags::kConsecutiveData)>::value;
    return uint32_t(flags & RATiedFlags::kConsecutiveData) >> kOffsetShift;
  }

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline void init(uint32_t workId, RATiedFlags flags, RegMask useRegMask, uint32_t useId, uint32_t useRewriteMask, RegMask outRegMask, uint32_t outId, uint32_t outRewriteMask, uint32_t rmSize = 0, uint32_t consecutiveParent = Globals::kInvalidId) noexcept {
    _workId = workId;
    _consecutiveParent = consecutiveParent;
    _flags = flags;
    _refCount = 1;
    _rmSize = uint8_t(rmSize);
    _useId = uint8_t(useId);
    _outId = uint8_t(outId);
    _useRegMask = useRegMask;
    _outRegMask = outRegMask;
    _useRewriteMask = useRewriteMask;
    _outRewriteMask = outRewriteMask;
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the associated WorkReg id.
  inline uint32_t workId() const noexcept { return _workId; }

  inline bool hasConsecutiveParent() const noexcept { return _consecutiveParent != Globals::kInvalidId; }
  inline uint32_t consecutiveParent() const noexcept { return _consecutiveParent; }
  inline uint32_t consecutiveData() const noexcept { return consecutiveDataFromFlags(_flags); }

  //! Returns TiedReg flags.
  inline RATiedFlags flags() const noexcept { return _flags; }
  //! Checks if the given `flag` is set.
  inline bool hasFlag(RATiedFlags flag) const noexcept { return Support::test(_flags, flag); }
  //! Adds tied register flags.
  inline void addFlags(RATiedFlags flags) noexcept { _flags |= flags; }

  //! Tests whether the register is read (writes `true` also if it's Read/Write).
  inline bool isRead() const noexcept { return hasFlag(RATiedFlags::kRead); }
  //! Tests whether the register is written (writes `true` also if it's Read/Write).
  inline bool isWrite() const noexcept { return hasFlag(RATiedFlags::kWrite); }
  //! Tests whether the register is read only.
  inline bool isReadOnly() const noexcept { return (_flags & RATiedFlags::kRW) == RATiedFlags::kRead; }
  //! Tests whether the register is write only.
  inline bool isWriteOnly() const noexcept { return (_flags & RATiedFlags::kRW) == RATiedFlags::kWrite; }
  //! Tests whether the register is read and written.
  inline bool isReadWrite() const noexcept { return (_flags & RATiedFlags::kRW) == RATiedFlags::kRW; }

  //! Tests whether the tied register has use operand (Read/ReadWrite).
  inline bool isUse() const noexcept { return hasFlag(RATiedFlags::kUse); }
  //! Tests whether the tied register has out operand (Write).
  inline bool isOut() const noexcept { return hasFlag(RATiedFlags::kOut); }

  //! Tests whether the tied register has \ref RATiedFlags::kLeadConsecutive flag set.
  inline bool isLeadConsecutive() const noexcept { return hasFlag(RATiedFlags::kLeadConsecutive); }
  //! Tests whether the tied register has \ref RATiedFlags::kUseConsecutive flag set.
  inline bool isUseConsecutive() const noexcept { return hasFlag(RATiedFlags::kUseConsecutive); }
  //! Tests whether the tied register has \ref RATiedFlags::kOutConsecutive flag set.
  inline bool isOutConsecutive() const noexcept { return hasFlag(RATiedFlags::kOutConsecutive); }

  //! Tests whether the tied register has any consecutive flag.
  inline bool hasAnyConsecutiveFlag() const noexcept { return hasFlag(RATiedFlags::kLeadConsecutive | RATiedFlags::kUseConsecutive | RATiedFlags::kOutConsecutive); }

  //! Tests whether the USE slot can be patched to memory operand.
  inline bool hasUseRM() const noexcept { return hasFlag(RATiedFlags::kUseRM); }
  //! Tests whether the OUT slot can be patched to memory operand.
  inline bool hasOutRM() const noexcept { return hasFlag(RATiedFlags::kOutRM); }

  inline uint32_t rmSize() const noexcept { return _rmSize; }

  inline void makeReadOnly() noexcept {
    _flags = (_flags & ~(RATiedFlags::kOut | RATiedFlags::kWrite)) | RATiedFlags::kUse;
    _useRewriteMask |= _outRewriteMask;
    _outRewriteMask = 0;
  }

  inline void makeWriteOnly() noexcept {
    _flags = (_flags & ~(RATiedFlags::kUse | RATiedFlags::kRead)) | RATiedFlags::kOut;
    _outRewriteMask |= _useRewriteMask;
    _useRewriteMask = 0;
  }

  //! Tests whether the register would duplicate.
  inline bool isDuplicate() const noexcept { return hasFlag(RATiedFlags::kDuplicate); }

  //! Tests whether the register (and the instruction it's part of) appears last in the basic block.
  inline bool isLast() const noexcept { return hasFlag(RATiedFlags::kLast); }
  //! Tests whether the register should be killed after USEd and/or OUTed.
  inline bool isKill() const noexcept { return hasFlag(RATiedFlags::kKill); }

  //! Tests whether the register is OUT or KILL (used internally by local register allocator).
  inline bool isOutOrKill() const noexcept { return hasFlag(RATiedFlags::kOut | RATiedFlags::kKill); }

  //! Returns a register mask that describes allocable USE registers (Read/ReadWrite access).
  inline RegMask useRegMask() const noexcept { return _useRegMask; }
  //! Returns a register mask that describes allocable OUT registers (WriteOnly access).
  inline RegMask outRegMask() const noexcept { return _outRegMask; }

  inline uint32_t refCount() const noexcept { return _refCount; }
  inline void addRefCount(uint32_t n = 1) noexcept { _refCount = uint8_t(_refCount + n); }

  //! Tests whether the register must be allocated to a fixed physical register before it's used.
  inline bool hasUseId() const noexcept { return _useId != BaseReg::kIdBad; }
  //! Tests whether the register must be allocated to a fixed physical register before it's written.
  inline bool hasOutId() const noexcept { return _outId != BaseReg::kIdBad; }

  //! Returns a physical register id used for 'use' operation.
  inline uint32_t useId() const noexcept { return _useId; }
  //! Returns a physical register id used for 'out' operation.
  inline uint32_t outId() const noexcept { return _outId; }

  inline uint32_t useRewriteMask() const noexcept { return _useRewriteMask; }
  inline uint32_t outRewriteMask() const noexcept { return _outRewriteMask; }

  //! Sets a physical register used for 'use' operation.
  inline void setUseId(uint32_t index) noexcept { _useId = uint8_t(index); }
  //! Sets a physical register used for 'out' operation.
  inline void setOutId(uint32_t index) noexcept { _outId = uint8_t(index); }

  inline bool isUseDone() const noexcept { return hasFlag(RATiedFlags::kUseDone); }
  inline bool isOutDone() const noexcept { return hasFlag(RATiedFlags::kUseDone); }

  inline void markUseDone() noexcept { addFlags(RATiedFlags::kUseDone); }
  inline void markOutDone() noexcept { addFlags(RATiedFlags::kUseDone); }

  //! \}
};

//! Flags used by \ref RAWorkReg.
enum class RAWorkRegFlags : uint32_t {
  //! No flags.
  kNone = 0,

  //! This register has already been allocated.
  kAllocated = 0x00000001u,
  //! Has been coalesced to another WorkReg.
  kCoalesced = 0x00000002u,

  //! Set when this register is used as a LEAD consecutive register at least once.
  kLeadConsecutive = 0x00000004u,
  //! Used to mark consecutive registers during processing.
  kProcessedConsecutive = 0x00000008u,

  //! Stack slot has to be allocated.
  kStackUsed = 0x00000010u,
  //! Stack allocation is preferred.
  kStackPreferred = 0x00000020u,
  //! Marked for stack argument reassignment.
  kStackArgToStack = 0x00000040u
};
ASMJIT_DEFINE_ENUM_FLAGS(RAWorkRegFlags)

//! Work register provides additional data of \ref VirtReg that is used by register allocator.
//!
//! In general when a virtual register is found by register allocator it maps it to \ref RAWorkReg
//! and then only works with it. The reason for such mapping is that users can create many virtual
//! registers, which are not used inside a register allocation scope (which is currently always a
//! function). So register allocator basically scans the function for virtual registers and maps
//! them into WorkRegs, which receive a temporary ID (workId), which starts from zero. This WorkId
//! is then used in bit-arrays and other mappings.
class RAWorkReg {
public:
  ASMJIT_NONCOPYABLE(RAWorkReg)

  //! \name Constants
  //! \{

  enum : uint32_t {
    kIdNone = 0xFFFFFFFFu
  };

  enum : uint32_t {
    kNoArgIndex = 0xFFu
  };

  //! \}

  //! \name Members
  //! \{

  //! RAPass specific ID used during analysis and allocation.
  uint32_t _workId = 0;
  //! Copy of ID used by \ref VirtReg.
  uint32_t _virtId = 0;

  //! Permanent association with \ref VirtReg.
  VirtReg* _virtReg = nullptr;
  //! Temporary association with \ref RATiedReg.
  RATiedReg* _tiedReg = nullptr;
  //! Stack slot associated with the register.
  RAStackSlot* _stackSlot = nullptr;

  //! Copy of a signature used by \ref VirtReg.
  OperandSignature _signature {};
  //! RAPass specific flags used during analysis and allocation.
  RAWorkRegFlags _flags = RAWorkRegFlags::kNone;

  //! Constains all USE ids collected from all instructions.
  //!
  //! If this mask is non-zero and not a power of two, it means that the register is used multiple times in
  //! instructions where it requires to have a different use ID. This means that in general it's not possible
  //! to keep this register in a single home.
  RegMask _useIdMask = 0;
  //! Preferred mask of registers (if non-zero) to allocate this register to.
  //!
  //! If this mask is zero it means that either there is no intersection of preferred registers collected from all
  //! TiedRegs or there is no preference at all (the register can be allocated to any register all the time).
  RegMask _preferredMask = 0xFFFFFFFFu;
  //! Consecutive mask, which was collected from all instructions where this register was used as a lead consecutive
  //! register.
  RegMask _consecutiveMask = 0xFFFFFFFFu;
  //! IDs of all physical registers that are clobbered during the lifetime of this WorkReg.
  //!
  //! This mask should be updated by `RAPass::buildLiveness()`, because it's global and should
  //! be updated after unreachable code has been removed.
  RegMask _clobberSurvivalMask = 0;
  //! IDs of all physical registers this WorkReg has been allocated to.
  RegMask _allocatedMask = 0;

  //! A byte-mask where each bit represents one valid byte of the register.
  uint64_t _regByteMask = 0;

  //! Argument index (or `kNoArgIndex` if none).
  uint8_t _argIndex = kNoArgIndex;
  //! Argument value index in the pack (0 by default).
  uint8_t _argValueIndex = 0;
  //! Global home register ID (if any, assigned by RA).
  uint8_t _homeRegId = BaseReg::kIdBad;
  //! Global hint register ID (provided by RA or user).
  uint8_t _hintRegId = BaseReg::kIdBad;

  //! Live spans of the `VirtReg`.
  LiveRegSpans _liveSpans {};
  //! Live statistics.
  RALiveStats _liveStats {};

  //! All nodes that read/write this VirtReg/WorkReg.
  ZoneVector<BaseNode*> _refs {};
  //! All nodes that write to this VirtReg/WorkReg.
  ZoneVector<BaseNode*> _writes {};

  //! Contains work IDs of all immediate consecutive registers of this register.
  //!
  //! \note This bit array only contains immediate consecutives. This means that if this is a register that is
  //! followed by 3 more registers, then it would still have only a single immediate. The rest registers would
  //! have immediate consecutive registers as well, except the last one.
  ZoneBitVector _immediateConsecutives {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline RAWorkReg(VirtReg* vReg, uint32_t workId) noexcept
    : _workId(workId),
      _virtId(vReg->id()),
      _virtReg(vReg),
      _signature(vReg->signature()) {}

  //! \}

  //! \name Accessors
  //! \{

  inline uint32_t workId() const noexcept { return _workId; }
  inline uint32_t virtId() const noexcept { return _virtId; }

  inline const char* name() const noexcept { return _virtReg->name(); }
  inline uint32_t nameSize() const noexcept { return _virtReg->nameSize(); }

  inline TypeId typeId() const noexcept { return _virtReg->typeId(); }

  inline RAWorkRegFlags flags() const noexcept { return _flags; }
  inline bool hasFlag(RAWorkRegFlags flag) const noexcept { return Support::test(_flags, flag); }
  inline void addFlags(RAWorkRegFlags flags) noexcept { _flags |= flags; }

  inline bool isAllocated() const noexcept { return hasFlag(RAWorkRegFlags::kAllocated); }
  inline void markAllocated() noexcept { addFlags(RAWorkRegFlags::kAllocated); }

  inline bool isLeadConsecutive() const noexcept { return hasFlag(RAWorkRegFlags::kLeadConsecutive); }
  inline void markLeadConsecutive() noexcept { addFlags(RAWorkRegFlags::kLeadConsecutive); }

  inline bool isProcessedConsecutive() const noexcept { return hasFlag(RAWorkRegFlags::kProcessedConsecutive); }
  inline void markProcessedConsecutive() noexcept { addFlags(RAWorkRegFlags::kProcessedConsecutive); }

  inline bool isStackUsed() const noexcept { return hasFlag(RAWorkRegFlags::kStackUsed); }
  inline void markStackUsed() noexcept { addFlags(RAWorkRegFlags::kStackUsed); }

  inline bool isStackPreferred() const noexcept { return hasFlag(RAWorkRegFlags::kStackPreferred); }
  inline void markStackPreferred() noexcept { addFlags(RAWorkRegFlags::kStackPreferred); }

  //! Tests whether this RAWorkReg has been coalesced with another one (cannot be used anymore).
  inline bool isCoalesced() const noexcept { return hasFlag(RAWorkRegFlags::kCoalesced); }

  inline OperandSignature signature() const noexcept { return _signature; }
  inline RegType type() const noexcept { return _signature.regType(); }
  inline RegGroup group() const noexcept { return _signature.regGroup(); }

  inline VirtReg* virtReg() const noexcept { return _virtReg; }

  inline bool hasTiedReg() const noexcept { return _tiedReg != nullptr; }
  inline RATiedReg* tiedReg() const noexcept { return _tiedReg; }
  inline void setTiedReg(RATiedReg* tiedReg) noexcept { _tiedReg = tiedReg; }
  inline void resetTiedReg() noexcept { _tiedReg = nullptr; }

  inline bool hasStackSlot() const noexcept { return _stackSlot != nullptr; }
  inline RAStackSlot* stackSlot() const noexcept { return _stackSlot; }

  inline LiveRegSpans& liveSpans() noexcept { return _liveSpans; }
  inline const LiveRegSpans& liveSpans() const noexcept { return _liveSpans; }

  inline RALiveStats& liveStats() noexcept { return _liveStats; }
  inline const RALiveStats& liveStats() const noexcept { return _liveStats; }

  inline bool hasArgIndex() const noexcept { return _argIndex != kNoArgIndex; }
  inline uint32_t argIndex() const noexcept { return _argIndex; }
  inline uint32_t argValueIndex() const noexcept { return _argValueIndex; }

  inline void setArgIndex(uint32_t argIndex, uint32_t valueIndex) noexcept {
    _argIndex = uint8_t(argIndex);
    _argValueIndex = uint8_t(valueIndex);
  }

  inline bool hasHomeRegId() const noexcept { return _homeRegId != BaseReg::kIdBad; }
  inline uint32_t homeRegId() const noexcept { return _homeRegId; }
  inline void setHomeRegId(uint32_t physId) noexcept { _homeRegId = uint8_t(physId); }

  inline bool hasHintRegId() const noexcept { return _hintRegId != BaseReg::kIdBad; }
  inline uint32_t hintRegId() const noexcept { return _hintRegId; }
  inline void setHintRegId(uint32_t physId) noexcept { _hintRegId = uint8_t(physId); }

  inline RegMask useIdMask() const noexcept { return _useIdMask; }
  inline bool hasUseIdMask() const noexcept { return _useIdMask != 0u; }
  inline bool hasMultipleUseIds() const noexcept { return _useIdMask != 0u && !Support::isPowerOf2(_useIdMask); }
  inline void addUseIdMask(RegMask mask) noexcept { _useIdMask |= mask; }

  inline RegMask preferredMask() const noexcept { return _preferredMask; }
  inline bool hasPrereffedMask() const noexcept { return _preferredMask != 0xFFFFFFFFu; }
  inline void restrictPreferredMask(RegMask mask) noexcept { _preferredMask &= mask; }

  inline RegMask consecutiveMask() const noexcept { return _consecutiveMask; }
  inline bool hasConsecutiveMask() const noexcept { return _consecutiveMask != 0xFFFFFFFFu; }
  inline void restrictConsecutiveMask(RegMask mask) noexcept { _consecutiveMask &= mask; }

  inline RegMask clobberSurvivalMask() const noexcept { return _clobberSurvivalMask; }
  inline void addClobberSurvivalMask(RegMask mask) noexcept { _clobberSurvivalMask |= mask; }

  inline RegMask allocatedMask() const noexcept { return _allocatedMask; }
  inline void addAllocatedMask(RegMask mask) noexcept { _allocatedMask |= mask; }

  inline uint64_t regByteMask() const noexcept { return _regByteMask; }
  inline void setRegByteMask(uint64_t mask) noexcept { _regByteMask = mask; }

  inline bool hasImmediateConsecutives() const noexcept { return !_immediateConsecutives.empty(); }
  inline const ZoneBitVector& immediateConsecutives() const noexcept { return _immediateConsecutives; }

  inline Error addImmediateConsecutive(ZoneAllocator* allocator, uint32_t workId) noexcept {
    if (_immediateConsecutives.size() <= workId)
      ASMJIT_PROPAGATE(_immediateConsecutives.resize(allocator, workId + 1));

    _immediateConsecutives.setBit(workId, true);
    return kErrorOk;
  }

  //! \}
};

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_RADEFS_P_H_INCLUDED
