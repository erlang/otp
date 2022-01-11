// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ARCHCOMMONS_H_INCLUDED
#define ASMJIT_CORE_ARCHCOMMONS_H_INCLUDED

// This file provides architecture-specific classes that are required in the core library. For example Imm operand
// allows to be created from arm::Shift in a const-expr way, so the arm::Shift must be provided. So this header file
// provides everything architecture-specific that is used by the Core API.

#include "../core/globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

//! Shift operation predicate (ARM) describes either SHIFT or EXTEND operation.
//!
//! \note The constants are AsmJit specific. The first 5 values describe real constants on ARM32 and AArch64 hardware,
//! however, the addition constants that describe extend modes are specific to AsmJit and would be translated to the
//! AArch64 specific constants by the assembler.
enum class ShiftOp {
  //! Shift left logical operation (default).
  //!
  //! Available to all ARM architectures.
  kLSL = 0x00u,

  //! Shift right logical operation.
  //!
  //! Available to all ARM architectures.
  kLSR = 0x01u,

  //! Shift right arithmetic operation.
  //!
  //! Available to all ARM architectures.
  kASR = 0x02u,

  //! Rotate right operation.
  //!
  //! \note Not available in AArch64 mode.
  kROR = 0x03u,

  //! Rotate right with carry operation (encoded as `ShiftOp::kROR` with zero).
  //!
  //! \note Not available in AArch64 mode.
  kRRX = 0x04u,

  //! Shift left by filling low order bits with ones.
  kMSL = 0x05u,

  //! UXTN extend register operation (AArch64 only).
  kUXTB = 0x06u,
  //! UXTH extend register operation (AArch64 only).
  kUXTH = 0x07u,
  //! UXTW extend register operation (AArch64 only).
  kUXTW = 0x08u,
  //! UXTX extend register operation (AArch64 only).
  kUXTX = 0x09u,

  //! SXTB extend register operation (AArch64 only).
  kSXTB = 0x0Au,
  //! SXTH extend register operation (AArch64 only).
  kSXTH = 0x0Bu,
  //! SXTW extend register operation (AArch64 only).
  kSXTW = 0x0Cu,
  //! SXTX extend register operation (AArch64 only).
  kSXTX = 0x0Du

  // NOTE: 0xE and 0xF are used by memory operand to specify POST|PRE offset mode.
};

//! Represents ARM immediate shift operation type and value.
class Shift {
public:
  //! Shift operation.
  ShiftOp _op;
  //! Shift Value.
  uint32_t _value;

  //! Default constructed Shift is not initialized.
  inline Shift() noexcept = default;

  //! Copy constructor (default)
  constexpr Shift(const Shift& other) noexcept = default;

  //! Constructs Shift from operation `op` and shift `value`.
  constexpr Shift(ShiftOp op, uint32_t value) noexcept
    : _op(op),
      _value(value) {}

  //! Returns the shift operation.
  constexpr ShiftOp op() const noexcept { return _op; }
  //! Sets shift operation to `op`.
  inline void setOp(ShiftOp op) noexcept { _op = op; }

  //! Returns the shift smount.
  constexpr uint32_t value() const noexcept { return _value; }
  //! Sets shift amount to `value`.
  inline void setValue(uint32_t value) noexcept { _value = value; }
};

//! Constructs a `LSL #value` shift (logical shift left).
static constexpr Shift lsl(uint32_t value) noexcept { return Shift(ShiftOp::kLSL, value); }
//! Constructs a `LSR #value` shift (logical shift right).
static constexpr Shift lsr(uint32_t value) noexcept { return Shift(ShiftOp::kLSR, value); }
//! Constructs a `ASR #value` shift (arithmetic shift right).
static constexpr Shift asr(uint32_t value) noexcept { return Shift(ShiftOp::kASR, value); }
//! Constructs a `ROR #value` shift (rotate right).
static constexpr Shift ror(uint32_t value) noexcept { return Shift(ShiftOp::kROR, value); }
//! Constructs a `RRX` shift (rotate with carry by 1).
static constexpr Shift rrx() noexcept { return Shift(ShiftOp::kRRX, 0); }
//! Constructs a `MSL #value` shift (logical shift left filling ones).
static constexpr Shift msl(uint32_t value) noexcept { return Shift(ShiftOp::kMSL, value); }

//! Constructs a `UXTB #value` extend and shift (unsigned byte extend).
static constexpr Shift uxtb(uint32_t value) noexcept { return Shift(ShiftOp::kUXTB, value); }
//! Constructs a `UXTH #value` extend and shift (unsigned hword extend).
static constexpr Shift uxth(uint32_t value) noexcept { return Shift(ShiftOp::kUXTH, value); }
//! Constructs a `UXTW #value` extend and shift (unsigned word extend).
static constexpr Shift uxtw(uint32_t value) noexcept { return Shift(ShiftOp::kUXTW, value); }
//! Constructs a `UXTX #value` extend and shift (unsigned dword extend).
static constexpr Shift uxtx(uint32_t value) noexcept { return Shift(ShiftOp::kUXTX, value); }

//! Constructs a `SXTB #value` extend and shift (signed byte extend).
static constexpr Shift sxtb(uint32_t value) noexcept { return Shift(ShiftOp::kSXTB, value); }
//! Constructs a `SXTH #value` extend and shift (signed hword extend).
static constexpr Shift sxth(uint32_t value) noexcept { return Shift(ShiftOp::kSXTH, value); }
//! Constructs a `SXTW #value` extend and shift (signed word extend).
static constexpr Shift sxtw(uint32_t value) noexcept { return Shift(ShiftOp::kSXTW, value); }
//! Constructs a `SXTX #value` extend and shift (signed dword extend).
static constexpr Shift sxtx(uint32_t value) noexcept { return Shift(ShiftOp::kSXTX, value); }

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_CORE_ARCHCOMMONS_H_INCLUDED
