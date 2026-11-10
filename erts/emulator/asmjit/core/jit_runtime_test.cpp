// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST) && !defined(ASMJIT_NO_JIT)

#include <asmjit/axl/mem_ops.h>
#include <asmjit/core/jit_runtime.h>

#if ASMJIT_TARGET_ARCH_X86 != 0 && !defined(ASMJIT_NO_X86)
  #include <asmjit/x86/x86_assembler.h>
#endif

#if ASMJIT_TARGET_ARCH_ARM == 64 && !defined(ASMJIT_NO_AARCH64)
  #include <asmjit/arm/a64_assembler.h>
#endif

ASMJIT_BEGIN_NAMESPACE

// JitRuntime::add() must fail if the code references a label that was never bound.
#if ASMJIT_TARGET_ARCH_X86 != 0 && !defined(ASMJIT_NO_X86)
TEST_CASE(core_jit_runtime_unbound_label) {
  JitRuntime rt;
  CodeHolder code;
  code.init(rt.environment());

  x86::Assembler a(&code);
  Label unbound = a.new_label();
  a.jmp(unbound);

  void* fn {};
  EXPECT_EQ(rt.add(&fn, &code), Error::kUnresolvedFixups);
}
#endif

// With `near_address`, `call(imm(fn))` to a function of the same module must be relocated to a direct call.
#if ASMJIT_TARGET_ARCH_X86 == 64 && !defined(ASMJIT_NO_X86)
static int ASMJIT_CDECL jit_runtime_test_callee() noexcept { return 42; }

TEST_CASE(core_jit_runtime_near_calls) {
  using Func = int (ASMJIT_CDECL*)();

  JitAllocator::CreateParams params {};
  params.options = JitAllocatorOptions::kStrictAddressRange;
  params.near_address = uintptr_t(jit_runtime_test_callee);

  JitRuntime rt(&params);
  CodeHolder code;
  code.init(rt.environment());

  x86::Assembler a(&code);
  a.sub(x86::rsp, 40);
  size_t call_offset = a.offset();
  a.call(imm((void*)jit_runtime_test_callee));
  a.add(x86::rsp, 40);
  a.ret();

  void* fn {};
  EXPECT_OK(rt.add(&fn, &code));

  // The call must be relocated to `call rel32` (with the REX prefix that would allow patching it to `call [rip+x]`).
  const uint8_t* call = static_cast<const uint8_t*>(fn) + call_offset;
  EXPECT_EQ(call[0], 0x40u);
  EXPECT_EQ(call[1], 0xE8u);

  int32_t rel32 = axl::loadu_i32_le(call + 2);
  EXPECT_EQ(uintptr_t(call + 6) + uintptr_t(intptr_t(rel32)), uintptr_t(jit_runtime_test_callee));
  EXPECT_EQ(reinterpret_cast<Func>(fn)(), 42);
  EXPECT_OK(rt.release(fn));
}
#endif

#if ASMJIT_TARGET_ARCH_ARM == 64 && !defined(ASMJIT_NO_AARCH64)
TEST_CASE(core_jit_runtime_unbound_label) {
  JitRuntime rt;
  CodeHolder code;
  code.init(rt.environment());

  a64::Assembler a(&code);
  Label unbound = a.new_label();
  a.b(unbound);

  void* fn {};
  EXPECT_EQ(rt.add(&fn, &code), Error::kUnresolvedFixups);
}
#endif

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
