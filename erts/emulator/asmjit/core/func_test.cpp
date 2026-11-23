// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/core/func.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(core_func_signature) {
  FuncSignature signature;
  signature.set_ret_t<int8_t>();
  signature.add_arg_t<int16_t>();
  signature.add_arg(TypeId::kInt32);

  EXPECT_EQ(signature, FuncSignature::build<int8_t, int16_t, int32_t>());
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
