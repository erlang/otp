// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/string_index.h>

#include <string.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_string_index) {
  static constexpr char strings[] =
    "hello\0"
    "test\0"
    "something_else\0"
    "something_even_else\0"
    "<out_of_bounds>\0";
  static constexpr auto strings_index = axl::make_string_index<5u>(strings);

  EXPECT_EQ(strcmp(strings + strings_index[0], "hello"), 0);
  EXPECT_EQ(strcmp(strings + strings_index[1], "test"), 0);
  EXPECT_EQ(strcmp(strings + strings_index[2], "something_else"), 0);
  EXPECT_EQ(strcmp(strings + strings_index[3], "something_even_else"), 0);
  EXPECT_EQ(strcmp(strings + strings_index[4], "<out_of_bounds>"), 0);
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
