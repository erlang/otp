// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/string_utils.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(axl_string_utils) {
  EXPECT_TRUE(axl::is_ascii_space(' '));
  EXPECT_TRUE(axl::is_ascii_space('\r'));
  EXPECT_TRUE(axl::is_ascii_space('\n'));
  EXPECT_TRUE(axl::is_ascii_space('\t'));
  EXPECT_FALSE(axl::is_ascii_space('\0'));
  EXPECT_FALSE(axl::is_ascii_space('a'));
  EXPECT_FALSE(axl::is_ascii_space('A'));
  EXPECT_FALSE(axl::is_ascii_space('\xFF'));

  EXPECT_TRUE(axl::is_ascii_alpha('a'));
  EXPECT_TRUE(axl::is_ascii_alpha('z'));
  EXPECT_TRUE(axl::is_ascii_alpha('A'));
  EXPECT_TRUE(axl::is_ascii_alpha('Z'));
  EXPECT_FALSE(axl::is_ascii_alpha('\0'));
  EXPECT_FALSE(axl::is_ascii_alpha('0'));
  EXPECT_FALSE(axl::is_ascii_alpha('9'));
  EXPECT_FALSE(axl::is_ascii_alpha('~'));

  EXPECT_TRUE(axl::is_ascii_digit('0'));
  EXPECT_TRUE(axl::is_ascii_digit('9'));
  EXPECT_FALSE(axl::is_ascii_digit('a'));
  EXPECT_FALSE(axl::is_ascii_digit('z'));
  EXPECT_FALSE(axl::is_ascii_digit('A'));
  EXPECT_FALSE(axl::is_ascii_digit('Z'));
  EXPECT_FALSE(axl::is_ascii_digit('\0'));
  EXPECT_FALSE(axl::is_ascii_digit('~'));

  EXPECT_TRUE(axl::is_ascii_alnum('0'));
  EXPECT_TRUE(axl::is_ascii_alnum('9'));
  EXPECT_TRUE(axl::is_ascii_alnum('a'));
  EXPECT_TRUE(axl::is_ascii_alnum('z'));
  EXPECT_TRUE(axl::is_ascii_alnum('A'));
  EXPECT_TRUE(axl::is_ascii_alnum('Z'));
  EXPECT_FALSE(axl::is_ascii_alnum('_'));
  EXPECT_FALSE(axl::is_ascii_alnum('~'));

  EXPECT_EQ(axl::ascii_to_lower('a'), 'a');
  EXPECT_EQ(axl::ascii_to_lower('A'), 'a');
  EXPECT_EQ(axl::ascii_to_lower('z'), 'z');
  EXPECT_EQ(axl::ascii_to_lower('Z'), 'z');
  EXPECT_EQ(axl::ascii_to_lower('0'), '0');
  EXPECT_EQ(axl::ascii_to_lower('~'), '~');
  EXPECT_EQ(axl::ascii_to_lower('_'), '_');

  EXPECT_EQ(axl::ascii_to_upper('a'), 'A');
  EXPECT_EQ(axl::ascii_to_upper('A'), 'A');
  EXPECT_EQ(axl::ascii_to_upper('z'), 'Z');
  EXPECT_EQ(axl::ascii_to_upper('Z'), 'Z');
  EXPECT_EQ(axl::ascii_to_upper('0'), '0');
  EXPECT_EQ(axl::ascii_to_upper('~'), '~');
  EXPECT_EQ(axl::ascii_to_upper('_'), '_');

  static const char some_string[] = "abcdefghijklmnopqrstuvwxyz";
  EXPECT_EQ(axl::str_nlen(some_string, 16u), 16u);
  EXPECT_EQ(axl::str_nlen(some_string, 0xFFFFu), 26u);

  uint32_t hash = 0;
  for (size_t i = 0; i < strlen(some_string); i++) {
    hash = axl::hash_char(hash, some_string[i]);
  }
  EXPECT_EQ(hash, axl::hash_string(some_string, strlen(some_string)));
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
