// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/commons.h>
#include <asmjit/core/string.h>

ASMJIT_BEGIN_NAMESPACE

namespace StringTests {

static constexpr size_t kCapacityAlignment = 32u;

// String initially allocated on heap with spare capacity for short test strings.
struct StringOnHeap : public String {
  StringOnHeap() noexcept {
    EXPECT_OK(assign_chars('\0', 64));
    EXPECT_OK(clear());
  }
};

// Variation helpers - used as typed by typed `TEST_CASE_T` tests.
TEST_DEFINE_TYPES(string_storage_types, String, StringOnHeap, StringTmp<64>);

static void verify_span_data(Span<const char> s, const char* data, size_t size = SIZE_MAX) noexcept {
  if (size == SIZE_MAX) {
    size = strlen(data);
  }

  for (size_t i = 0; i < size; i++) {
    EXPECT_EQ(s.data()[i], data[i])
      .message("Invalid string detected at %zu [char=0x%02X expected=0x%02X]",
        i, static_cast<uint8_t>(s.data()[i]), static_cast<uint8_t>(data[i]));
  }
}

static void verify_span_chars(Span<const char> s, char c, size_t size) noexcept {
  EXPECT_EQ(s.size(), size);

  for (size_t i = 0; i < size; i++) {
    EXPECT_EQ(s.data()[i], c)
      .message("Invalid string detected at %zu [char=0x%02X expected=0x%02X]",
        i, static_cast<uint8_t>(s.data()[i]), static_cast<uint8_t>(c));

  }
}

static void verify_string_data(const String& s, const char* data, size_t size = SIZE_MAX) noexcept {
  if (size == SIZE_MAX) {
    size = strlen(data);
  }

  EXPECT_EQ(s.size(), size);
  EXPECT_GE(s.capacity(), size);
  EXPECT_EQ(s.is_empty(), size == 0);

  EXPECT_EQ(s.data()[size], '\0')
    .message("Invalid string detected - no null terminator at the end [%zu]=0x%02X",
      size, static_cast<uint8_t>(s.data()[size]));

  verify_span_data(s.span(), data, size);
}

static void verify_string_chars(const String& s, char c, size_t size) noexcept {
  EXPECT_EQ(s.size(), size);
  EXPECT_GE(s.capacity(), size);
  EXPECT_EQ(s.is_empty(), size == 0);

  EXPECT_EQ(s.data()[size], '\0')
    .message("Invalid string detected - no null terminator at the end [%zu]=0x%02X",
      size, static_cast<uint8_t>(s.data()[size]));

  verify_span_chars(s.span(), c, size);
}

static void make_abc_pattern(char* data, size_t size) noexcept {
  for (size_t i = 0; i < size; i++) {
    data[i] = static_cast<char>('a' + (i % 26u));
  }
  data[size] = '\0';
}

TEST_CASE(core_string_empty) {
  String s;
  verify_string_data(s, "");
  EXPECT_EQ(s.capacity(), String::kSSOCapacity);
  EXPECT_FALSE(s.is_large_or_external());
  EXPECT_FALSE(s.is_external());

  StringOnHeap s_heap;
  verify_string_data(s_heap, "");
  EXPECT_GT(s_heap.capacity(), String::kSSOCapacity);
  EXPECT_TRUE(s_heap.is_large_or_external());
  EXPECT_FALSE(s_heap.is_external());

  StringTmp<64> s_tmp;
  verify_string_data(s_tmp, "");
  EXPECT_GE(s_tmp.capacity(), 64u);
  EXPECT_TRUE(s_tmp.is_large_or_external());
  EXPECT_TRUE(s_tmp.is_external());
  EXPECT_EQ(s_tmp.data(), s_tmp._embedded_data);
}

TEST_CASE_T(core_string_basics, string_storage_types) {
  T s;

  EXPECT_OK(s.assign('a'));
  EXPECT_EQ(s.size(), 1u);
  EXPECT_EQ(s.data()[0], 'a');
  EXPECT_EQ(s.data()[1], '\0');
  EXPECT_TRUE(s.equals("a"));
  EXPECT_TRUE(s.equals("a", 1));

  EXPECT_OK(s.assign_chars('b', 4));
  EXPECT_EQ(s.size(), 4u);
  EXPECT_EQ(s.data()[0], 'b');
  EXPECT_EQ(s.data()[1], 'b');
  EXPECT_EQ(s.data()[2], 'b');
  EXPECT_EQ(s.data()[3], 'b');
  EXPECT_EQ(s.data()[4], '\0');
  EXPECT_TRUE(s.equals("bbbb"));
  EXPECT_TRUE(s.equals("bbbb", 4));

  EXPECT_OK(s.assign("abc"));
  EXPECT_EQ(s.size(), 3u);
  EXPECT_EQ(s.data()[0], 'a');
  EXPECT_EQ(s.data()[1], 'b');
  EXPECT_EQ(s.data()[2], 'c');
  EXPECT_EQ(s.data()[3], '\0');
  EXPECT_TRUE(s.equals("abc"));
  EXPECT_TRUE(s.equals("abc", 3));

  const char* large = "Large string that will not fit into SSO buffer";
  EXPECT_OK(s.assign(large));
  EXPECT_TRUE(s.is_large_or_external());
  EXPECT_EQ(s.size(), strlen(large));
  EXPECT_GT(s.capacity(), String::kSSOCapacity);
  EXPECT_TRUE(s.equals(large));
  EXPECT_TRUE(s.equals(large, strlen(large)));

  const char* additional = " (additional content)";
  EXPECT_TRUE(s.is_large_or_external());
  EXPECT_OK(s.append(additional));
  EXPECT_EQ(s.size(), strlen(large) + strlen(additional));
  verify_string_data(s, "Large string that will not fit into SSO buffer (additional content)");

  EXPECT_OK(s.clear());
  EXPECT_EQ(s.size(), 0u);
  EXPECT_TRUE(s.is_empty());
  EXPECT_EQ(s.data()[0], '\0');
  EXPECT_TRUE(s.is_large_or_external()); // Clear should never release the memory.

  EXPECT_OK(s.append_int(1234u));
  EXPECT_TRUE(s.equals("1234"));

  EXPECT_OK(s.assign_int(0xFFFFu, 16, 0, StringFormatFlags::kAlternate));
  EXPECT_TRUE(s.equals("0xFFFF"));

  EXPECT_OK(s.assign_int(0xFFFFu, 16, 0));
  EXPECT_TRUE(s.equals("FFFF"));

  EXPECT_OK(s.append_chars('F', 1000));
  EXPECT_FALSE(s.is_external());
  verify_string_chars(s, 'F', 1004);
}

TEST_CASE_T(core_string_assign_append, string_storage_types) {
  char pattern[257];
  make_abc_pattern(pattern, 256);

  for (size_t size : {size_t(1), size_t(String::kSSOCapacity - 1), size_t(String::kSSOCapacity),
                     size_t(String::kSSOCapacity + 1), size_t(256)}) {
    T s;
    TEST_LOG("Assigning and appending %zu characters", size);
    EXPECT_OK(s.assign(pattern, size));
    verify_string_data(s, pattern, size);

    String other;
    EXPECT_OK(other.assign(pattern, size));
    EXPECT_OK(s.assign(other));
    verify_string_data(s, pattern, size);

    EXPECT_OK(s.assign(Span<const char>(pattern, size)));
    verify_string_data(s, pattern, size);

    EXPECT_OK(s.clear());
    EXPECT_OK(s.append(Span<const char>(pattern, size)));
    verify_string_data(s, pattern, size);

    EXPECT_OK(s.clear());
    EXPECT_OK(s.append(other));
    verify_string_data(s, pattern, size);

    char expected[514];
    memcpy(expected, pattern, size);
    memcpy(expected + size, pattern, size);
    EXPECT_OK(s.append(pattern, size));
    verify_string_data(s, expected, size * 2);
  }

  T s;
  EXPECT_OK(s.assign("abc", SIZE_MAX));
  EXPECT_OK(s.append("def", SIZE_MAX));
  EXPECT_OK(s.append('g'));
  EXPECT_OK(s.append_chars('h', 3));
  verify_string_data(s, "abcdefghhh");

  EXPECT_OK(s.assign(nullptr));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("unused", 0));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));

  String empty;
  EXPECT_OK(s.assign(empty));
  verify_string_data(s, "");
}

TEST_CASE(core_string_sso_boundary) {
  char pattern[String::kSSOCapacity + 2];
  make_abc_pattern(pattern, String::kSSOCapacity + 1);

  for (size_t size : {size_t(String::kSSOCapacity - 1),
                      size_t(String::kSSOCapacity),
                      size_t(String::kSSOCapacity + 1)}) {
    String s;
    EXPECT_OK(s.assign(pattern, size));
    verify_string_data(s, pattern, size);
    EXPECT_EQ(s.is_large_or_external(), size > String::kSSOCapacity);
    EXPECT_FALSE(s.is_external());
  }

  String s;
  EXPECT_OK(s.assign(pattern, String::kSSOCapacity - 1));
  char* sso_data = s.data();

  EXPECT_OK(s.append(pattern[String::kSSOCapacity - 1]));
  verify_string_data(s, pattern, String::kSSOCapacity);
  EXPECT_EQ(s.data(), sso_data);
  EXPECT_FALSE(s.is_large_or_external());

  EXPECT_OK(s.append(pattern[String::kSSOCapacity]));
  verify_string_data(s, pattern, String::kSSOCapacity + 1);
  EXPECT_TRUE(s.is_large_or_external());

  char* owned_data = s.data();
  size_t capacity = s.capacity();

  EXPECT_OK(s.assign("short"));
  verify_string_data(s, "short");
  EXPECT_EQ(s.data(), owned_data);
  EXPECT_EQ(s.capacity(), capacity);
}

TEST_CASE(core_string_external_boundary) {
  StringTmp<64> s;
  char pattern[129];

  const size_t capacity = s.capacity();
  EXPECT_LT(capacity + 1, ASMJIT_ARRAY_SIZE(pattern));

  make_abc_pattern(pattern, capacity + 1);

  EXPECT_OK(s.assign(pattern, capacity - 1));
  EXPECT_EQ(s.data(), s._embedded_data);
  verify_string_data(s, pattern, capacity - 1);

  EXPECT_OK(s.append(pattern[capacity - 1]));
  EXPECT_TRUE(s.is_external());
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, pattern, capacity);

  EXPECT_OK(s.append(pattern[capacity]));
  EXPECT_FALSE(s.is_external());
  EXPECT_TRUE(s.is_large_or_external());
  verify_string_data(s, pattern, capacity + 1);

  // Growing an external string must leave the original buffer intact and must never free it.
  EXPECT_EQ(memcmp(s._embedded_data, pattern, capacity), 0);
  EXPECT_EQ(s._embedded_data[capacity], '\0');

  StringTmp<64> assigned;
  EXPECT_OK(assigned.assign(pattern, capacity));
  EXPECT_TRUE(assigned.is_external());
  verify_string_data(assigned, pattern, capacity);

  EXPECT_OK(assigned.assign(pattern, capacity + 1));
  EXPECT_FALSE(assigned.is_external());
  verify_string_data(assigned, pattern, capacity + 1);
}

TEST_CASE_T(core_string_assign_empty, string_storage_types) {
  T s;
  String empty;

  EXPECT_OK(s.assign("old"));
  char* data = s.data();
  size_t capacity = s.capacity();

  EXPECT_OK(s.assign(Span<const char>()));
  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign(empty));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_chars('x', 0));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_hex(nullptr, 0));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_hex(nullptr, 0, ':'));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_format("%s", ""));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_format(""));
  verify_string_data(s, "");
}

TEST_CASE_T(core_string_assign_aliased, string_storage_types) {
  T s;
  EXPECT_OK(s.assign("abcdef"));
  EXPECT_OK(s.assign(s.data() + 2, 4));
  verify_string_data(s, "cdef");

  EXPECT_OK(s.assign("abcdef"));
  EXPECT_OK(s.assign(s.data() + 2));
  verify_string_data(s, "cdef");

  EXPECT_OK(s.assign(s));
  verify_string_data(s, "cdef");

  EXPECT_OK(s.assign("abcdef"));
  EXPECT_OK(s.assign(Span<const char>(s.data() + 2, 4)));
  verify_string_data(s, "cdef");
}

TEST_CASE_T(core_string_append_empty, string_storage_types) {
  T s;
  String empty;

  EXPECT_OK(s.assign("keep"));
  char* data = s.data();
  size_t capacity = s.capacity();

  EXPECT_OK(s.append(""));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append("unused", 0));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append(nullptr));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append(Span<const char>()));

  EXPECT_OK(s.append(empty));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append_chars('x', 0));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append_hex(nullptr, 0));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append_hex(nullptr, 0, ':'));
  verify_string_data(s, "keep");

  EXPECT_OK(s.append_format("%s", ""));
  verify_string_data(s, "keep");

  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, "keep");
}

TEST_CASE_T(core_string_append_aliased, string_storage_types) {
  T s;
  EXPECT_OK(s.assign("abc"));
  char* data = s.data();

  EXPECT_OK(s.append(s));
  EXPECT_EQ(s.data(), data);
  verify_string_data(s, "abcabc");

  EXPECT_OK(s.assign("abc"));
  data = s.data();

  EXPECT_OK(s.append(Span<const char>(s.data() + 2, 2)));
  EXPECT_EQ(s.data(), data);

  const char expected[] = {'a', 'b', 'c', 'c', '\0'};
  verify_string_data(s, expected, sizeof(expected));
}

TEST_CASE_T(core_string_append_aliased_growth, string_storage_types) {
  auto test_append_aliased = [](size_t size) {
    char pattern[257];
    char expected[513];
    EXPECT_LE(size, 256u);

    make_abc_pattern(pattern, size);
    memcpy(expected, pattern, size);
    memcpy(expected + size, pattern, size);

    for (unsigned overload = 0; overload < 3; overload++) {
      T s;
      EXPECT_OK(s.assign(pattern, size));
      if (overload == 0) {
        EXPECT_OK(s.append(s));
      }
      else if (overload == 1) {
        EXPECT_OK(s.append(s.data()));
      }
      else {
        EXPECT_OK(s.append(s.span()));
      }
      verify_string_data(s, expected, size * 2);
    }
  };

  test_append_aliased(3);
  test_append_aliased(20);
  test_append_aliased(128);
  test_append_aliased(255);
}

TEST_CASE(core_string_append_aliased_external_growth) {
  StringTmp<64> s;
  char pattern[129];
  char expected[257];

  size_t size = s.capacity();
  EXPECT_LT(size, ASMJIT_ARRAY_SIZE(pattern));

  make_abc_pattern(pattern, size);
  memcpy(expected, pattern, size);
  memcpy(expected + size, pattern, size);

  EXPECT_OK(s.assign(pattern, size));
  EXPECT_OK(s.append(s));
  EXPECT_FALSE(s.is_external());
  verify_string_data(s, expected, size * 2);
}

TEST_CASE_T(core_string_clear_reset, string_storage_types) {
  T s;
  EXPECT_OK(s.assign("content"));

  char* data = s.data();
  size_t capacity = s.capacity();
  bool external = s.is_external();
  bool large_or_external = s.is_large_or_external();

  EXPECT_OK(s.clear());
  verify_string_data(s, "");

  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  EXPECT_EQ(s.is_external(), external);
  EXPECT_EQ(s.is_large_or_external(), large_or_external);
  EXPECT_OK(s.clear());
  verify_string_data(s, "");

  EXPECT_OK(s.assign("content"));
  EXPECT_OK(s.reset());
  EXPECT_EQ(s.capacity(), String::kSSOCapacity);
  EXPECT_FALSE(s.is_large_or_external());
  EXPECT_FALSE(s.is_external());
  verify_string_data(s, "");

  EXPECT_OK(s.reset());
  EXPECT_OK(s.append("reused"));
  verify_string_data(s, "reused");

  EXPECT_OK(s.assign_chars('x', 256));
  EXPECT_TRUE(s.is_large_or_external());
  EXPECT_OK(s.reset());
  EXPECT_FALSE(s.is_large_or_external());
  verify_string_data(s, "");
}

TEST_CASE_T(core_string_prepare, string_storage_types) {
  T s;

  EXPECT_OK(s.assign("old"));
  char* data = s.data();
  size_t capacity = s.capacity();
  char* p = s.assign_uninitialized(3);
  EXPECT_EQ(p, data);
  EXPECT_EQ(s.size(), 3u);
  EXPECT_EQ(s.data()[3], '\0');

  memcpy(p, "abc", 3);
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, "abc");

  p = s.append_uninitialized(3);
  EXPECT_EQ(p, s.data() + 3);
  EXPECT_EQ(s.size(), 6u);
  EXPECT_EQ(s.data()[6], '\0');
  EXPECT_EQ(memcmp(s.data(), "abc", 3), 0);

  memcpy(p, "def", 3);
  EXPECT_EQ(s.data(), data);
  verify_string_data(s, "abcdef");

  p = s.append_uninitialized(0);
  EXPECT_EQ(p, s.data() + 6);
  verify_string_data(s, "abcdef");

  p = s.assign_uninitialized(0);
  EXPECT_EQ(p, s.data());
  verify_string_data(s, "");

  const size_t size = s.capacity() + 1;
  p = s.assign_uninitialized(size);
  EXPECT_NOT_NULL(p);
  EXPECT_EQ(p, s.data());
  EXPECT_EQ(s.size(), size);
  EXPECT_EQ(s.data()[size], '\0');
  EXPECT_FALSE(s.is_external());

  memset(p, 'x', size);
  verify_string_chars(s, 'x', size);

  size_t additional = s.capacity() - s.size() + 1;
  p = s.append_uninitialized(additional);
  EXPECT_NOT_NULL(p);
  EXPECT_EQ(p, s.data() + size);

  memset(p, 'x', additional);
  verify_string_chars(s, 'x', size + additional);
}

TEST_CASE_T(core_string_append_uninitialized_at_capacity, string_storage_types) {
  T s;
  size_t capacity = s.capacity();

  EXPECT_OK(s.assign_chars('x', capacity));
  char* data = s.data();
  bool external = s.is_external();

  EXPECT_EQ(s.append_uninitialized(0), data + capacity);
  EXPECT_OK(s.append_chars('y', 0));
  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  EXPECT_EQ(s.is_external(), external);
  verify_string_chars(s, 'x', capacity);
}

TEST_CASE_T(core_string_size_overflow, string_storage_types) {
  T s;

  EXPECT_OK(s.assign("keep"));
  bool external = s.is_external();
  char* data = s.data();
  size_t capacity = s.capacity();

  for (size_t size : {SIZE_MAX, SIZE_MAX - 1, SIZE_MAX - size_t(axl::kGrowThreshold)}) {
    EXPECT_NULL(s.assign_uninitialized(size));
    verify_string_data(s, "keep");

    EXPECT_NULL(s.append_uninitialized(size));
    verify_string_data(s, "keep");
  }

  EXPECT_EQ(s.assign_chars('x', SIZE_MAX), Error::kOutOfMemory);
  verify_string_data(s, "keep");

  EXPECT_EQ(s.append_chars('x', SIZE_MAX), Error::kOutOfMemory);
  verify_string_data(s, "keep");

  EXPECT_EQ(s.append("x", SIZE_MAX - 1), Error::kOutOfMemory);
  verify_string_data(s, "keep");

  EXPECT_EQ(s.assign(Span<const char>("x", SIZE_MAX - 1)), Error::kOutOfMemory);
  verify_string_data(s, "keep");

  EXPECT_EQ(s.pad_end(SIZE_MAX), Error::kOutOfMemory);
  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  EXPECT_EQ(s.is_external(), external);
  verify_string_data(s, "keep");
}

TEST_CASE_T(core_string_pad_truncate, string_storage_types) {
  T s;

  EXPECT_OK(s.assign("abc"));
  char* data = s.data();
  size_t capacity = s.capacity();

  EXPECT_OK(s.pad_end(0));
  EXPECT_OK(s.pad_end(2));
  EXPECT_OK(s.pad_end(3));
  verify_string_data(s, "abc");

  EXPECT_OK(s.pad_end(5));
  verify_string_data(s, "abc  ");

  EXPECT_OK(s.pad_end(8, '.'));
  verify_string_data(s, "abc  ...");

  EXPECT_OK(s.truncate(100));
  EXPECT_OK(s.truncate(8));
  verify_string_data(s, "abc  ...");

  EXPECT_OK(s.truncate(3));
  verify_string_data(s, "abc");

  EXPECT_OK(s.truncate(0));
  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, "");

  EXPECT_OK(s.assign("abc"));
  verify_string_data(s, "abc", 3);

  EXPECT_OK(s.pad_end(256, 'x'));
  EXPECT_EQ(s.size(), 256u);
  verify_span_data(Span<const char>(s.data(), 3), "abc", 3);
  verify_span_chars(Span<const char>(s.data() + 3, 256 - 3), 'x', 256 - 3);

  data = s.data();
  capacity = s.capacity();
  EXPECT_OK(s.truncate(2));
  verify_string_data(s, "ab");

  EXPECT_OK(s.truncate(0));
  EXPECT_EQ(s.data(), data);
  EXPECT_EQ(s.capacity(), capacity);
  verify_string_data(s, "");
}

TEST_CASE_T(core_string_binary_equality_accessors, string_storage_types) {
  T s;

  const char binary[] = {'a', '\0', 'b', static_cast<char>(0xFF)};
  EXPECT_OK(s.assign(binary, sizeof(binary)));
  verify_string_data(s, binary, sizeof(binary));

  char doubled[sizeof(binary) * 2];
  memcpy(doubled, binary, sizeof(binary));
  memcpy(doubled + sizeof(binary), binary, sizeof(binary));

  EXPECT_OK(s.append(binary, sizeof(binary)));
  verify_string_data(s, doubled, sizeof(doubled));

  EXPECT_OK(s.assign(binary, sizeof(binary)));
  EXPECT_TRUE(s.equals(binary, sizeof(binary)));
  EXPECT_FALSE(s.equals("a"));
  EXPECT_FALSE(s.equals(""));
  EXPECT_FALSE(s.equals(Span<const char>()));
  EXPECT_FALSE(s.equals(binary, sizeof(binary) - 1));
  const char different[] = {'a', '\0', 'c', static_cast<char>(0xFF)};
  EXPECT_FALSE(s.equals(different, sizeof(different)));

  String other;
  EXPECT_OK(other.assign(binary, sizeof(binary)));
  EXPECT_TRUE(s.equals(other));
  EXPECT_TRUE(s == other);
  EXPECT_FALSE(s != other);

  EXPECT_OK(other.assign(different, sizeof(different)));
  EXPECT_FALSE(s == other);
  EXPECT_TRUE(s != other);

  const String& const_s = s;
  EXPECT_EQ(s.begin(), s.data());
  EXPECT_EQ(s.end(), s.data() + sizeof(binary));
  EXPECT_EQ(const_s.begin(), const_s.data());
  EXPECT_EQ(const_s.end(), const_s.data() + sizeof(binary));
  EXPECT_EQ(s.span().data(), s.data());
  EXPECT_EQ(s.span().size(), sizeof(binary));
  EXPECT_EQ(s.span_mut().data(), s.data());
  EXPECT_EQ(s.span_mut().size(), sizeof(binary));
  s.span_mut()[2] = 'c';
  verify_string_data(s, different, sizeof(different));

  EXPECT_OK(s.assign_chars('\0', 3));
  const char zeros[] = {0, 0, 0};
  verify_string_data(s, zeros, sizeof(zeros));
  EXPECT_FALSE(s.is_empty());
  EXPECT_FALSE(s.equals(""));

  EXPECT_OK(s.assign('\0'));
  EXPECT_OK(s.append('\0'));
  EXPECT_OK(s.append(Span<const char>(zeros, 1)));
  verify_string_data(s, zeros, sizeof(zeros));

  EXPECT_OK(s.assign("abc"));
  EXPECT_TRUE(s == "abc");
  EXPECT_FALSE(s != "abc");
  EXPECT_FALSE(s.equals("ab"));
  EXPECT_FALSE(s.equals("abcd"));
  EXPECT_FALSE(s.equals("abd"));
  EXPECT_TRUE(s != "abd");

  EXPECT_OK(s.clear());
  EXPECT_TRUE(s.equals(""));
  EXPECT_TRUE(s.equals(Span<const char>()));
  EXPECT_TRUE(s.equals("unused", 0));
  EXPECT_FALSE(s.equals("x"));
}

TEST_CASE_T(core_string_move, string_storage_types) {
  T source;

  EXPECT_OK(source.assign("content"));
  bool external = source.is_external();
  bool large_or_external = source.is_large_or_external();
  char* data = source.data();
  size_t capacity = source.capacity();

  String moved(axl::move(source));
  EXPECT_FALSE(source.is_large_or_external());
  EXPECT_EQ(moved.capacity(), capacity);
  EXPECT_EQ(moved.is_large_or_external(), large_or_external);
  EXPECT_EQ(moved.is_external(), external);
  if (large_or_external) {
    EXPECT_EQ(moved.data(), data);
  }
  verify_string_data(source, "");
  verify_string_data(moved, "content");

  EXPECT_OK(source.append("source reused"));
  verify_string_data(source, "source reused");

  String destination;
  EXPECT_OK(destination.assign_chars('x', 256));

  destination = axl::move(moved);
  EXPECT_FALSE(moved.is_large_or_external());
  EXPECT_EQ(destination.capacity(), capacity);
  EXPECT_EQ(destination.is_external(), external);
  if (large_or_external) {
    EXPECT_EQ(destination.data(), data);
  }
  verify_string_data(moved, "");
  verify_string_data(destination, "content");

  EXPECT_OK(moved.append("moved reused"));
  verify_string_data(moved, "moved reused");
}

TEST_CASE_T(core_string_swap, string_storage_types) {
  T first;

  EXPECT_OK(first.assign("first"));
  char* first_data = first.data();
  size_t first_capacity = first.capacity();
  bool first_large = first.is_large_or_external();
  bool first_external = first.is_external();

  String second;
  EXPECT_OK(second.assign_chars('b', 256));

  char* second_data = second.data();
  size_t second_capacity = second.capacity();
  first.swap(second);
  EXPECT_EQ(first.data(), second_data);
  EXPECT_EQ(first.capacity(), second_capacity);
  EXPECT_FALSE(first.is_external());
  verify_string_chars(first, 'b', 256);

  EXPECT_EQ(second.capacity(), first_capacity);
  EXPECT_EQ(second.is_large_or_external(), first_large);
  EXPECT_EQ(second.is_external(), first_external);
  if (first_large) {
    EXPECT_EQ(second.data(), first_data);
  }
  verify_string_data(second, "first");

  first.swap(second);
  verify_string_data(first, "first");
  verify_string_chars(second, 'b', 256);

  first.swap(first);
  verify_string_data(first, "first");
}

TEST_CASE(core_string_growth_threshold) {
  const size_t threshold = 1u << 17;

  String s;
  EXPECT_OK(s.assign_chars('x', threshold - 1));
  EXPECT_EQ(s.capacity(), threshold - 1);

  size_t reallocations = 0;
  for (size_t i = 0; i < 100; i++) {
    size_t previous_capacity = s.capacity();
    EXPECT_OK(s.append('x'));
    reallocations += s.capacity() != previous_capacity;
  }
  verify_string_chars(s, 'x', threshold + 99);
  EXPECT_EQ(reallocations, 1u);
  EXPECT_EQ((s.capacity() + 1u) % kCapacityAlignment, 0u);
}

TEST_CASE(core_string_growth_transition_alignment) {
  // Force growth with requested allocation sizes just below, at, and above each growth-rate transition.
  for (size_t threshold : {size_t(1u << 17), size_t(1u << 22)}) {
    for (size_t additional : {size_t(127), size_t(128), size_t(129)}) {
      String s;
      size_t initial_size = threshold - 129;
      EXPECT_OK(s.assign_chars('x', initial_size));
      EXPECT_EQ(s.capacity(), initial_size);
      EXPECT_OK(s.append_chars('x', additional));
      verify_string_chars(s, 'x', initial_size + additional);
      EXPECT_GT(s.capacity(), initial_size);
      EXPECT_EQ((s.capacity() + 1u) % kCapacityAlignment, 0u);
    }
  }
}

TEST_CASE_T(core_string_format_number, string_storage_types) {
  struct SignedNumberCase {
    int64_t value;
    uint32_t base;
    size_t width;
    StringFormatFlags flags;
    const char* expected;
  };

  struct UnsignedNumberCase {
    uint64_t value;
    uint32_t base;
    size_t width;
    StringFormatFlags flags;
    const char* expected;
  };

  using F = StringFormatFlags;
  const UnsignedNumberCase unsigned_cases[] = {
    { 0         ,  0,  0, F::kNone, "0"},
    { 0         ,  2,  0, F::kNone, "0"},
    { 0         ,  8,  0, F::kNone, "0"},
    { 0         , 10,  0, F::kNone, "0"},
    { 0         , 16,  0, F::kNone, "0"},
    { 42        ,  0,  0, F::kNone, "42"},
    { 42        ,  2,  0, F::kNone, "101010"},
    { 42        ,  8,  0, F::kNone, "52"},
    { 42        , 10,  0, F::kNone, "42"},
    { 42        , 16,  0, F::kNone, "2A"},
    { UINT64_MAX,  2,  0, F::kNone, "1111111111111111111111111111111111111111111111111111111111111111"},
    { UINT64_MAX,  8,  0, F::kNone, "1777777777777777777777"},
    { UINT64_MAX, 10,  0, F::kNone, "18446744073709551615"},
    { UINT64_MAX, 10,  0, F::kNone, "18446744073709551615"},
    { UINT64_MAX, 10, 22, F::kNone, "0018446744073709551615"},
    { UINT64_MAX, 16,  0, F::kNone, "FFFFFFFFFFFFFFFF"},
    { UINT64_MAX, 10,  0, F::kSigned, "-1"},
    { 0         , 10,  0, F::kShowSign, "+0"},
    { 42        , 10,  0, F::kShowSign, "+42"},
    { 42        , 10,  0, F::kShowSpace, " 42"},
    { 0         ,  8,  0, F::kAlternate, "0"},
    { 42        ,  8,  0, F::kAlternate, "052"},
    { 0         , 16,  0, F::kAlternate, "0x0"},
    { 42        , 16,  0, F::kAlternate, "0x2A"},
    { 42        , 10,  0, F::kAlternate, "42"},
    { 42        ,  2,  0, F::kAlternate, "101010"},
    { 42        , 10,  1, F::kNone, "42"},
    { 42        , 10,  2, F::kNone, "42"},
    { 42        , 10,  5, F::kNone, "00042"},
    { 0         , 10,  5, F::kNone, "00000"},
    { 42        , 10,  5, F::kShowSign, "+00042"},
    { 42        , 10,  5, F::kShowSpace, " 00042"},
    { 42        ,  8,  5, F::kAlternate, "000052"},
    { 42        , 16,  5, F::kAlternate, "0x0002A"},
    { 42        , 16,  5, F::kAlternate | F::kShowSign | F::kShowSpace, "+0x0002A"},
    { 42        , 16,  5, F::kAlternate | F::kShowSpace, " 0x0002A"},
  };

  const SignedNumberCase signed_cases[] = {
    { 0         ,  0,  0, F::kNone, "0"},
    { 42        , 10,  0, F::kNone, "42"},
    { -42       ,  0,  0, F::kNone, "-42"},
    { -42       ,  2,  0, F::kNone, "-101010"},
    { -42       ,  8,  0, F::kNone, "-52"},
    { -42       , 16,  0, F::kNone, "-2A"},
    { INT64_MIN ,  2,  0, F::kNone, "-1000000000000000000000000000000000000000000000000000000000000000"},
    { INT64_MIN ,  8,  0, F::kNone, "-1000000000000000000000"},
    { INT64_MIN , 10,  0, F::kNone, "-9223372036854775808"},
    { INT64_MIN , 16,  0, F::kNone, "-8000000000000000"},
    { INT64_MAX , 10,  0, F::kNone, "9223372036854775807"},
    { INT64_MAX , 10, 20, F::kNone, "09223372036854775807"},
    { 42        , 10,  0, F::kShowSpace, " 42"},
    { 42        , 10,  5, F::kShowSpace, " 00042"},
    { 42        , 10,  0, F::kShowSign | F::kShowSpace, "+42"},
    { -42       , 10,  0, F::kShowSign | F::kShowSpace, "-42"},
    { -42       , 10,  5, F::kNone, "-00042"},
    { -42       , 10,  5, F::kShowSign, "-00042"},
    { -42       , 16,  5, F::kAlternate | F::kShowSign | F::kShowSpace, "-0x0002A"}
  };

  for (const UnsignedNumberCase& c : unsigned_cases) {
    T s;
    Error err = s.assign_int(c.value, c.base, c.width, c.flags);
    EXPECT_OK(err);
    verify_string_data(s, c.expected);

    EXPECT_OK(s.assign("prefix:"));
    err = s.append_int(c.value, c.base, c.width, c.flags);

    EXPECT_OK(err);
    char expected[80] = "prefix:";
    size_t size = strlen(c.expected);
    memcpy(expected + 7, c.expected, size);
    verify_string_data(s, expected, 7 + size);
  }

  for (const SignedNumberCase& c : signed_cases) {
    T s;

    Error err = s.assign_int(int64_t(c.value), c.base, c.width, c.flags);
    EXPECT_OK(err);
    verify_string_data(s, c.expected);

    EXPECT_OK(s.assign("prefix:"));
    err = s.append_int(int64_t(c.value), c.base, c.width, c.flags);

    EXPECT_OK(err);
    char expected[80] = "prefix:";
    size_t size = strlen(c.expected);
    memcpy(expected + 7, c.expected, size);
    verify_string_data(s, expected, 7 + size);
  }
}

TEST_CASE_T(core_string_format_number_width_overflow, string_storage_types) {
  T s;

  EXPECT_OK(s.assign("keep"));
  char* data = s.data();
  size_t capacity = s.capacity();

  for (StringFormatFlags flags : {StringFormatFlags::kNone, StringFormatFlags::kShowSign | StringFormatFlags::kAlternate}) {
    EXPECT_EQ(s.assign_int(42u, 16, SIZE_MAX, flags), Error::kOutOfMemory);
    verify_string_data(s, "keep");

    EXPECT_EQ(s.append_int(42u, 16, SIZE_MAX, flags), Error::kOutOfMemory);
    EXPECT_EQ(s.data(), data);
    EXPECT_EQ(s.capacity(), capacity);
    verify_string_data(s, "keep");
  }
}

TEST_CASE_T(core_string_format_number_invalid_base, string_storage_types) {
  T s;

  EXPECT_OK(s.assign("keep"));
  char* data = s.data();
  size_t capacity = s.capacity();

  for (uint32_t base : {1u, 3u, 7u, 9u, 11u, 17u, 36u, UINT32_MAX}) {
    EXPECT_EQ(s.assign_int(42u, base), Error::kInvalidArgument);
    verify_string_data(s, "keep");

    EXPECT_EQ(s.assign_int(-42, base), Error::kInvalidArgument);
    verify_string_data(s, "keep");

    EXPECT_EQ(s.append_int(42u, base), Error::kInvalidArgument);
    verify_string_data(s, "keep");

    EXPECT_EQ(s.append_int(-42, base), Error::kInvalidArgument);
    EXPECT_EQ(s.data(), data);
    EXPECT_EQ(s.capacity(), capacity);
    verify_string_data(s, "keep");
  }
}

TEST_CASE_T(core_string_format_hex_data, string_storage_types) {
  uint8_t bytes[256];
  for (size_t i = 0; i < 256; i++) {
    bytes[i] = uint8_t(i);
  }

  char expected[sizeof(bytes) * 3];
  char appended[sizeof(expected) + 7];

  for (char separator : {'\0', ':', ' '}) {
    for (size_t size : {size_t(1), size_t(2), size_t(17), sizeof(bytes)}) {
      T s;

      // Use the C formatter as an independent reference for byte conversion and separator placement.
      char* p = expected;
      for (size_t i = 0; i < size; i++) {
        if (i && separator) {
          *p++ = separator;
        }
        EXPECT_EQ(snprintf(p, 3, "%02X", unsigned(bytes[i])), 2);
        p += 2;
      }

      EXPECT_OK(s.assign("old"));
      EXPECT_OK(s.assign_hex(bytes, size, separator));
      verify_string_data(s, expected);

      EXPECT_OK(s.assign("prefix:"));
      EXPECT_OK(s.append_hex(bytes, size, separator));

      memcpy(appended, "prefix:", 7);
      memcpy(appended + 7, expected, size_t(p - expected));
      verify_string_data(s, appended, 7 + size_t(p - expected));
    }
  }
}

TEST_CASE_T(core_string_format_hex_size_overflow, string_storage_types) {
  T s;
  const uint8_t fmt_byte = 0;

  EXPECT_OK(s.assign("keep"));
  char* data = s.data();
  size_t capacity = s.capacity();

  for (char separator : {'\0', ':'}) {
    size_t limit = separator ? SIZE_MAX / 3 : SIZE_MAX / 2;
    for (size_t size : {limit - 1, limit, limit + 1, SIZE_MAX}) {
      EXPECT_EQ(s.assign_hex(&fmt_byte, size, separator), Error::kOutOfMemory);
      verify_string_data(s, "keep");

      EXPECT_EQ(s.append_hex(&fmt_byte, size, separator), Error::kOutOfMemory);
      EXPECT_EQ(s.data(), data);
      EXPECT_EQ(s.capacity(), capacity);
      verify_string_data(s, "keep");
    }
  }
}

TEST_CASE_T(core_string_format, string_storage_types) {
  T s;
  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_format("%s:%d:%08X:%.2f", "value", -42, 0xABu, 1.25));
  verify_string_data(s, "value:-42:000000AB:1.25");

  EXPECT_OK(s.append_format("/%c/%%", 'x'));
  verify_string_data(s, "value:-42:000000AB:1.25/x/%");

  const char binary[] = {'a', '\0', 'b'};
  EXPECT_OK(s.assign_format("a%cb", 0));
  verify_string_data(s, binary, sizeof(binary));

  EXPECT_OK(s.assign_format("a%cb", 0));
  verify_string_data(s, binary, sizeof(binary));
}

TEST_CASE(core_string_format_empty) {
  StringTmp<256> s;

  EXPECT_OK(s.assign("old"));
  EXPECT_OK(s.assign_format("%s", ""));
  verify_string_data(s, "");

  EXPECT_OK(s.assign("keep"));
  EXPECT_OK(s.append_format("%s", ""));
  verify_string_data(s, "keep");
}

TEST_CASE(core_string_format_assign_capacity) {
  // Exercise formatted assignment at the boundaries of both owned and external buffers.
  for (bool external : {false, true}) {
    for (int delta : {-1, 0, 1}) {
      String owned;
      StringTmp<256> tmp;

      EXPECT_OK(owned.assign_chars('x', 200));
      String& s = external ? static_cast<String&>(tmp) : owned;

      EXPECT_OK(s.assign("prefix:"));
      size_t output_size = size_t(int(s.capacity()) + delta);

      char pattern[513];
      EXPECT_LT(output_size, ASMJIT_ARRAY_SIZE(pattern));
      make_abc_pattern(pattern, output_size);

      // Initialize spare bytes so the terminator check does not depend on allocator contents.
      memset(s.data() + s.size() + 1, '?', s.capacity() - s.size());
      char* old_data = s.data();
      size_t old_capacity = s.capacity();

      EXPECT_OK(s.assign_format("%s", pattern));
      verify_string_data(s, pattern, output_size);

      if (delta <= 0) {
        EXPECT_EQ(s.data(), old_data);
        EXPECT_EQ(s.capacity(), old_capacity);
        EXPECT_EQ(s.is_external(), external);
      }
      else if (external) {
        EXPECT_FALSE(s.is_external());
      }
    }
  }
}

TEST_CASE(core_string_format_append_capacity) {
  // Exercise formatted append at the boundaries of both owned and external buffers.
  for (bool external : {false, true}) {
    for (int delta : {-1, 0, 1}) {
      String owned;
      StringTmp<256> tmp;

      EXPECT_OK(owned.assign_chars('x', 200));
      String& s = external ? static_cast<String&>(tmp) : owned;

      EXPECT_OK(s.assign("prefix:"));
      size_t start = s.size();
      size_t remaining = s.capacity() - start;
      size_t output_size = size_t(int(remaining) + delta);

      char pattern[513];
      char expected[521];
      EXPECT_LT(output_size, ASMJIT_ARRAY_SIZE(pattern));

      make_abc_pattern(pattern, output_size);
      memcpy(expected, "prefix:", start);
      memcpy(expected + start, pattern, output_size);
      memset(s.data() + s.size() + 1, '?', s.capacity() - s.size());
      char* old_data = s.data();
      size_t old_capacity = s.capacity();

      EXPECT_OK(s.append_format("%s", pattern));
      verify_string_data(s, expected, start + output_size);

      if (delta <= 0) {
        EXPECT_EQ(s.data(), old_data);
        EXPECT_EQ(s.capacity(), old_capacity);
        EXPECT_EQ(s.is_external(), external);
      }
      else if (external) {
        EXPECT_FALSE(s.is_external());
      }
    }
  }
}

TEST_CASE_T(core_string_assign_format_stack_boundary, string_storage_types) {
  char pattern[4097];
  char expected[4129];

  for (size_t size : {size_t(1023), size_t(1024), size_t(2048), size_t(3071), size_t(3072), size_t(3073), size_t(4096)}) {
    make_abc_pattern(pattern, size);
    T s;

    EXPECT_OK(s.assign_format("%s", pattern));
    verify_string_data(s, pattern, size);

    T mixed;
    EXPECT_OK(mixed.assign("old"));
    memcpy(expected, pattern, size);
    memcpy(expected + size, "/123/2.5", 8);

    EXPECT_OK(mixed.assign_format("%s/%d/%.1f", pattern, 123, 2.5));
    verify_string_data(mixed, expected, size + 8);
  }
}

TEST_CASE_T(core_string_append_format_stack_boundary, string_storage_types) {
  char pattern[4097];
  char expected[4129];
  for (size_t size : {size_t(1023), size_t(1024), size_t(2048), size_t(3071), size_t(3072), size_t(3073), size_t(4096)}) {
    make_abc_pattern(pattern, size);
    T s;

    EXPECT_OK(s.assign("prefix:"));
    memcpy(expected, "prefix:", 7);
    memcpy(expected + 7, pattern, size);

    EXPECT_OK(s.append_format("%s", pattern));
    verify_string_data(s, expected, 7 + size);

    T mixed;
    EXPECT_OK(mixed.assign("prefix:"));
    memcpy(expected + 7 + size, "/123/2.5", 8);

    EXPECT_OK(mixed.append_format("%s/%d/%.1f", pattern, 123, 2.5));
    verify_string_data(mixed, expected, 7 + size + 8);
  }
}

} // {StringTests}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
