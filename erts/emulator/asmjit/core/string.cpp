// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/axl/commons.h>
#include <asmjit/axl/mem_alloc.h>
#include <asmjit/core/string.h>

ASMJIT_BEGIN_NAMESPACE

// String - Globals
// ================

// NOTE: Generally, we can allow up to SIZE_MAX allocations - but when the allocation is large (and will never be
// allocated by the underlying allocator as it overflows theoretical address space, like `SIZE_MAX`, some tools
// such as valgrind and ASAN would start complaining). Thus, it's better to restrict the allocations on 64-bit
// targets to 56 bits. The allocations would fail anyway, but sanitizers would not complain.
static constexpr size_t kStringCapacityAlignment = 32;
static constexpr size_t kStringMinAllocSize = 128;
static constexpr size_t kStringMaxAllocSize = sizeof(size_t) >= 8 ? ((SIZE_MAX >> 8) + 1u) : SIZE_MAX - 4095;
static constexpr size_t kStringFormatBufferSize = 128;

static ASMJIT_INLINE size_t String_expand_capacity(size_t byte_size) noexcept {
  // Always added regardless of size.
  static constexpr uint32_t kGrowthAddend = 64;
  // 50% growth rate of containers after its size in bytes reached 128 KiB.
  static constexpr uint32_t kGrowthLimit50 = 1u << 17;
  // 25% growth rate of containers after its size in bytes reached 4 MiB.
  static constexpr uint32_t kGrowthLimit25 = 1u << 22;

  size_t n = size_t(byte_size);
  size_t s = size_t(n >= kGrowthLimit50) + size_t(n >= kGrowthLimit25);
  size_t max_growth = kStringMaxAllocSize - byte_size;
  size_t growth = (n >> s) + kGrowthAddend;

  return axl::align_up((n + axl::min(growth, max_growth)), kStringCapacityAlignment);
}

// String - Formatting
// ===================

static const char string_hex_chars[] = "0123456789ABCDEF";

static Error String_format_number(char buf[kStringFormatBufferSize], uint64_t i, uint32_t base, StringFormatFlags flags, Out<Span<char>> output, Out<size_t> sign_alt_size) noexcept {
  uint64_t orig = i;

  char* p = buf + kStringFormatBufferSize;
  char sign = '\0';

  if (base == 0) {
    base = 10;
  }

  // Sign Part
  // ---------

  if (axl::test(flags, StringFormatFlags::kSigned) && int64_t(i) < 0) {
    i = axl::neg(i);
    sign = '-';
  }
  else if (axl::test(flags, StringFormatFlags::kShowSign)) {
    sign = '+';
  }
  else if (axl::test(flags, StringFormatFlags::kShowSpace)) {
    sign = ' ';
  }

  // Number Part
  // -----------

  switch (base) {
    case 2:
    case 8:
    case 16: {
      uint32_t shift = axl::ctz(base);
      uint32_t mask = base - 1;

      do {
        uint64_t d = i >> shift;
        size_t r = size_t(i & mask);

        *--p = string_hex_chars[r];
        i = d;
      } while (i);

      break;
    }

    case 10: {
      do {
        uint64_t d = i / 10u;
        uint64_t r = i % 10u;

        *--p = char(uint32_t('0') + uint32_t(r));
        i = d;
      } while (i);
      break;
    }

    default:
      return make_error(Error::kInvalidArgument);
  }

  // Alternate Form
  // --------------

  char* prev_p = p;

  if (axl::test(flags, StringFormatFlags::kAlternate)) {
    if (base == 8) {
      if (orig != 0) {
        *--p = '0';
      }
    }
    if (base == 16) {
      *--p = 'x';
      *--p = '0';
    }
  }

  if (sign != 0) {
    *--p = sign;
  }

  output = Span<char>(p, static_cast<size_t>(buf + kStringFormatBufferSize - p));
  sign_alt_size = static_cast<size_t>(prev_p - p);
  return Error::kOk;
}

static inline Error String_copy_formatted_number(char* dst_ptr, Span<const char> formatted_span, size_t sign_alt_size, size_t zero_fill_size) noexcept {
  // sign_alt_size describes a portion in formatted_span.
  ASMJIT_ASSERT(formatted_span.size() >= sign_alt_size);

  if (zero_fill_size != 0u) {
    if (sign_alt_size) {
      memcpy(dst_ptr, formatted_span.data(), sign_alt_size);
      dst_ptr += sign_alt_size;
    }
    memset(dst_ptr, '0', zero_fill_size);
    dst_ptr += zero_fill_size;
    memcpy(dst_ptr, formatted_span.data() + sign_alt_size, formatted_span.size() - sign_alt_size);
    return Error::kOk;
  }
  else {
    memcpy(dst_ptr, formatted_span.data(), formatted_span.size());
    return Error::kOk;
  }
}

static Error String_format_hex(char* dst, const void* data, size_t size, char separator) noexcept {
  ASMJIT_ASSERT(size > 0u);

  size_t i = 0;
  const uint8_t* src = static_cast<const uint8_t*>(data);

  if (separator) {
    for (;;) {
      dst[0] = string_hex_chars[(src[0] >> 4) & 0xF];
      dst[1] = string_hex_chars[(src[0]     ) & 0xF];

      if (++i >= size) {
        break;
      }

      // This makes sure that the separator is only put between two hexadecimal bytes.
      dst[2] = separator;

      dst += 3;
      src += 1;
    }
  }
  else {
    do {
      dst[0] = string_hex_chars[(src[0] >> 4) & 0xF];
      dst[1] = string_hex_chars[(src[0]     ) & 0xF];

      dst += 2;
      src += 1;
    } while (++i < size);
  }

  return Error::kOk;
}

template<bool IsAppend>
inline Error String_vformat(String& self, const char* fmt, va_list ap) noexcept {
  constexpr size_t kStaticBufferSize = 2048 + 1024;
  char static_buf[kStaticBufferSize];

  va_list ap_copy;
  va_copy(ap_copy, ap);

  int fmt_result = vsnprintf(static_buf, kStaticBufferSize, fmt, ap);
  size_t result_size = static_cast<size_t>(fmt_result);

  Error err = Error::kOk;
  if (ASMJIT_LIKELY(result_size < kStaticBufferSize)) {
    err = !IsAppend ? self.assign(static_buf, result_size) : self.append(static_buf, result_size);
  }
  else if (fmt_result < 0) {
    err = make_error(Error::kInvalidArgument);
  }
  else {
    char* dynamic_buf = axl::mem_alloc<char>(result_size + 1u);
    if (!dynamic_buf) {
      err = make_error(Error::kOutOfMemory);
    }
    else {
      fmt_result = vsnprintf(dynamic_buf, result_size + 1u, fmt, ap_copy);
      if (static_cast<size_t>(fmt_result) != result_size) {
        err = make_error(Error::kInvalidState);
      }
      else {
        err = !IsAppend ? self.assign(dynamic_buf, result_size) : self.append(dynamic_buf, result_size);
      }
      axl::mem_free(dynamic_buf);
    }
  }

  va_end(ap_copy);
  return err;
}

// String - Clear & Reset
// ======================

Error String::reset() noexcept {
  if (_type == kTypeLarge) {
    axl::mem_free(_large.data);
  }

  _reset_internal();
  return Error::kOk;
}

Error String::clear() noexcept {
  if (is_large_or_external()) {
    _large.size = 0;
    _large.data[0] = '\0';
  }
  else {
    _raw.uptr[0] = 0;
  }

  return Error::kOk;
}

// String - Prepare
// ================

static ASMJIT_INLINE void String_extract(String& s, uint8_t type, Out<char*> data, Out<size_t> size, Out<size_t> capacity) noexcept {
  if (s.is_large_or_external(type)) {
    data = s._large.data;
    size = s._large.size;
    capacity = s._large.capacity;
  }
  else {
    // For some reason clang's static analysis flags this function having "use-after-free". The step
    // to get to that is to execute this branch (`is_large_or_external()` returning false) and then
    // assuming `type == kTypeLarge` in another condition, which contradicts the first condition.
    ASMJIT_ASSERT(type < String::kTypeLarge);

    data = s._small.data;
    size = s._small.type;
    capacity = s.kSSOCapacity;
  }
}

char* String::assign_uninitialized(size_t assign_size) noexcept {
  uint8_t type = _type;

  char* data;
  size_t size;
  size_t capacity;
  String_extract(*this, type, Out(data), Out(size), Out(capacity));

  if (assign_size > capacity) {
    // Prevent arithmetic overflow.
    if (ASMJIT_UNLIKELY(assign_size >= kStringMaxAllocSize)) {
      return nullptr;
    }

    size_t new_capacity = axl::align_up<size_t>(assign_size + 1, kStringMinAllocSize);
    char* new_data = axl::mem_alloc<char>(new_capacity);

    if (ASMJIT_UNLIKELY(!new_data)) {
      return nullptr;
    }

    if (type == kTypeLarge) {
      axl::mem_free(data);
    }

    _large.type = kTypeLarge;
    _large.size = assign_size;
    _large.capacity = new_capacity - 1;
    _large.data = new_data;

    new_data[assign_size] = '\0';
    return new_data;
  }
  else {
    _set_size(assign_size);
    data[assign_size] = '\0';
    return data;
  }
}

char* String::append_uninitialized(size_t append_size) noexcept {
  uint8_t type = _type;

  char* data;
  size_t size;
  size_t capacity;
  String_extract(*this, type, Out(data), Out(size), Out(capacity));

  size_t new_size = append_size + size;

  if (ASMJIT_LIKELY(capacity - size >= append_size)) {
    _set_size(new_size);
    data[new_size] = '\0';
    return data + size;
  }

  // Prevent arithmetic overflow.
  if (ASMJIT_UNLIKELY(append_size >= kStringMaxAllocSize - size)) {
    return nullptr;
  }

  size_t new_capacity_plus_one = String_expand_capacity(new_size + 1u);
  ASMJIT_ASSERT(new_capacity_plus_one >= new_size + 1u);

  char* new_data = axl::mem_alloc<char>(new_capacity_plus_one);
  if (ASMJIT_UNLIKELY(!new_data)) {
    return nullptr;
  }

  memcpy(new_data, data, size);

  if (type == kTypeLarge) {
    axl::mem_free(data);
  }

  _large.type = kTypeLarge;
  _large.size = new_size;
  _large.capacity = new_capacity_plus_one - 1u;
  _large.data = new_data;

  new_data[new_size] = '\0';
  return new_data + size;
}

// String - Assign
// ===============

Error String::assign(const char* data, size_t data_size) noexcept {
  uint8_t type = _type;
  char* dst = nullptr;

  // Null terminated string without `data_size` specified.
  if (data_size == SIZE_MAX) {
    data_size = data ? strlen(data) : size_t(0);
  }

  if (is_large_or_external(type)) {
    if (data_size <= _large.capacity) {
      // Enough buffer size - this goes to `memmove` at the end.
      dst = _large.data;
      _large.size = data_size;
    }
    else {
      if (ASMJIT_UNLIKELY(data_size >= kStringMaxAllocSize)) {
        return make_error(Error::kOutOfMemory);
      }

      size_t capacity_plus_one = axl::align_up(data_size + 1, 32);
      dst = axl::mem_alloc<char>(capacity_plus_one);

      if (ASMJIT_UNLIKELY(!dst)) {
        return make_error(Error::kOutOfMemory);
      }

      // Do memcpy before free in case `data` is a span of the string itself.
      ::memcpy(dst, data, data_size);

      if (type == kTypeLarge) {
        axl::mem_free(_large.data);
      }

      _large.type = kTypeLarge;
      _large.data = dst;
      _large.size = data_size;
      _large.capacity = capacity_plus_one - 1u;

      dst[data_size] = '\0';
      return Error::kOk;
    }
  }
  else {
    if (data_size <= kSSOCapacity) {
      ASMJIT_ASSERT(data_size < 0xFFu);

      // Enough buffer size - this goes to `memmove` at the end.
      dst = _small.data;
      _small.type = uint8_t(data_size);
    }
    else {
      if (ASMJIT_UNLIKELY(data_size >= kStringMaxAllocSize)) {
        return make_error(Error::kOutOfMemory);
      }

      size_t capacity_plus_one = axl::align_up(data_size + 1, 32);
      dst = axl::mem_alloc<char>(capacity_plus_one);

      if (ASMJIT_UNLIKELY(!dst)) {
        return make_error(Error::kOutOfMemory);
      }

      // Do memcpy before we alter string data in case `data` is a span of the string itself.
      ::memcpy(dst, data, data_size);

      _large.type = kTypeLarge;
      _large.data = dst;
      _large.size = data_size;
      _large.capacity = capacity_plus_one - 1u;

      dst[data_size] = '\0';
      return Error::kOk;
    }
  }

  // NOTE: It's better to use `memmove()` in case `data` is a span of the string itself.
  if (data_size) {
    ::memmove(dst, data, data_size);
  }

  dst[data_size] = '\0';
  return Error::kOk;
}

Error String::assign_chars(char c, size_t n) noexcept {
  char* p = assign_uninitialized(n);
  if (ASMJIT_UNLIKELY(p == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  memset(p, c, n);
  return Error::kOk;
}

Error String::_assign_int(uint64_t i, uint32_t base, size_t width, StringFormatFlags flags) noexcept {
  char buffer[kStringFormatBufferSize];
  Span<char> formatted_span;
  size_t sign_alt_size;

  ASMJIT_PROPAGATE(String_format_number(buffer, i, base, flags, Out(formatted_span), Out(sign_alt_size)));

  // Just correct the width here so no calculations would overflow. If it's that large it will fail later.
  if (width > kStringMaxAllocSize + 1u) {
    width = kStringMaxAllocSize + 1u;
  }

  size_t number_size = formatted_span.size() - sign_alt_size;
  size_t full_width_size = axl::max(width, number_size);
  size_t zero_fill_size = full_width_size - number_size;

  char* p = assign_uninitialized(formatted_span.size() + zero_fill_size);
  if (ASMJIT_UNLIKELY(p == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  return String_copy_formatted_number(p, formatted_span, sign_alt_size, zero_fill_size);
}

Error String::assign_hex(const void* data, size_t size, char separator) noexcept {
  if (size - 1u >= (kStringMaxAllocSize - 1u) / 2u) {
    return size == 0 ? clear() : make_error(Error::kOutOfMemory);
  }

  size_t formatted_size = size * 2u;
  if (separator) {
    if (size > (kStringMaxAllocSize - 1u) / 3u) {
      return make_error(Error::kOutOfMemory);
    }
    formatted_size += size - 1u;
  }

  char* dst = assign_uninitialized(formatted_size);
  if (ASMJIT_UNLIKELY(dst == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  return String_format_hex(dst, data, size, separator);
}

Error String::assign_format(const char* fmt, ...) noexcept {
  Error err;
  va_list ap;

  va_start(ap, fmt);
  err = assign_vformat(fmt, ap);
  va_end(ap);

  return err;
}

Error String::assign_vformat(const char* fmt, va_list ap) noexcept {
  return String_vformat<false>(*this, fmt, ap);
}

// String - Append
// ===============

Error String::append(const char* data, size_t data_size) noexcept {
  if (data_size == SIZE_MAX) {
    data_size = data ? strlen(data) : size_t(0);
  }

  if (data_size == 0u) {
    return Error::kOk;
  }

  uint8_t type = _type;

  char* self_data;
  size_t self_size;
  size_t capacity;
  String_extract(*this, type, Out(self_data), Out(self_size), Out(capacity));

  if (capacity - self_size >= data_size) {
    self_data += self_size;
    memmove(self_data, data, data_size);
    self_data[data_size] = '\0';
    _set_size(self_size + data_size);
    return Error::kOk;
  }

  // Prevent arithmetic overflow.
  if (ASMJIT_UNLIKELY(data_size >= kStringMaxAllocSize - self_size)) {
    return make_error(Error::kOutOfMemory);
  }

  size_t new_size = data_size + self_size;
  size_t new_capacity_plus_one = String_expand_capacity(new_size + 1u);
  ASMJIT_ASSERT(new_capacity_plus_one >= new_size + 1u);

  char* new_data = axl::mem_alloc<char>(new_capacity_plus_one);
  if (ASMJIT_UNLIKELY(!new_data)) {
    return make_error(Error::kOutOfMemory);
  }

  memcpy(new_data, self_data, self_size);
  memcpy(new_data + self_size, data, data_size);

  if (type == kTypeLarge) {
    axl::mem_free(self_data);
  }

  _large.type = kTypeLarge;
  _large.size = new_size;
  _large.capacity = new_capacity_plus_one - 1u;
  _large.data = new_data;

  new_data[new_size] = '\0';
  return Error::kOk;
}

Error String::append_chars(char c, size_t n) noexcept {
  char* p = append_uninitialized(n);
  if (ASMJIT_UNLIKELY(p == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  memset(p, c, n);
  return Error::kOk;
}

Error String::_append_int(uint64_t i, uint32_t base, size_t width, StringFormatFlags flags) noexcept {
  char buffer[kStringFormatBufferSize];
  Span<char> formatted_span;
  size_t sign_alt_size;

  // Just correct the width here so no calculations would overflow. If it's large the function will fail.
  if (width > kStringMaxAllocSize + 1u) {
    width = kStringMaxAllocSize + 1u;
  }

  ASMJIT_PROPAGATE(String_format_number(buffer, i, base, flags, Out(formatted_span), Out(sign_alt_size)));
  size_t number_size = formatted_span.size() - sign_alt_size;
  size_t full_width_size = axl::max(width, number_size);
  size_t zero_fill_size = full_width_size - number_size;

  char* p = append_uninitialized(formatted_span.size() + zero_fill_size);
  if (ASMJIT_UNLIKELY(p == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  return String_copy_formatted_number(p, formatted_span, sign_alt_size, zero_fill_size);
}

Error String::append_hex(const void* data, size_t size, char separator) noexcept {
  if (size - 1u >= kStringMaxAllocSize / 2u) {
    return size == 0 ? Error::kOk : make_error(Error::kOutOfMemory);
  }

  size_t formatted_size = size * 2u;
  if (separator) {
    if (size >= kStringMaxAllocSize / 3u) {
      return make_error(Error::kOutOfMemory);
    }
    formatted_size += size - 1u;
  }

  char* dst = append_uninitialized(formatted_size);
  if (ASMJIT_UNLIKELY(dst == nullptr)) {
    return make_error(Error::kOutOfMemory);
  }
  return String_format_hex(dst, data, size, separator);
}

Error String::append_format(const char* fmt, ...) noexcept {
  Error err;
  va_list ap;

  va_start(ap, fmt);
  err = append_vformat(fmt, ap);
  va_end(ap);

  return err;
}

Error String::append_vformat(const char* fmt, va_list ap) noexcept {
  return String_vformat<true>(*this, fmt, ap);
}

// String - Operations
// ===================

Error String::pad_end(size_t n, char c) noexcept {
  size_t self_size = size();
  return n > self_size ? append_chars(c, n - self_size) : Error::kOk;
}

Error String::truncate(size_t new_size) noexcept {
  if (is_large_or_external()) {
    if (new_size < _large.size) {
      _large.data[new_size] = '\0';
      _large.size = new_size;
    }
  }
  else {
    if (new_size < _type) {
      _small.data[new_size] = '\0';
      _small.type = uint8_t(new_size);
    }
  }

  return Error::kOk;
}

// String - Equality
// =================

bool String::equals(const char* other, size_t size) const noexcept {
  const char* a_data = data();
  const char* b_data = other;

  size_t a_size = this->size();
  size_t b_size = size;

  if (b_size == SIZE_MAX) {
    size_t i;
    for (i = 0; i < a_size; i++) {
      if (a_data[i] != b_data[i] || b_data[i] == 0) {
        return false;
      }
    }
    return b_data[i] == 0;
  }
  else {
    if (a_size != b_size) {
      return false;
    }
    return a_size == 0 || ::memcmp(a_data, b_data, a_size) == 0;
  }
}

ASMJIT_END_NAMESPACE
