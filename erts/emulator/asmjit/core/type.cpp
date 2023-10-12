// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/misc_p.h"
#include "../core/type.h"

ASMJIT_BEGIN_NAMESPACE

namespace TypeUtils {

template<uint32_t Index>
struct ScalarOfTypeId {
  enum : uint32_t {
    kTypeId = uint32_t(
      isScalar(TypeId(Index)) ? TypeId(Index) :
      isMask8 (TypeId(Index)) ? TypeId::kUInt8 :
      isMask16(TypeId(Index)) ? TypeId::kUInt16 :
      isMask32(TypeId(Index)) ? TypeId::kUInt32 :
      isMask64(TypeId(Index)) ? TypeId::kUInt64 :
      isMmx32 (TypeId(Index)) ? TypeId::kUInt32 :
      isMmx64 (TypeId(Index)) ? TypeId::kUInt64 :
      isVec32 (TypeId(Index)) ? TypeId((Index - uint32_t(TypeId::_kVec32Start ) + uint32_t(TypeId::kInt8)) & 0xFF) :
      isVec64 (TypeId(Index)) ? TypeId((Index - uint32_t(TypeId::_kVec64Start ) + uint32_t(TypeId::kInt8)) & 0xFF) :
      isVec128(TypeId(Index)) ? TypeId((Index - uint32_t(TypeId::_kVec128Start) + uint32_t(TypeId::kInt8)) & 0xFF) :
      isVec256(TypeId(Index)) ? TypeId((Index - uint32_t(TypeId::_kVec256Start) + uint32_t(TypeId::kInt8)) & 0xFF) :
      isVec512(TypeId(Index)) ? TypeId((Index - uint32_t(TypeId::_kVec512Start) + uint32_t(TypeId::kInt8)) & 0xFF) : TypeId::kVoid)
  };
};

template<uint32_t Index>
struct SizeOfTypeId {
  enum : uint32_t {
    kTypeSize =
      isInt8   (TypeId(Index)) ?  1 :
      isUInt8  (TypeId(Index)) ?  1 :
      isInt16  (TypeId(Index)) ?  2 :
      isUInt16 (TypeId(Index)) ?  2 :
      isInt32  (TypeId(Index)) ?  4 :
      isUInt32 (TypeId(Index)) ?  4 :
      isInt64  (TypeId(Index)) ?  8 :
      isUInt64 (TypeId(Index)) ?  8 :
      isFloat32(TypeId(Index)) ?  4 :
      isFloat64(TypeId(Index)) ?  8 :
      isFloat80(TypeId(Index)) ? 10 :
      isMask8  (TypeId(Index)) ?  1 :
      isMask16 (TypeId(Index)) ?  2 :
      isMask32 (TypeId(Index)) ?  4 :
      isMask64 (TypeId(Index)) ?  8 :
      isMmx32  (TypeId(Index)) ?  4 :
      isMmx64  (TypeId(Index)) ?  8 :
      isVec32  (TypeId(Index)) ?  4 :
      isVec64  (TypeId(Index)) ?  8 :
      isVec128 (TypeId(Index)) ? 16 :
      isVec256 (TypeId(Index)) ? 32 :
      isVec512 (TypeId(Index)) ? 64 : 0
  };
};

const TypeData _typeData = {
  #define VALUE(x) TypeId(ScalarOfTypeId<x>::kTypeId)
  { ASMJIT_LOOKUP_TABLE_256(VALUE, 0) },
  #undef VALUE

  #define VALUE(x) SizeOfTypeId<x>::kTypeSize
  { ASMJIT_LOOKUP_TABLE_256(VALUE, 0) }
  #undef VALUE
};

} // {TypeUtils}

ASMJIT_END_NAMESPACE
