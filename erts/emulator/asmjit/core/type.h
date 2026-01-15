// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_TYPE_H_INCLUDED
#define ASMJIT_CORE_TYPE_H_INCLUDED

#include "../core/globals.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Type identifier provides a minimalist type system used across AsmJit library.
//!
//! This is an additional information that can be used to describe a value-type of physical or virtual register. It's
//! used mostly by BaseCompiler to describe register representation (the group of data stored in the register and the
//! width used) and it's also used by APIs that allow to describe and work with function signatures.
enum class TypeId : uint8_t {
  //! Void type.
  kVoid = 0,

  _kBaseStart = 32,
  _kBaseEnd = 44,

  _kIntStart = 32,
  _kIntEnd = 41,

  //! Abstract signed integer type that has a native size.
  kIntPtr = 32,
  //! Abstract unsigned integer type that has a native size.
  kUIntPtr = 33,

  //! 8-bit signed integer type.
  kInt8 = 34,
  //! 8-bit unsigned integer type.
  kUInt8 = 35,
  //! 16-bit signed integer type.
  kInt16 = 36,
  //! 16-bit unsigned integer type.
  kUInt16 = 37,
  //! 32-bit signed integer type.
  kInt32 = 38,
  //! 32-bit unsigned integer type.
  kUInt32 = 39,
  //! 64-bit signed integer type.
  kInt64 = 40,
  //! 64-bit unsigned integer type.
  kUInt64 = 41,

  _kFloatStart  = 42,
  _kFloatEnd = 44,

  //! 32-bit floating point type.
  kFloat32 = 42,
  //! 64-bit floating point type.
  kFloat64 = 43,
  //! 80-bit floating point type.
  kFloat80 = 44,

  _kMaskStart = 45,
  _kMaskEnd = 48,

  //! 8-bit opmask register (K).
  kMask8 = 45,
  //! 16-bit opmask register (K).
  kMask16 = 46,
  //! 32-bit opmask register (K).
  kMask32 = 47,
  //! 64-bit opmask register (K).
  kMask64 = 48,

  _kMmxStart = 49,
  _kMmxEnd = 50,

  //! 64-bit MMX register only used for 32 bits.
  kMmx32 = 49,
  //! 64-bit MMX register.
  kMmx64 = 50,

  _kVec32Start  = 51,
  _kVec32End = 60,

  kInt8x4 = 51,
  kUInt8x4 = 52,
  kInt16x2 = 53,
  kUInt16x2 = 54,
  kInt32x1 = 55,
  kUInt32x1 = 56,
  kFloat32x1 = 59,

  _kVec64Start  = 61,
  _kVec64End = 70,

  kInt8x8 = 61,
  kUInt8x8 = 62,
  kInt16x4 = 63,
  kUInt16x4 = 64,
  kInt32x2 = 65,
  kUInt32x2 = 66,
  kInt64x1 = 67,
  kUInt64x1 = 68,
  kFloat32x2 = 69,
  kFloat64x1 = 70,

  _kVec128Start = 71,
  _kVec128End = 80,

  kInt8x16 = 71,
  kUInt8x16 = 72,
  kInt16x8 = 73,
  kUInt16x8 = 74,
  kInt32x4 = 75,
  kUInt32x4 = 76,
  kInt64x2 = 77,
  kUInt64x2 = 78,
  kFloat32x4 = 79,
  kFloat64x2 = 80,

  _kVec256Start = 81,
  _kVec256End = 90,

  kInt8x32 = 81,
  kUInt8x32 = 82,
  kInt16x16 = 83,
  kUInt16x16 = 84,
  kInt32x8 = 85,
  kUInt32x8 = 86,
  kInt64x4 = 87,
  kUInt64x4 = 88,
  kFloat32x8 = 89,
  kFloat64x4 = 90,

  _kVec512Start = 91,
  _kVec512End = 100,

  kInt8x64 = 91,
  kUInt8x64 = 92,
  kInt16x32 = 93,
  kUInt16x32 = 94,
  kInt32x16 = 95,
  kUInt32x16 = 96,
  kInt64x8 = 97,
  kUInt64x8 = 98,
  kFloat32x16 = 99,
  kFloat64x8 = 100,

  kLastAssigned = kFloat64x8,

  kMaxValue = 255
};
ASMJIT_DEFINE_ENUM_COMPARE(TypeId)

//! Type identifier utilities.
namespace TypeUtils {

struct TypeData {
  TypeId scalarOf[uint32_t(TypeId::kMaxValue) + 1];
  uint8_t sizeOf[uint32_t(TypeId::kMaxValue) + 1];
};
ASMJIT_VARAPI const TypeData _typeData;

//! Returns the scalar type of `typeId`.
static ASMJIT_INLINE_NODEBUG TypeId scalarOf(TypeId typeId) noexcept { return _typeData.scalarOf[uint32_t(typeId)]; }

//! Returns the size [in bytes] of `typeId`.
static ASMJIT_INLINE_NODEBUG uint32_t sizeOf(TypeId typeId) noexcept { return _typeData.sizeOf[uint32_t(typeId)]; }

//! Tests whether a given type `typeId` is between `a` and `b`.
static ASMJIT_INLINE_NODEBUG constexpr bool isBetween(TypeId typeId, TypeId a, TypeId b) noexcept {
  return Support::isBetween(uint32_t(typeId), uint32_t(a), uint32_t(b));
}

//! Tests whether a given type `typeId` is \ref TypeId::kVoid.
static ASMJIT_INLINE_NODEBUG constexpr bool isVoid(TypeId typeId) noexcept { return typeId == TypeId::kVoid; }
//! Tests whether a given type `typeId` is a valid non-void type.
static ASMJIT_INLINE_NODEBUG constexpr bool isValid(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kIntStart, TypeId::_kVec512End); }
//! Tests whether a given type `typeId` is scalar (has no vector part).
static ASMJIT_INLINE_NODEBUG constexpr bool isScalar(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kBaseStart, TypeId::_kBaseEnd); }
//! Tests whether a given type `typeId` is abstract, which means that its size depends on register size.
static ASMJIT_INLINE_NODEBUG constexpr bool isAbstract(TypeId typeId) noexcept { return isBetween(typeId, TypeId::kIntPtr, TypeId::kUIntPtr); }

//! Tests whether a given type is a scalar integer (signed or unsigned) of any size.
static ASMJIT_INLINE_NODEBUG constexpr bool isInt(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kIntStart, TypeId::_kIntEnd); }
//! Tests whether a given type is a scalar 8-bit integer (signed).
static ASMJIT_INLINE_NODEBUG constexpr bool isInt8(TypeId typeId) noexcept { return typeId == TypeId::kInt8; }
//! Tests whether a given type is a scalar 8-bit integer (unsigned).
static ASMJIT_INLINE_NODEBUG constexpr bool isUInt8(TypeId typeId) noexcept { return typeId == TypeId::kUInt8; }
//! Tests whether a given type is a scalar 16-bit integer (signed).
static ASMJIT_INLINE_NODEBUG constexpr bool isInt16(TypeId typeId) noexcept { return typeId == TypeId::kInt16; }
//! Tests whether a given type is a scalar 16-bit integer (unsigned).
static ASMJIT_INLINE_NODEBUG constexpr bool isUInt16(TypeId typeId) noexcept { return typeId == TypeId::kUInt16; }
//! Tests whether a given type is a scalar 32-bit integer (signed).
static ASMJIT_INLINE_NODEBUG constexpr bool isInt32(TypeId typeId) noexcept { return typeId == TypeId::kInt32; }
//! Tests whether a given type is a scalar 32-bit integer (unsigned).
static ASMJIT_INLINE_NODEBUG constexpr bool isUInt32(TypeId typeId) noexcept { return typeId == TypeId::kUInt32; }
//! Tests whether a given type is a scalar 64-bit integer (signed).
static ASMJIT_INLINE_NODEBUG constexpr bool isInt64(TypeId typeId) noexcept { return typeId == TypeId::kInt64; }
//! Tests whether a given type is a scalar 64-bit integer (unsigned).
static ASMJIT_INLINE_NODEBUG constexpr bool isUInt64(TypeId typeId) noexcept { return typeId == TypeId::kUInt64; }

//! Tests whether a given type is an 8-bit general purpose register representing either signed or unsigned 8-bit integer.
static ASMJIT_INLINE_NODEBUG constexpr bool isGp8(TypeId typeId) noexcept { return isBetween(typeId, TypeId::kInt8, TypeId::kUInt8); }
//! Tests whether a given type is a 16-bit general purpose register representing either signed or unsigned 16-bit integer
static ASMJIT_INLINE_NODEBUG constexpr bool isGp16(TypeId typeId) noexcept { return isBetween(typeId, TypeId::kInt16, TypeId::kUInt16); }
//! Tests whether a given type is a 32-bit general purpose register representing either signed or unsigned 32-bit integer
static ASMJIT_INLINE_NODEBUG constexpr bool isGp32(TypeId typeId) noexcept { return isBetween(typeId, TypeId::kInt32, TypeId::kUInt32); }
//! Tests whether a given type is a 64-bit general purpose register representing either signed or unsigned 64-bit integer
static ASMJIT_INLINE_NODEBUG constexpr bool isGp64(TypeId typeId) noexcept { return isBetween(typeId, TypeId::kInt64, TypeId::kUInt64); }

//! Tests whether a given type is a scalar floating point of any size.
static ASMJIT_INLINE_NODEBUG constexpr bool isFloat(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kFloatStart, TypeId::_kFloatEnd); }
//! Tests whether a given type is a scalar 32-bit float.
static ASMJIT_INLINE_NODEBUG constexpr bool isFloat32(TypeId typeId) noexcept { return typeId == TypeId::kFloat32; }
//! Tests whether a given type is a scalar 64-bit float.
static ASMJIT_INLINE_NODEBUG constexpr bool isFloat64(TypeId typeId) noexcept { return typeId == TypeId::kFloat64; }
//! Tests whether a given type is a scalar 80-bit float.
static ASMJIT_INLINE_NODEBUG constexpr bool isFloat80(TypeId typeId) noexcept { return typeId == TypeId::kFloat80; }

//! Tests whether a given type is a mask register of any size.
static ASMJIT_INLINE_NODEBUG constexpr bool isMask(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kMaskStart, TypeId::_kMaskEnd); }
//! Tests whether a given type is an 8-bit mask register.
static ASMJIT_INLINE_NODEBUG constexpr bool isMask8(TypeId typeId) noexcept { return typeId == TypeId::kMask8; }
//! Tests whether a given type is an 16-bit mask register.
static ASMJIT_INLINE_NODEBUG constexpr bool isMask16(TypeId typeId) noexcept { return typeId == TypeId::kMask16; }
//! Tests whether a given type is an 32-bit mask register.
static ASMJIT_INLINE_NODEBUG constexpr bool isMask32(TypeId typeId) noexcept { return typeId == TypeId::kMask32; }
//! Tests whether a given type is an 64-bit mask register.
static ASMJIT_INLINE_NODEBUG constexpr bool isMask64(TypeId typeId) noexcept { return typeId == TypeId::kMask64; }

//! Tests whether a given type is an MMX register.
//!
//! \note MMX functionality is in general deprecated on X86 architecture. AsmJit provides it just for completeness.
static ASMJIT_INLINE_NODEBUG constexpr bool isMmx(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kMmxStart, TypeId::_kMmxEnd); }
//! Tests whether a given type is an MMX register, which only uses the low 32 bits of data (only specific cases).
//!
//! \note MMX functionality is in general deprecated on X86 architecture. AsmJit provides it just for completeness.
static ASMJIT_INLINE_NODEBUG constexpr bool isMmx32(TypeId typeId) noexcept { return typeId == TypeId::kMmx32; }
//! Tests whether a given type is an MMX register, which uses 64 bits of data (default).
//!
//! \note MMX functionality is in general deprecated on X86 architecture. AsmJit provides it just for completeness.
static ASMJIT_INLINE_NODEBUG constexpr bool isMmx64(TypeId typeId) noexcept { return typeId == TypeId::kMmx64; }

//! Tests whether a given type is a vector register of any size.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec32Start, TypeId::_kVec512End); }
//! Tests whether a given type is a 32-bit or 32-bit view of a vector register.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec32(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec32Start, TypeId::_kVec32End); }
//! Tests whether a given type is a 64-bit or 64-bit view of a vector register.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec64(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec64Start, TypeId::_kVec64End); }
//! Tests whether a given type is a 128-bit or 128-bit view of a vector register.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec128(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec128Start, TypeId::_kVec128End); }
//! Tests whether a given type is a 256-bit or 256-bit view of a vector register.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec256(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec256Start, TypeId::_kVec256End); }
//! Tests whether a given type is a 512-bit or 512-bit view of a vector register.
static ASMJIT_INLINE_NODEBUG constexpr bool isVec512(TypeId typeId) noexcept { return isBetween(typeId, TypeId::_kVec512Start, TypeId::_kVec512End); }

//! \cond
enum TypeCategory : uint32_t {
  kTypeCategoryUnknown = 0,
  kTypeCategoryEnum = 1,
  kTypeCategoryIntegral = 2,
  kTypeCategoryFloatingPoint = 3,
  kTypeCategoryFunction = 4
};

template<typename T, TypeCategory kCategory>
struct TypeIdOfT_ByCategory {}; // Fails if not specialized.

template<typename T>
struct TypeIdOfT_ByCategory<T, kTypeCategoryIntegral> {
  enum : uint32_t {
    kTypeId = uint32_t(
      (sizeof(T) == 1 &&  std::is_signed<T>::value) ? TypeId::kInt8 :
      (sizeof(T) == 1 && !std::is_signed<T>::value) ? TypeId::kUInt8 :
      (sizeof(T) == 2 &&  std::is_signed<T>::value) ? TypeId::kInt16 :
      (sizeof(T) == 2 && !std::is_signed<T>::value) ? TypeId::kUInt16 :
      (sizeof(T) == 4 &&  std::is_signed<T>::value) ? TypeId::kInt32 :
      (sizeof(T) == 4 && !std::is_signed<T>::value) ? TypeId::kUInt32 :
      (sizeof(T) == 8 &&  std::is_signed<T>::value) ? TypeId::kInt64 :
      (sizeof(T) == 8 && !std::is_signed<T>::value) ? TypeId::kUInt64 : TypeId::kVoid)
  };
};

template<typename T>
struct TypeIdOfT_ByCategory<T, kTypeCategoryFloatingPoint> {
  enum : uint32_t {
    kTypeId = uint32_t(
      (sizeof(T) == 4 ) ? TypeId::kFloat32 :
      (sizeof(T) == 8 ) ? TypeId::kFloat64 :
      (sizeof(T) >= 10) ? TypeId::kFloat80 : TypeId::kVoid)
  };
};

template<typename T>
struct TypeIdOfT_ByCategory<T, kTypeCategoryEnum>
  : public TypeIdOfT_ByCategory<typename std::underlying_type<T>::type, kTypeCategoryIntegral> {};

template<typename T>
struct TypeIdOfT_ByCategory<T, kTypeCategoryFunction> {
  enum : uint32_t {
    kTypeId = uint32_t(TypeId::kUIntPtr)
  };
};
//! \endcond

//! TypeIdOfT<> template allows to get a TypeId from a C++ type `T`.
#ifdef _DOXYGEN
template<typename T>
struct TypeIdOfT {
  //! TypeId of C++ type `T`.
  static constexpr TypeId kTypeId = _TypeIdDeducedAtCompileTime_;
};
#else
template<typename T>
struct TypeIdOfT
  : public TypeIdOfT_ByCategory<T,
    std::is_enum<T>::value           ? kTypeCategoryEnum          :
    std::is_integral<T>::value       ? kTypeCategoryIntegral      :
    std::is_floating_point<T>::value ? kTypeCategoryFloatingPoint :
    std::is_function<T>::value       ? kTypeCategoryFunction      : kTypeCategoryUnknown> {};
#endif

//! \cond
template<typename T>
struct TypeIdOfT<T*> {
  enum : uint32_t {
    kTypeId = uint32_t(TypeId::kUIntPtr)
  };
};

template<typename T>
struct TypeIdOfT<T&> {
  enum : uint32_t {
    kTypeId = uint32_t(TypeId::kUIntPtr)
  };
};
//! \endcond

//! Returns a corresponding \ref TypeId of `T` type.
template<typename T>
static ASMJIT_INLINE_NODEBUG constexpr TypeId typeIdOfT() noexcept { return TypeId(TypeIdOfT<T>::kTypeId); }

//! Returns offset needed to convert a `kIntPtr` and `kUIntPtr` TypeId into a type that matches `registerSize`
//! (general-purpose register size). If you find such TypeId it's then only about adding the offset to it.
//!
//! For example:
//!
//! ```
//! uint32_t registerSize = /* 4 or 8 */;
//! uint32_t deabstractDelta = TypeUtils::deabstractDeltaOfSize(registerSize);
//!
//! TypeId typeId = 'some type-id';
//!
//! // Normalize some typeId into a non-abstract typeId.
//! if (TypeUtils::isAbstract(typeId)) typeId += deabstractDelta;
//!
//! // The same, but by using TypeUtils::deabstract() function.
//! typeId = TypeUtils::deabstract(typeId, deabstractDelta);
//! ```
static ASMJIT_INLINE_NODEBUG constexpr uint32_t deabstractDeltaOfSize(uint32_t registerSize) noexcept {
  return registerSize >= 8 ? uint32_t(TypeId::kInt64) - uint32_t(TypeId::kIntPtr)
                           : uint32_t(TypeId::kInt32) - uint32_t(TypeId::kIntPtr);
}

//! Deabstracts a given `typeId` into a native type by using `deabstractDelta`, which was previously
//! calculated by calling \ref deabstractDeltaOfSize() with a target native register size.
static ASMJIT_INLINE_NODEBUG constexpr TypeId deabstract(TypeId typeId, uint32_t deabstractDelta) noexcept {
  return isAbstract(typeId) ? TypeId(uint32_t(typeId) + deabstractDelta) : typeId;
}

static ASMJIT_INLINE_NODEBUG constexpr TypeId scalarToVector(TypeId scalarTypeId, TypeId vecStartId) noexcept {
  return TypeId(uint32_t(vecStartId) + uint32_t(scalarTypeId) - uint32_t(TypeId::kInt8));
}

} // {TypeUtils}

//! Provides type identifiers that can be used in templates instead of native types.
namespace Type {

//! bool as C++ type-name.
struct Bool {};
//! int8_t as C++ type-name.
struct Int8 {};
//! uint8_t as C++ type-name.
struct UInt8 {};
//! int16_t as C++ type-name.
struct Int16 {};
//! uint16_t as C++ type-name.
struct UInt16 {};
//! int32_t as C++ type-name.
struct Int32 {};
//! uint32_t as C++ type-name.
struct UInt32 {};
//! int64_t as C++ type-name.
struct Int64 {};
//! uint64_t as C++ type-name.
struct UInt64 {};
//! intptr_t as C++ type-name.
struct IntPtr {};
//! uintptr_t as C++ type-name.
struct UIntPtr {};
//! float as C++ type-name.
struct Float32 {};
//! double as C++ type-name.
struct Float64 {};

} // {Type}

//! \cond
#define ASMJIT_DEFINE_TYPE_ID(T, TYPE_ID) \
namespace TypeUtils {                     \
  template<>                              \
  struct TypeIdOfT<T> {                   \
    enum : uint32_t {                     \
      kTypeId = uint32_t(TYPE_ID)         \
    };                                    \
  };                                      \
}

ASMJIT_DEFINE_TYPE_ID(void         , TypeId::kVoid);
ASMJIT_DEFINE_TYPE_ID(Type::Bool   , TypeId::kUInt8);
ASMJIT_DEFINE_TYPE_ID(Type::Int8   , TypeId::kInt8);
ASMJIT_DEFINE_TYPE_ID(Type::UInt8  , TypeId::kUInt8);
ASMJIT_DEFINE_TYPE_ID(Type::Int16  , TypeId::kInt16);
ASMJIT_DEFINE_TYPE_ID(Type::UInt16 , TypeId::kUInt16);
ASMJIT_DEFINE_TYPE_ID(Type::Int32  , TypeId::kInt32);
ASMJIT_DEFINE_TYPE_ID(Type::UInt32 , TypeId::kUInt32);
ASMJIT_DEFINE_TYPE_ID(Type::Int64  , TypeId::kInt64);
ASMJIT_DEFINE_TYPE_ID(Type::UInt64 , TypeId::kUInt64);
ASMJIT_DEFINE_TYPE_ID(Type::IntPtr , TypeId::kIntPtr);
ASMJIT_DEFINE_TYPE_ID(Type::UIntPtr, TypeId::kUIntPtr);
ASMJIT_DEFINE_TYPE_ID(Type::Float32, TypeId::kFloat32);
ASMJIT_DEFINE_TYPE_ID(Type::Float64, TypeId::kFloat64);
//! \endcond

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_TYPE_H_INCLUDED
