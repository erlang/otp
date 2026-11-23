// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_UJIT) && !defined(ASMJIT_NO_JIT)

#include <asmjit/ujit.h>
#include <asmjit/axl/algorithm.h>

#include <asmjit-testing/commons/asmjit_utils.h>
#include <asmjit-testing/commons/random.h>

#include <algorithm>
#include <cmath>
#include <limits>

ASMJIT_BEGIN_SUB_NAMESPACE(ujit::UniCompilerTests)

// ujit::UniCompiler - Tests - Constants
// =====================================

static constexpr uint64_t kRandomSeed = 0x1234u;
static constexpr uint32_t kTestIterCount = 1000u;

static ASMJIT_INLINE_CONSTEXPR uint32_t byte_width_from_vec_width(VecWidth vw) noexcept {
  return 16u << uint32_t(vw);
}

// ujit::UniCompiler - Tests - MulAdd
// ==================================

class FPUOps {
public:
  // In case we want to generate these functions (x86).
  JitRuntime rt;

  using UnaryOpF32 = float (ASMJIT_CDECL*)(float) noexcept;
  using BinaryOpF32 = float (ASMJIT_CDECL*)(float, float) noexcept;
  using FMAOpF32 = float (ASMJIT_CDECL*)(float, float, float) noexcept;

  using UnaryOpF64 = double (ASMJIT_CDECL*)(double) noexcept;
  using BinaryOpF64 = double (ASMJIT_CDECL*)(double, double) noexcept;
  using FMAOpF64 = double (ASMJIT_CDECL*)(double, double, double) noexcept;

  BinaryOpF32 add_f32 {};
  BinaryOpF32 sub_f32 {};
  BinaryOpF32 mul_f32 {};
  BinaryOpF32 div_f32 {};
  UnaryOpF32 sqrt_f32 {};
  FMAOpF32 madd_fma_f32 {};
  FMAOpF32 madd_nofma_f32 {};

  BinaryOpF64 add_f64 {};
  BinaryOpF64 sub_f64 {};
  BinaryOpF64 mul_f64 {};
  BinaryOpF64 div_f64 {};
  UnaryOpF64 sqrt_f64 {};
  FMAOpF64 madd_fma_f64 {};
  FMAOpF64 madd_nofma_f64 {};

  bool initialized {};
};

static FPUOps fpu_ops;

static void init_fpu_ops_from_libc() {
  fpu_ops.add_f32 = [](float a, float b) noexcept { return a + b; };
  fpu_ops.sub_f32 = [](float a, float b) noexcept { return a - b; };
  fpu_ops.mul_f32 = [](float a, float b) noexcept { return a * b; };
  fpu_ops.div_f32 = [](float a, float b) noexcept { return a / b; };
  fpu_ops.sqrt_f32 = [](float a) noexcept { return std::sqrt(a); };
  fpu_ops.madd_fma_f32 = [](float a, float b, float c) noexcept { return std::fma(a, b, c); };
  fpu_ops.madd_nofma_f32 = [](float a, float b, float c) noexcept { return fpu_ops.add_f32(fpu_ops.mul_f32(a, b), c); };

  fpu_ops.add_f64 = [](double a, double b) noexcept { return a + b; };
  fpu_ops.sub_f64 = [](double a, double b) noexcept { return a - b; };
  fpu_ops.mul_f64 = [](double a, double b) noexcept { return a * b; };
  fpu_ops.div_f64 = [](double a, double b) noexcept { return a / b; };
  fpu_ops.sqrt_f64 = [](double a) noexcept { return std::sqrt(a); };
  fpu_ops.madd_fma_f64 = [](double a, double b, double c) noexcept { return std::fma(a, b, c); };
  fpu_ops.madd_nofma_f64 = [](double a, double b, double c) noexcept { return fpu_ops.add_f64(fpu_ops.mul_f64(a, b), c); };
}

#if defined(ASMJIT_UJIT_X86)
template<typename Fn, typename T>
static Fn compile_fpu_unary_op(JitRuntime& rt, InstId inst_id) noexcept {
  CodeHolder code;
  EXPECT_OK(code.init(rt.environment()));

  x86::Compiler cc(&code);
  FuncNode* func_node = cc.add_func(FuncSignature::build<T, T>());
  x86::Vec a = cc.new_vec128("a");

  func_node->set_arg(0, a);

  EXPECT_OK(cc.emit(inst_id, a, a));
  EXPECT_OK(cc.ret(a));
  EXPECT_OK(cc.end_func());
  EXPECT_OK(cc.finalize());

  Fn fn;
  EXPECT_OK(rt.add(&fn, cc.code()));
  return fn;
};

template<typename Fn, typename T>
static Fn compile_fpu_binary_op(JitRuntime& rt, InstId inst_id) noexcept {
  CodeHolder code;
  EXPECT_OK(code.init(rt.environment()));

  x86::Compiler cc(&code);
  FuncNode* func_node = cc.add_func(FuncSignature::build<T, T, T>());

  x86::Vec a = cc.new_vec128("a");
  x86::Vec b = cc.new_vec128("b");

  func_node->set_arg(0, a);
  func_node->set_arg(1, b);
  EXPECT_OK(cc.emit(inst_id, a, b));
  EXPECT_OK(cc.ret(a));

  EXPECT_OK(cc.end_func());
  EXPECT_OK(cc.finalize());

  Fn fn;
  EXPECT_OK(rt.add(&fn, cc.code()));
  return fn;
};

template<typename Fn, typename T>
static Fn compile_fpu_fma_op(JitRuntime& rt, InstId inst_id) noexcept {
  CodeHolder code;
  EXPECT_OK(code.init(rt.environment()));

  x86::Compiler cc(&code);
  FuncNode* func_node = cc.add_func(FuncSignature::build<T, T, T, T>());

  x86::Vec a = cc.new_vec128("a");
  x86::Vec b = cc.new_vec128("b");
  x86::Vec c = cc.new_vec128("c");

  func_node->frame().set_avx_enabled();
  func_node->set_arg(0, a);
  func_node->set_arg(1, b);
  func_node->set_arg(2, c);
  EXPECT_OK(cc.emit(inst_id, a, b, c));
  EXPECT_OK(cc.ret(a));

  EXPECT_OK(cc.end_func());
  EXPECT_OK(cc.finalize());

  Fn fn;
  EXPECT_OK(rt.add(&fn, cc.code()));
  return fn;
};

static void init_fpu_ops() noexcept {
  if (!fpu_ops.initialized) {
    init_fpu_ops_from_libc();
    fpu_ops.initialized = true;

    fpu_ops.add_f32 = compile_fpu_binary_op<FPUOps::BinaryOpF32, float>(fpu_ops.rt, x86::Inst::kIdAddss);
    fpu_ops.sub_f32 = compile_fpu_binary_op<FPUOps::BinaryOpF32, float>(fpu_ops.rt, x86::Inst::kIdSubss);
    fpu_ops.mul_f32 = compile_fpu_binary_op<FPUOps::BinaryOpF32, float>(fpu_ops.rt, x86::Inst::kIdMulss);
    fpu_ops.div_f32 = compile_fpu_binary_op<FPUOps::BinaryOpF32, float>(fpu_ops.rt, x86::Inst::kIdDivss);
    fpu_ops.sqrt_f32 = compile_fpu_unary_op<FPUOps::UnaryOpF32, float>(fpu_ops.rt, x86::Inst::kIdSqrtss);
    fpu_ops.madd_fma_f32 = compile_fpu_fma_op<FPUOps::FMAOpF32, float>(fpu_ops.rt, x86::Inst::kIdVfmadd213ss);

    fpu_ops.add_f64 = compile_fpu_binary_op<FPUOps::BinaryOpF64, double>(fpu_ops.rt, x86::Inst::kIdAddsd);
    fpu_ops.sub_f64 = compile_fpu_binary_op<FPUOps::BinaryOpF64, double>(fpu_ops.rt, x86::Inst::kIdSubsd);
    fpu_ops.mul_f64 = compile_fpu_binary_op<FPUOps::BinaryOpF64, double>(fpu_ops.rt, x86::Inst::kIdMulsd);
    fpu_ops.div_f64 = compile_fpu_binary_op<FPUOps::BinaryOpF64, double>(fpu_ops.rt, x86::Inst::kIdDivsd);
    fpu_ops.sqrt_f64 = compile_fpu_unary_op<FPUOps::UnaryOpF64, double>(fpu_ops.rt, x86::Inst::kIdSqrtsd);
    fpu_ops.madd_fma_f64 = compile_fpu_fma_op<FPUOps::FMAOpF64, double>(fpu_ops.rt, x86::Inst::kIdVfmadd213sd);
  }
}
#else
static void init_fpu_ops() {
  if (!fpu_ops.initialized) {
    init_fpu_ops_from_libc();
    fpu_ops.initialized = true;
  }
}

#endif

static float fadd(float a, float b) noexcept { return fpu_ops.add_f32(a, b); }
static float fsub(float a, float b) noexcept { return fpu_ops.sub_f32(a, b); }
static float fmul(float a, float b) noexcept { return fpu_ops.mul_f32(a, b); }
static float fdiv(float a, float b) noexcept { return fpu_ops.div_f32(a, b); }
static float fsqrt(float a) noexcept { return fpu_ops.sqrt_f32(a); }
static float fmadd_fma_ref(float a, float b, float c) noexcept { return fpu_ops.madd_fma_f32(a, b, c); }
static float fmadd_nofma_ref(float a, float b, float c) noexcept { return fpu_ops.madd_nofma_f32(a, b, c); }

static double fadd(double a, double b) noexcept { return fpu_ops.add_f64(a, b); }
static double fsub(double a, double b) noexcept { return fpu_ops.sub_f64(a, b); }
static double fmul(double a, double b) noexcept { return fpu_ops.mul_f64(a, b); }
static double fdiv(double a, double b) noexcept { return fpu_ops.div_f64(a, b); }
static double fsqrt(double a) noexcept { return fpu_ops.sqrt_f64(a); }
static double fmadd_fma_ref(double a, double b, double c) noexcept { return fpu_ops.madd_fma_f64(a, b, c); }
static double fmadd_nofma_ref(double a, double b, double c) noexcept { return fpu_ops.madd_nofma_f64(a, b, c); }

static ASMJIT_INLINE float fsign(float a) noexcept { return std::bit_cast<float>(std::bit_cast<uint32_t>(a) & (uint32_t(1) << 31)); }
static ASMJIT_INLINE double fsign(double a) noexcept { return std::bit_cast<double>(std::bit_cast<uint64_t>(a) & (uint64_t(1) << 63)); }

static ASMJIT_INLINE float fxor(float a, float b) noexcept { return std::bit_cast<float>(std::bit_cast<uint32_t>(a) ^ std::bit_cast<uint32_t>(b)); }
static ASMJIT_INLINE double fxor(double a, double b) noexcept { return std::bit_cast<double>(std::bit_cast<uint64_t>(a) ^ std::bit_cast<uint64_t>(b)); }

// ujit::UniCompiler - Tests - Alias Mode
// ======================================

enum class Op2Variation : uint32_t {
  kNoAlias = 0,
  kSrc1Mem,
  kSrc1Dst,

  kMaxValue = kSrc1Mem
};

enum class Op3Variation : uint32_t {
  kNoAlias = 0,
  kSrc1Mem,
  kSrc1Dst,
  kSrc2Mem,
  kSrc2Dst,
  kSrc1MemSrc2Dst,
  kSrc1DstSrc2Mem,
  kSrc1IsSrc2,
  kSrc1DstSrc2Dst,

  kMaxValue = kSrc1DstSrc2Dst
};

static const char* op_variation_name(Op2Variation alias_mode) noexcept {
  switch (alias_mode) {
    case Op2Variation::kNoAlias: return "dst, src1";
    case Op2Variation::kSrc1Mem: return "dst, [src1]";
    case Op2Variation::kSrc1Dst: return "dst, dst";

    default:
      ASMJIT_NOT_REACHED();
  }
}

static const char* op_variation_name(Op3Variation alias_mode) noexcept {
  switch (alias_mode) {
    case Op3Variation::kNoAlias       : return "dst, src1, src2";
    case Op3Variation::kSrc1Mem       : return "dst, [src1], src2";
    case Op3Variation::kSrc1Dst       : return "dst, dst, src2";
    case Op3Variation::kSrc2Mem       : return "dst, src1, [src2]";
    case Op3Variation::kSrc2Dst       : return "dst, src1, dst";
    case Op3Variation::kSrc1MemSrc2Dst: return "dst, [src1], dst";
    case Op3Variation::kSrc1DstSrc2Mem: return "dst, dst, [src2]";
    case Op3Variation::kSrc1IsSrc2    : return "dst, src1src2, src1src2";

    default:
      ASMJIT_NOT_REACHED();
  }
}

// ujit::UniCompiler - Tests - Types
// =================================

struct Variation {
  uint32_t value;

  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator==(uint32_t v) const noexcept { return value == v; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator!=(uint32_t v) const noexcept { return value != v; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator<=(uint32_t v) const noexcept { return value <= v; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator< (uint32_t v) const noexcept { return value <  v; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator>=(uint32_t v) const noexcept { return value >= v; }
  [[nodiscard]] ASMJIT_INLINE_CONSTEXPR bool operator> (uint32_t v) const noexcept { return value >  v; }
};

// ujit::UniCompiler - Tests - JIT Function Prototypes
// ===================================================

typedef uint32_t (*TestCondRRFunc)(int32_t a, int32_t b);
typedef uint32_t (*TestCondRIFunc)(int32_t a);

typedef void (*TestMFunc)(void* ptr);
typedef uintptr_t (*TestRMFunc)(uintptr_t reg, void* ptr);
typedef void (*TestMRFunc)(void* ptr, uintptr_t reg);

typedef uint32_t (*TestRRFunc)(uint32_t a);
typedef uint32_t (*TestRRRFunc)(uint32_t a, uint32_t b);
typedef uint32_t (*TestRRIFunc)(uint32_t a);

typedef void (*TestVVFunc)(void* dst, const void* src);
typedef void (*TestVVVFunc)(void* dst, const void* src1, const void* src2);
typedef void (*TestVVVVFunc)(void* dst, const void* src1, const void* src2, const void* src3);

// ujit::UniCompiler - Tests - JIT Context Error Handler
// =====================================================

class TestErrorHandler : public ErrorHandler {
public:
  TestErrorHandler() noexcept {}
  ~TestErrorHandler() noexcept override {}

  void handle_error(Error err, const char* message, BaseEmitter* origin) override {
    axl::maybe_unused(origin);
    EXPECT_EQ(err, Error::kOk)
      .message("AsmJit Error: %s", message);
  }
};
// ujit::UniCompiler - Tests - JIT Context for Testing
// ===================================================

class JitContext {
public:
  JitRuntime rt;
  CpuFeatures features {};
  CpuHints cpu_hints {};

#if !defined(ASMJIT_NO_LOGGING)
  StringLogger logger;
#endif // !ASMJIT_NO_LOGGING

  TestErrorHandler eh;
  CodeHolder code;
  BackendCompiler cc;

  void prepare() noexcept {
    code.reset();
    code.init(rt.environment());
    code.set_error_handler(&eh);

#if !defined(ASMJIT_NO_LOGGING)
    logger.clear();
    code.set_logger(&logger);
#endif // !ASMJIT_NO_LOGGING

    code.attach(&cc);
    cc.add_diagnostic_options(DiagnosticOptions::kRAAnnotate);
    cc.add_diagnostic_options(DiagnosticOptions::kValidateAssembler);
    cc.add_diagnostic_options(DiagnosticOptions::kValidateIntermediate);
  }

  template<typename Fn>
  Fn finish() {
    Fn fn;
    EXPECT_OK(cc.finalize());
    EXPECT_OK(rt.add(&fn, &code));
    code.reset();
    return fn;
  }

#if !defined(ASMJIT_NO_LOGGING)
  const char* logger_content() const noexcept { return logger.data(); }
#else
  const char* logger_content() const noexcept { return "<ASMJIT_NO_LOGGING>"; }
#endif
};

// ujit::UniCompiler - Tests - Conditional Operations - Functions
// ==============================================================

static TestCondRRFunc create_func_cond_rr(JitContext& ctx, UniOpCond op, CondCode cond_code, uint32_t variation) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uint32_t, int32_t, int32_t>());
  EXPECT_NOT_NULL(node);

  Gp a = uc.new_gp32("a");
  Gp b = uc.new_gp32("b");
  Gp result = uc.new_gp32("result");

  node->set_arg(0, a);
  node->set_arg(1, b);

  switch (variation) {
    case 0: {
      // Test a conditional branch based on the given condition.
      Label done = uc.new_label();
      uc.mov(result, 1);
      uc.j(done, UniCondition(op, cond_code, a, b));
      uc.mov(result, 0);
      uc.bind(done);
      break;
    }

    case 1: {
      // Test a cmov functionality.
      Gp true_value = uc.new_gp32("true_value");
      uc.mov(result, 0);
      uc.mov(true_value, 1);
      uc.cmov(result, true_value, UniCondition(op, cond_code, a, b));
      break;
    }

    case 2: {
      // Test a select functionality.
      Gp false_value = uc.new_gp32("false_value");
      Gp true_value = uc.new_gp32("true_value");
      uc.mov(false_value, 0);
      uc.mov(true_value, 1);
      uc.select(result, true_value, false_value, UniCondition(op, cond_code, a, b));
      break;
    }
  }

  uc.ret(result);
  uc.end_func();

  return ctx.finish<TestCondRRFunc>();
}

static TestCondRIFunc create_func_cond_ri(JitContext& ctx, UniOpCond op, CondCode cond_code, Imm imm) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uint32_t, int32_t>());
  EXPECT_NOT_NULL(node);

  Gp a = uc.new_gp32("a");
  Gp result = uc.new_gp32("result");
  Label done = uc.new_label();

  node->set_arg(0, a);
  uc.mov(result, 1);
  uc.j(done, UniCondition(op, cond_code, a, imm));
  uc.mov(result, 0);
  uc.bind(done);
  uc.ret(result);

  uc.end_func();
  return ctx.finish<TestCondRIFunc>();
}

// ujit::UniCompiler - Tests - Conditional Operations - Runner
// ===========================================================

static ASMJIT_NOINLINE void test_conditional_op(JitContext& ctx, UniOpCond op, CondCode cond_code, int32_t a, int32_t b, bool expected) {
  for (uint32_t variation = 0; variation < 3; variation++) {
    TestCondRRFunc fn_rr = create_func_cond_rr(ctx, op, cond_code, variation);
    TestCondRIFunc fn_ri = create_func_cond_ri(ctx, op, cond_code, b);

    uint32_t observed = fn_rr(a, b);
    EXPECT_EQ(observed, uint32_t(expected))
      .message("Operation failed (RR):\n"
               "  Input #1: %d\n"
               "  Input #2: %d\n"
               "  Expected: %d\n"
               "  Observed: %d\n"
               "Assembly:\n%s",
               a,
               b,
               uint32_t(expected),
               observed,
               ctx.logger_content());

    observed = fn_ri(a);
    EXPECT_EQ(observed, uint32_t(expected))
      .message("Operation failed (RI):\n"
               "  Input #1: %d\n"
               "  Input #2: %d\n"
               "  Expected: %d\n"
               "  Observed: %d\n"
               "Assembly:\n%s",
               a,
               b,
               uint32_t(expected),
               observed,
               ctx.logger_content());

    ctx.rt.reset();
  }
}

static ASMJIT_NOINLINE void test_cond_ops(JitContext& ctx) {
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kEqual, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kEqual, 1, 1, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kEqual, 1, 2, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kEqual, 100, 31, false);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kNotEqual, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kNotEqual, 1, 1, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kNotEqual, 1, 2, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kNotEqual, 100, 31, true);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 1, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 111111, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 111111, 222, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 222, 111111, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGT, 222, 111, true);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 1, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 111111, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 111111, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 111111, 222, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedGE, 222, 111111, false);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 1, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 0, 1, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 111111, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 111111, 222, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 222, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLT, 222, 111, false);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 1, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 0, 1, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 111111, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 111111, 222, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 222, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kUnsignedLE, 22222, 22222, true);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, 1, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, 111111, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, 111111, -222, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, -222, 111111, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, -222, -111, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGT, -111, -1, false);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, 1, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, 111111, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, 111111, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, 111111, -222, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, -222, 111111, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, -111, -1, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedGE, -111, -111, true);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, 1, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, 111111, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, 111111, -222, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, -222, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, -222, -111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, -111, -1, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLT, -1, -1, false);

  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, 1, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, 111111, 0, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, 111111, -222, false);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, -222, 111111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, -222, -111, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, -111, -1, true);
  test_conditional_op(ctx, UniOpCond::kCompare, CondCode::kSignedLE, -1, -1, true);

  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kZero, 0, 0, true);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kZero, 1, 0, true);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kZero, 111111, 0, true);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kZero, 111111, -222, false);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kZero, -222, 111111, false);

  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kNotZero, 0, 0, false);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kNotZero, 1, 0, false);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kNotZero, 111111, 0, false);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kNotZero, 111111, -222, true);
  test_conditional_op(ctx, UniOpCond::kTest, CondCode::kNotZero, -222, 111111, true);

  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0x0), 0, true);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0x1), 0, false);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0xFF), 7, false);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0xFF), 9, true);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0xFFFFFFFF), 31, false);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTZero, int32_t(0x7FFFFFFF), 31, true);

  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0x0), 0, false);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0x1), 0, true);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0xFF), 7, true);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0xFF), 9, false);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0xFFFFFFFF), 31, true);
  test_conditional_op(ctx, UniOpCond::kBitTest, CondCode::kBTNotZero, int32_t(0x7FFFFFFF), 31, false);

  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0x00000001), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0x00000001), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAnd, CondCode::kNotZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0x00000001), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0x00000001), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignOr, CondCode::kNotZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0x00000001), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0x00000001), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignXor, CondCode::kNotZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0xFF000000), int32_t(0x01000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kCarry, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0xFF000000), int32_t(0x01000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotCarry, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0xFF000000), int32_t(0x01000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0x000000FF), int32_t(0x80000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0x000000FF), int32_t(0x800000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kSign, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0x000000FF), int32_t(0x80000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0x000000FF), int32_t(0x800000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignAdd, CondCode::kNotSign, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0xFF000000), int32_t(0x01000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotZero, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0xFF000000), int32_t(0x01000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0x000000FF), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedLT, int32_t(0x00000111), int32_t(0x0000F0FF), true);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0x000000FF), int32_t(0x000000FF), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGE, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0x00000000), int32_t(0xFFFFFFFF), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0x00000000), int32_t(0x00000001), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0x00000001), int32_t(0x00000010), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0xFFFFFFFF), int32_t(0xFF000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kSign, int32_t(0x7FFFFFFF), int32_t(0x80000000), true);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0x00000000), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0x00000000), int32_t(0xFFFFFFFF), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0x00000000), int32_t(0x00000001), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0x00000001), int32_t(0x00000010), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kNotSign, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);

  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0x00000000), int32_t(0x00000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0xFF000000), int32_t(0x01000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0x000000FF), int32_t(0x00000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0x000000FF), int32_t(0x000000FF), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0xFFFFFFFF), int32_t(0xFF000000), true);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0x7FFFFFFF), int32_t(0x80000000), false);
  test_conditional_op(ctx, UniOpCond::kAssignSub, CondCode::kUnsignedGT, int32_t(0x00000111), int32_t(0x0000F0FF), false);

  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kZero, int32_t(0x00000000), 1, true);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kZero, int32_t(0x000000FF), 8, true);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kZero, int32_t(0x000000FF), 7, false);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kZero, int32_t(0xFFFFFFFF), 31, false);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kZero, int32_t(0x7FFFFFFF), 31, true);

  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kNotZero, int32_t(0x00000000), 1, false);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kNotZero, int32_t(0x000000FF), 8, false);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kNotZero, int32_t(0x000000FF), 7, true);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kNotZero, int32_t(0xFFFFFFFF), 31, true);
  test_conditional_op(ctx, UniOpCond::kAssignShr, CondCode::kNotZero, int32_t(0x7FFFFFFF), 31, false);
}

// ujit::UniCompiler - Tests - M Operations - Functions
// ====================================================

static TestMFunc create_func_m(JitContext& ctx, UniOpM op) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*>());
  EXPECT_NOT_NULL(node);

  Gp ptr = uc.new_gpz("ptr");
  node->set_arg(0, ptr);
  uc.emit_m(op, mem_ptr(ptr));

  uc.end_func();
  return ctx.finish<TestMFunc>();
}

// ujit::UniCompiler - Tests - M Operations - Runner
// =================================================

static ASMJIT_NOINLINE void test_m_ops(JitContext& ctx) {
  uint8_t buffer[8];

  TestMFunc fn_zero_u8 = create_func_m(ctx, UniOpM::kStoreZeroU8);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u8(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0BCDEFGH", 8), 0);
  fn_zero_u8(buffer + 5);
  EXPECT_EQ(memcmp(buffer, "\0BCDE\0GH", 8), 0);

  TestMFunc fn_zero_u16 = create_func_m(ctx, UniOpM::kStoreZeroU16);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u16(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0CDEFGH", 8), 0);
  fn_zero_u16(buffer + 4);
  EXPECT_EQ(memcmp(buffer, "\0\0CD\0\0GH", 8), 0);

  TestMFunc fn_zero_u32 = create_func_m(ctx, UniOpM::kStoreZeroU32);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u32(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0EFGH", 8), 0);
  fn_zero_u32(buffer + 4);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);

#if ASMJIT_TARGET_ARCH_BITS >= 64
  TestMFunc fn_zero_u64 = create_func_m(ctx, UniOpM::kStoreZeroU64);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u64(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);
#endif

  TestMFunc fn_zero_reg = create_func_m(ctx, UniOpM::kStoreZeroReg);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_reg(buffer + 0);
#if ASMJIT_TARGET_ARCH_BITS >= 64
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);
#else
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0EFGH", 8), 0);
#endif

  ctx.rt.reset();
}

// ujit::UniCompiler - Tests - RM Operations - Functions
// =====================================================

static TestRMFunc create_func_rm(JitContext& ctx, UniOpRM op) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uintptr_t, uintptr_t, void*>());
  EXPECT_NOT_NULL(node);

  Gp reg = uc.new_gpz("reg");
  Gp ptr = uc.new_gpz("ptr");

  node->set_arg(0, reg);
  node->set_arg(1, ptr);

  uc.emit_rm(op, reg, mem_ptr(ptr));
  uc.ret(reg);

  uc.end_func();
  return ctx.finish<TestRMFunc>();
}

// ujit::UniCompiler - Tests - RM Operations - Runner
// ==================================================

static ASMJIT_NOINLINE void test_rm_ops(JitContext& ctx) {
  union Mem {
    uint8_t buffer[8];
    uint16_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
  };

  Mem mem{};

  TestRMFunc fn_load_i8 = create_func_rm(ctx, UniOpRM::kLoadI8);
  mem.u8 = uint8_t(int8_t(6));
  EXPECT_EQ(fn_load_i8(0, mem.buffer), uintptr_t(intptr_t(6)));

  mem.u8 = uint8_t(int8_t(-6));
  EXPECT_EQ(fn_load_i8(0, mem.buffer), uintptr_t(intptr_t(-6)));

  TestRMFunc fn_load_u8 = create_func_rm(ctx, UniOpRM::kLoadU8);
  mem.u8 = uint8_t(0x80);
  EXPECT_EQ(fn_load_u8(0, mem.buffer), 0x80u);

  mem.u8 = uint8_t(0xFF);
  EXPECT_EQ(fn_load_u8(0, mem.buffer), 0xFFu);

  TestRMFunc fn_load_i16 = create_func_rm(ctx, UniOpRM::kLoadI16);
  mem.u16 = uint16_t(int16_t(666));
  EXPECT_EQ(fn_load_i16(0, mem.buffer), uintptr_t(intptr_t(666)));

  mem.u16 = uint16_t(int16_t(-666));
  EXPECT_EQ(fn_load_i16(0, mem.buffer), uintptr_t(intptr_t(-666)));

  TestRMFunc fn_load_u16 = create_func_rm(ctx, UniOpRM::kLoadU16);
  mem.u16 = uint16_t(0x8000);
  EXPECT_EQ(fn_load_u16(0, mem.buffer), 0x8000u);

  mem.u16 = uint16_t(0xFEED);
  EXPECT_EQ(fn_load_u16(0, mem.buffer), 0xFEEDu);

  TestRMFunc fn_load_i32 = create_func_rm(ctx, UniOpRM::kLoadI32);
  mem.u32 = uint32_t(int32_t(666666));
  EXPECT_EQ(fn_load_i32(0, mem.buffer), uintptr_t(intptr_t(666666)));

  mem.u32 = uint32_t(int32_t(-666666));
  EXPECT_EQ(fn_load_i32(0, mem.buffer), uintptr_t(intptr_t(-666666)));

  TestRMFunc fn_load_u32 = create_func_rm(ctx, UniOpRM::kLoadU32);
  mem.u32 = 0x12345678;
  EXPECT_EQ(fn_load_u32(0, mem.buffer), uint32_t(0x12345678));

#if ASMJIT_TARGET_ARCH_BITS >= 64
  TestRMFunc fn_load_i64 = create_func_rm(ctx, UniOpRM::kLoadI64);
  mem.u64 = 0xF123456789ABCDEFu;
  EXPECT_EQ(fn_load_i64(0, mem.buffer), 0xF123456789ABCDEFu);

  TestRMFunc fn_load_u64 = create_func_rm(ctx, UniOpRM::kLoadU64);
  mem.u64 = 0xF123456789ABCDEFu;
  EXPECT_EQ(fn_load_u64(0, mem.buffer), 0xF123456789ABCDEFu);
#endif

  TestRMFunc fn_load_reg = create_func_rm(ctx, UniOpRM::kLoadReg);
  mem.u64 = 0xF123456789ABCDEFu;
#if ASMJIT_TARGET_ARCH_BITS >= 64
  EXPECT_EQ(fn_load_reg(0, mem.buffer), 0xF123456789ABCDEFu);
#else
  EXPECT_EQ(fn_load_reg(0, mem.buffer), 0x89ABCDEFu);
#endif

  TestRMFunc fn_load_merge_u8 = create_func_rm(ctx, UniOpRM::kLoadMergeU8);
  mem.u8 = uint8_t(0xAA);
  EXPECT_EQ(fn_load_merge_u8(0x1F2FFF00, mem.buffer), 0x1F2FFFAAu);

  TestRMFunc fn_load_shift_u8 = create_func_rm(ctx, UniOpRM::kLoadShiftU8);
  mem.u8 = uint8_t(0xAA);
  EXPECT_EQ(fn_load_shift_u8(0x002FFF00, mem.buffer), 0x2FFF00AAu);

  TestRMFunc fn_load_merge_u16 = create_func_rm(ctx, UniOpRM::kLoadMergeU16);
  mem.u16 = uint16_t(0xAABB);
  EXPECT_EQ(fn_load_merge_u16(0x1F2F0000, mem.buffer), 0x1F2FAABBu);

  TestRMFunc fn_load_shift_u16 = create_func_rm(ctx, UniOpRM::kLoadShiftU16);
  mem.u16 = uint16_t(0xAABB);
  EXPECT_EQ(fn_load_shift_u16(0x00001F2F, mem.buffer), 0x1F2FAABBu);

  ctx.rt.reset();
}

// ujit::UniCompiler - Tests - MR Operations - Functions
// =====================================================

static TestMRFunc create_func_mr(JitContext& ctx, UniOpMR op) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, uintptr_t>());
  EXPECT_NOT_NULL(node);

  Gp ptr = uc.new_gpz("ptr");
  Gp reg = uc.new_gpz("reg");

  node->set_arg(0, ptr);
  node->set_arg(1, reg);

  uc.emit_mr(op, mem_ptr(ptr), reg);

  uc.end_func();
  return ctx.finish<TestMRFunc>();
}

// ujit::UniCompiler - Tests - MR Operations - Runner
// ==================================================

static ASMJIT_NOINLINE void test_mr_ops(JitContext& ctx) {
  union Mem {
    uint8_t buffer[8];
    uint16_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
  };

  Mem mem{};

  TestMRFunc fn_store_u8 = create_func_mr(ctx, UniOpMR::kStoreU8);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u8(mem.buffer, 0x7A);
  EXPECT_EQ(memcmp(mem.buffer, "zBCDEFGH", 8), 0);

  TestMRFunc fn_store_u16 = create_func_mr(ctx, UniOpMR::kStoreU16);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u16(mem.buffer, 0x7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzCDEFGH", 8), 0);

  TestMRFunc fn_store_u32 = create_func_mr(ctx, UniOpMR::kStoreU32);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u32(mem.buffer, 0x7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzEFGH", 8), 0);

#if ASMJIT_TARGET_ARCH_BITS >= 64
  TestMRFunc fn_store_u64 = create_func_mr(ctx, UniOpMR::kStoreU64);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u64(mem.buffer, 0x7A7A7A7A7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzzzzz", 8), 0);
#endif

  TestMRFunc fn_store_reg = create_func_mr(ctx, UniOpMR::kStoreReg);
  memcpy(mem.buffer, "ABCDEFGH", 8);
#if ASMJIT_TARGET_ARCH_BITS >= 64
  fn_store_reg(mem.buffer, 0x7A7A7A7A7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzzzzz", 8), 0);
#else
  fn_store_reg(mem.buffer, 0x7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzEFGH", 8), 0);
#endif

  TestMRFunc fn_add_u8 = create_func_mr(ctx, UniOpMR::kAddU8);
  mem.u64 = 0;
  mem.u8 = 42;
  fn_add_u8(mem.buffer, 13);
  EXPECT_EQ(mem.u8, 55u);
  EXPECT_EQ(memcmp(mem.buffer + 1, "\0\0\0\0\0\0\0", 7), 0);

  TestMRFunc fn_add_u16 = create_func_mr(ctx, UniOpMR::kAddU16);
  mem.u64 = 0;
  mem.u16 = 442;
  fn_add_u16(mem.buffer, 335);
  EXPECT_EQ(mem.u16, 777u);
  EXPECT_EQ(memcmp(mem.buffer + 2, "\0\0\0\0\0\0", 6), 0);

  TestMRFunc fn_add_u32 = create_func_mr(ctx, UniOpMR::kAddU32);
  mem.u64 = 0;
  mem.u32 = 442332;
  fn_add_u32(mem.buffer, 335223);
  EXPECT_EQ(mem.u32, 777555u);
  EXPECT_EQ(memcmp(mem.buffer + 4, "\0\0\0\0", 4), 0);

#if ASMJIT_TARGET_ARCH_BITS >= 64
  TestMRFunc fn_add_u64 = create_func_mr(ctx, UniOpMR::kAddU64);
  mem.u64 = 0xF123456789ABCDEFu;
  fn_add_u64(mem.buffer, 0x0102030405060708u);
  EXPECT_EQ(mem.u64, 0xF225486B8EB1D4F7u);
#endif

  TestMRFunc fn_add_reg = create_func_mr(ctx, UniOpMR::kAddReg);
  mem.u64 = 0xFFFFFFFFFFFFFFFF;
#if ASMJIT_TARGET_ARCH_BITS >= 64
  fn_add_reg(mem.buffer, 1);
  EXPECT_EQ(mem.u64, 0u);
#else
  mem.u32 = 0x01020304;
  fn_add_reg(mem.buffer, 0x02030405);
  EXPECT_EQ(mem.u32, 0x03050709u);
  EXPECT_EQ(memcmp(mem.buffer + 4, "\xFF\xFF\xFF\xFF", 4), 0);
#endif

  ctx.rt.reset();
}

// ujit::UniCompiler - Tests - RR - Metadata
// ==========================================

static const char* rr_op_name(UniOpRR op) noexcept {
  switch (op) {
    case UniOpRR::kAbs:     return "abs";
    case UniOpRR::kNeg:     return "neg";
    case UniOpRR::kNot:     return "not";
    case UniOpRR::kBSwap:   return "bswap";
    case UniOpRR::kCLZ:     return "clz";
    case UniOpRR::kCTZ:     return "ctz";
    case UniOpRR::kReflect: return "reflect";

    default:
      ASMJIT_NOT_REACHED();
  }
}

// ujit::UniCompiler - Tests - RR Operations - Functions
// =====================================================

static TestRRFunc create_func_rr(JitContext& ctx, UniOpRR op, Op2Variation variation) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  Gp a = uc.new_gp32("a");
  Gp result;

  node->set_arg(0, a);

  switch (variation) {
    case Op2Variation::kNoAlias: {
      result = uc.new_gp32("result");
      uc.emit_2i(op, result, a);
      break;
    }

    case Op2Variation::kSrc1Mem: {
      Mem m = uc.new_stack(a.size(), a.size(), "a_stack");

      result = uc.new_gp32("result");
      uc.store(m, a);
      uc.emit_2i(op, result, m);
      break;
    }

    case Op2Variation::kSrc1Dst: {
      result = a;
      uc.emit_2i(op, result, a);
      break;
    }

    default: {
      ASMJIT_NOT_REACHED();
    }
  }

  uc.ret(result);
  uc.end_func();

  return ctx.finish<TestRRFunc>();
}

// ujit::UniCompiler - Tests - RR Operations - Runner
// ==================================================

static ASMJIT_NOINLINE void test_rr_op(JitContext& ctx, UniOpRR op, uint32_t a, uint32_t expected) {
  for (Op2Variation variation : axl::enumerate(Op2Variation::kMaxValue)) {
    TestRRFunc fn_rr = create_func_rr(ctx, op, variation);
    uint32_t observed = fn_rr(a);
    EXPECT_EQ(observed, expected)
      .message("Operation '%s %s' failed:\n"
               "  Input #1: %d\n"
               "  Expected: %d\n"
               "  Observed: %d\n"
               "Assembly:\n%s",
               rr_op_name(op),
               op_variation_name(variation),
               a,
               uint32_t(expected),
               observed,
               ctx.logger_content());
    ctx.rt.reset();
  }
}

static ASMJIT_NOINLINE void test_rr_ops(JitContext& ctx) {
  test_rr_op(ctx, UniOpRR::kAbs, 0u, 0u);
  test_rr_op(ctx, UniOpRR::kAbs, 1u, 1u);
  test_rr_op(ctx, UniOpRR::kAbs, uint32_t(-1), 1u);
  test_rr_op(ctx, UniOpRR::kAbs, uint32_t(-333), 333u);
  test_rr_op(ctx, UniOpRR::kAbs, 0x80000000u, 0x80000000u);

  test_rr_op(ctx, UniOpRR::kNeg, 0u, 0u);
  test_rr_op(ctx, UniOpRR::kNeg, 1u, uint32_t(-1));
  test_rr_op(ctx, UniOpRR::kNeg, uint32_t(-1), 1u);
  test_rr_op(ctx, UniOpRR::kNeg, uint32_t(-333), 333u);
  test_rr_op(ctx, UniOpRR::kNeg, 333u, uint32_t(-333));
  test_rr_op(ctx, UniOpRR::kNeg, 0x80000000u, 0x80000000u);

  test_rr_op(ctx, UniOpRR::kNot, 0u, 0xFFFFFFFFu);
  test_rr_op(ctx, UniOpRR::kNot, 1u, 0xFFFFFFFEu);
  test_rr_op(ctx, UniOpRR::kNot, 0xFFFFFFFF, 0u);
  test_rr_op(ctx, UniOpRR::kNot, 0x12333245, ~0x12333245u);
  test_rr_op(ctx, UniOpRR::kNot, 0x80000000u, 0x7FFFFFFFu);

  test_rr_op(ctx, UniOpRR::kBSwap, 0x11223344u, 0x44332211u);
  test_rr_op(ctx, UniOpRR::kBSwap, 0xFFFF0000u, 0x0000FFFFu);
  test_rr_op(ctx, UniOpRR::kBSwap, 0x00000000u, 0x00000000u);

  test_rr_op(ctx, UniOpRR::kCLZ, 0x80000000u, 0u);
  test_rr_op(ctx, UniOpRR::kCLZ, 0x40000000u, 1u);
  test_rr_op(ctx, UniOpRR::kCLZ, 0x00800000u, 8u);
  test_rr_op(ctx, UniOpRR::kCLZ, 0x00008000u, 16u);
  test_rr_op(ctx, UniOpRR::kCLZ, 0x00000080u, 24u);
  test_rr_op(ctx, UniOpRR::kCLZ, 0x00000001u, 31u);

  test_rr_op(ctx, UniOpRR::kCTZ, 0x80000000u, 31u);
  test_rr_op(ctx, UniOpRR::kCTZ, 0x40000000u, 30u);
  test_rr_op(ctx, UniOpRR::kCTZ, 0x00800000u, 23u);
  test_rr_op(ctx, UniOpRR::kCTZ, 0x00008000u, 15u);
  test_rr_op(ctx, UniOpRR::kCTZ, 0x00000080u, 7u);
  test_rr_op(ctx, UniOpRR::kCTZ, 0x00000001u, 0u);

  test_rr_op(ctx, UniOpRR::kReflect, 0x00000000u, 0x00000000u);
  test_rr_op(ctx, UniOpRR::kReflect, 0x00FF0000u, 0x00FF0000u);
  test_rr_op(ctx, UniOpRR::kReflect, 0x000000FFu, 0x000000FFu);
  test_rr_op(ctx, UniOpRR::kReflect, 0x80000000u, 0x7FFFFFFFu);
  test_rr_op(ctx, UniOpRR::kReflect, 0xFFFFFFFFu, 0x00000000u);
  test_rr_op(ctx, UniOpRR::kReflect, 0x88FF0000u, 0x7700FFFFu);
}

// ujit::UniCompiler - Tests - RRR - Metadata
// ==========================================

static const char* rrr_op_name(UniOpRRR op) noexcept {
  switch (op) {
    case UniOpRRR::kAnd:    return "and";
    case UniOpRRR::kOr:     return "or";
    case UniOpRRR::kXor:    return "xor";
    case UniOpRRR::kBic:    return "bic";
    case UniOpRRR::kAdd:    return "add";
    case UniOpRRR::kSub:    return "sub";
    case UniOpRRR::kMul:    return "mul";
    case UniOpRRR::kUDiv:   return "udiv";
    case UniOpRRR::kUMod:   return "umod";
    case UniOpRRR::kSMin:   return "smin";
    case UniOpRRR::kSMax:   return "smax";
    case UniOpRRR::kUMin:   return "umin";
    case UniOpRRR::kUMax:   return "umax";
    case UniOpRRR::kSll:    return "sll";
    case UniOpRRR::kSrl:    return "srl";
    case UniOpRRR::kSra:    return "sra";
    case UniOpRRR::kRol:    return "rol";
    case UniOpRRR::kRor:    return "ror";
    case UniOpRRR::kSBound: return "sbound";

    default:
      ASMJIT_NOT_REACHED();
  }
}

// ujit::UniCompiler - Tests - RRR Operations - Functions
// ======================================================

static TestRRRFunc create_func_rrr(JitContext& ctx, UniOpRRR op, Op3Variation variation) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uint32_t, uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  Gp a = uc.new_gp32("a");
  Gp b = uc.new_gp32("b");
  Gp result;

  switch (variation) {
    case Op3Variation::kNoAlias: {
      result = uc.new_gp32("result");
      node->set_arg(0, a);
      node->set_arg(1, b);
      uc.emit_3i(op, result, a, b);
      break;
    }

    case Op3Variation::kSrc1Dst:
    case Op3Variation::kSrc2Dst: {
      result = (variation == Op3Variation::kSrc1Dst) ? a : b;
      node->set_arg(0, a);
      node->set_arg(1, b);
      uc.emit_3i(op, result, a, b);
      break;
    }

    case Op3Variation::kSrc1Mem:
    case Op3Variation::kSrc1MemSrc2Dst: {
      Mem m = uc.new_stack(a.size(), a.size(), "a_stack");
      result = (variation == Op3Variation::kSrc1Mem) ? uc.new_gp32("result") : b;

      node->set_arg(0, a);
      node->set_arg(1, b);

      uc.store(m, a);
      uc.emit_3i(op, result, m, b);
      break;
    }

    case Op3Variation::kSrc2Mem:
    case Op3Variation::kSrc1DstSrc2Mem: {
      Mem m = uc.new_stack(b.size(), b.size(), "b_stack");
      result = (variation == Op3Variation::kSrc2Mem) ? uc.new_gp32("result") : a;

      node->set_arg(0, a);
      node->set_arg(1, b);

      uc.store(m, b);
      uc.emit_3i(op, result, a, m);
      break;
    }

    case Op3Variation::kSrc1IsSrc2: {
      result = uc.new_gp32("result");
      node->set_arg(0, a);
      uc.emit_3i(op, result, a, a);
      break;
    }

    case Op3Variation::kSrc1DstSrc2Dst: {
      result = a;
      node->set_arg(0, a);
      uc.emit_3i(op, result, a, a);
      break;
    }

    default: {
      ASMJIT_NOT_REACHED();
    }
  }

  uc.ret(result);
  uc.end_func();

  return ctx.finish<TestRRRFunc>();
}

static TestRRIFunc create_func_rri(JitContext& ctx, UniOpRRR op, Op3Variation variation, Imm imm) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(VecWidth::k128);

  FuncNode* node = uc.add_func(FuncSignature::build<uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  Gp a = uc.new_gp32("a");
  Gp result;

  switch (variation) {
    case Op3Variation::kNoAlias: {
      result = uc.new_gp32("result");
      node->set_arg(0, a);
      uc.emit_3i(op, result, a, imm);
      break;
    }

    case Op3Variation::kSrc1Dst: {
      result = a;
      node->set_arg(0, a);
      uc.emit_3i(op, result, a, imm);
      break;
    }

    case Op3Variation::kSrc1Mem: {
      Mem m = uc.new_stack(a.size(), a.size(), "a_stack");

      result = uc.new_gp32("result");
      node->set_arg(0, a);

      uc.store(m, a);
      uc.emit_3i(op, result, m, imm);
      break;
    }

    default:
      ASMJIT_NOT_REACHED();
  }

  uc.ret(result);
  uc.end_func();

  return ctx.finish<TestRRIFunc>();
}

// ujit::UniCompiler - Tests - RRR Operations - Runner
// ===================================================

static ASMJIT_NOINLINE void test_rrr_op(JitContext& ctx, UniOpRRR op, uint32_t a, uint32_t b, uint32_t expected) {
  for (Op3Variation variation : axl::enumerate(a == b ? Op3Variation::kMaxValue : Op3Variation::kSrc1DstSrc2Mem)) {
    TestRRRFunc fn_rrr = create_func_rrr(ctx, op, variation);
    uint32_t observed = fn_rrr(a, b);
    EXPECT_EQ(observed, expected)
      .message("Operation '%s %s' failed:\n"
               "  Input #1: %d\n"
               "  Input #2: %d\n"
               "  Expected: %d\n"
               "  Observed: %d\n"
               "Assembly:\n%s",
               rrr_op_name(op),
               op_variation_name(variation),
               a,
               b,
               uint32_t(expected),
               observed,
               ctx.logger_content());
    ctx.rt.reset();
  }

  for (Op3Variation alias_mode : axl::enumerate(Op3Variation::kSrc1Dst)) {
    TestRRIFunc fn_rri = create_func_rri(ctx, op, alias_mode, Imm(b));
    uint32_t observed = fn_rri(a);
    EXPECT_EQ(observed, expected)
      .message("Operation '%s %s@imm' failed:\n"
               "  Input #1: %d\n"
               "  Input #2: %d\n"
               "  Expected: %d\n"
               "  Observed: %d\n"
               "Assembly:\n%s",
               rrr_op_name(op),
               op_variation_name(alias_mode),
               a,
               b,
               uint32_t(expected),
               observed,
               ctx.logger_content());

    ctx.rt.reset();
  }
}

static ASMJIT_NOINLINE void test_rrr_ops(JitContext& ctx) {
  test_rrr_op(ctx, UniOpRRR::kAnd, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0xFFu, 0x11u, 0x11u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0x11u, 0xFFu, 0x11u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0xFFu, 0xFFu, 0xFFu);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0xFF11u, 0x1111u, 0x1111u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0x1111u, 0xFF11u, 0x1111u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0x0000FFFFu, 0xFFFF0000u, 0u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0xFFFFFFFFu, 0xFFFF0000u, 0xFFFF0000u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0x11111111u, 0x11223344u, 0x11001100u);
  test_rrr_op(ctx, UniOpRRR::kAnd, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, UniOpRRR::kOr, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kOr, 0xFFu, 0x11u, 0xFFu);
  test_rrr_op(ctx, UniOpRRR::kOr, 0x11u, 0xFFu, 0xFFu);
  test_rrr_op(ctx, UniOpRRR::kOr, 0xFFu, 0xFFu, 0xFFu);
  test_rrr_op(ctx, UniOpRRR::kOr, 0xFF11u, 0x1111u, 0xFF11u);
  test_rrr_op(ctx, UniOpRRR::kOr, 0x1111u, 0xFF11u, 0xFF11u);
  test_rrr_op(ctx, UniOpRRR::kOr, 0x0000FFFFu, 0xFFFF0001u, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kOr, 0xFFFFFFFFu, 0xFF000000u, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kOr, 0x11111111u, 0x00223344u, 0x11333355u);
  test_rrr_op(ctx, UniOpRRR::kOr, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, UniOpRRR::kXor, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kXor, 0xFFu, 0x11u, 0xEEu);
  test_rrr_op(ctx, UniOpRRR::kXor, 0x11u, 0xFFu, 0xEEu);
  test_rrr_op(ctx, UniOpRRR::kXor, 0xFFu, 0xFFu, 0x00u);
  test_rrr_op(ctx, UniOpRRR::kXor, 0xFF11u, 0x1111u, 0xEE00u);
  test_rrr_op(ctx, UniOpRRR::kXor, 0x1111u, 0xFF11u, 0xEE00u);
  test_rrr_op(ctx, UniOpRRR::kXor, 0x0000FFFFu, 0xFFFF0001u, 0xFFFFFFFEu);
  test_rrr_op(ctx, UniOpRRR::kXor, 0xFFFFFFFFu, 0xFF000000u, 0x00FFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kXor, 0x11111111u, 0x00223344u, 0x11332255u);
  test_rrr_op(ctx, UniOpRRR::kXor, 0xFFFFFFFFu, 0xFFFFFFFFu, 0x00000000u);

  test_rrr_op(ctx, UniOpRRR::kBic, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0xFFu, 0x11u, 0xEEu);
  test_rrr_op(ctx, UniOpRRR::kBic, 0x11u, 0xFFu, 0x00u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0xFFu, 0xFFu, 0x00u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0xFF11u, 0x1111u, 0xEE00u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0x1111u, 0xFF11u, 0x0000u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0x0000FFFFu, 0xFFFF0000u, 0x0000FFFFu);
  test_rrr_op(ctx, UniOpRRR::kBic, 0xFFFFFFFFu, 0xFFFF0000u, 0x0000FFFFu);
  test_rrr_op(ctx, UniOpRRR::kBic, 0x11111111u, 0x11223344u, 0x00110011u);
  test_rrr_op(ctx, UniOpRRR::kBic, 0xFFFFFFFFu, 0xFFFFFFFFu, 0x00000000u);

  test_rrr_op(ctx, UniOpRRR::kAdd, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 1u, 1u, 2u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 1u, 2u, 3u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 1u, 0xFFFu, 0x1000u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 1u, 0xFFF000u, 0xFFF001u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 0xFF000000u, 0x00FFFFFFu, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kAdd, 0xFFFFFFFFu, 0x00000001u, 0x00000000u);
  test_rrr_op(ctx, UniOpRRR::kAdd, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFEu);

  test_rrr_op(ctx, UniOpRRR::kSub, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSub, 1u, 1u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSub, 1u, 2u, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kSub, 0xFFFFFFFFu, 0x80000000u, 0x7FFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kSub, 0xFFFFFFFFu, 0xFFFFFFFFu, 0x00000000u);

  test_rrr_op(ctx, UniOpRRR::kMul, 0u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kMul, 0u, 1u, 0u);
  test_rrr_op(ctx, UniOpRRR::kMul, 1u, 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kMul, 1u, 1u, 1u);
  test_rrr_op(ctx, UniOpRRR::kMul, 13u, 13u, 0xA9u);
  test_rrr_op(ctx, UniOpRRR::kMul, 1000u, 999u, 999000u);
  test_rrr_op(ctx, UniOpRRR::kMul, 0x0000FFFFu, 0x00010001u, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kMul, 0x0000FFFFu, 0x0000FFFFu, 0xFFFE0001u);
  test_rrr_op(ctx, UniOpRRR::kMul, 0xFFFFFFFFu, 0xFFFFFFFFu, 0x00000001u);

  test_rrr_op(ctx, UniOpRRR::kUDiv, 0u, 1u, 0u);
  test_rrr_op(ctx, UniOpRRR::kUDiv, 1u, 1u, 1u);
  test_rrr_op(ctx, UniOpRRR::kUDiv, 1000u, 1000u, 1u);
  test_rrr_op(ctx, UniOpRRR::kUDiv, 100000u, 1000u, 100u);

  test_rrr_op(ctx, UniOpRRR::kUMod, 1999u, 1000u, 999u);
  test_rrr_op(ctx, UniOpRRR::kUMod, 2000u, 2000u, 0u);

  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(1111), uint32_t(0), uint32_t(0));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-1111), uint32_t(0), uint32_t(-1111));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(1), uint32_t(22), uint32_t(1));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(1), uint32_t(0), uint32_t(0));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(1000), uint32_t(1000), uint32_t(1000));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(100101033), uint32_t(999), uint32_t(999));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(100101033), uint32_t(112), uint32_t(112));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(112), uint32_t(1125532), uint32_t(112));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(1111), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-1111), uint32_t(-1), uint32_t(-1111));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-1), uint32_t(-22), uint32_t(-22));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-1), uint32_t(-128), uint32_t(-128));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-128), uint32_t(-1), uint32_t(-128));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(-128), uint32_t(9), uint32_t(-128));
  test_rrr_op(ctx, UniOpRRR::kSMin, uint32_t(12444), uint32_t(-1), uint32_t(-1));

  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(1), uint32_t(22), uint32_t(22));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(1), uint32_t(0), uint32_t(1));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(1000), uint32_t(1000), uint32_t(1000));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(100101033), uint32_t(999), uint32_t(100101033));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(100101033), uint32_t(112), uint32_t(100101033));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(112), uint32_t(1125532), uint32_t(1125532));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(1111), uint32_t(-1), uint32_t(1111));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(-1111), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(-1), uint32_t(-22), uint32_t(-1));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(-1), uint32_t(-128), uint32_t(-1));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(-128), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(-128), uint32_t(9), uint32_t(9));
  test_rrr_op(ctx, UniOpRRR::kSMax, uint32_t(12444), uint32_t(-1), uint32_t(12444));

  test_rrr_op(ctx, UniOpRRR::kUMin, 1, 22, 1);
  test_rrr_op(ctx, UniOpRRR::kUMin, 22, 1, 1);
  test_rrr_op(ctx, UniOpRRR::kUMin, 1, 255, 1);
  test_rrr_op(ctx, UniOpRRR::kUMin, 255, 1, 1);
  test_rrr_op(ctx, UniOpRRR::kUMin, 1000, 1000, 1000);
  test_rrr_op(ctx, UniOpRRR::kUMin, 1023, 255, 255);
  test_rrr_op(ctx, UniOpRRR::kUMin, 255, 1023, 255);
  test_rrr_op(ctx, UniOpRRR::kUMin, 0xFFFFFFFFu, 255, 255);
  test_rrr_op(ctx, UniOpRRR::kUMin, 255, 0xFFFFFFFFu, 255);
  test_rrr_op(ctx, UniOpRRR::kUMin, 0xFFFFFFFFu, 0xFFFFFF00u, 0xFFFFFF00u);
  test_rrr_op(ctx, UniOpRRR::kUMin, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, UniOpRRR::kUMax, 1, 22, 22);
  test_rrr_op(ctx, UniOpRRR::kUMax, 22, 1, 22);
  test_rrr_op(ctx, UniOpRRR::kUMax, 1, 255, 255);
  test_rrr_op(ctx, UniOpRRR::kUMax, 255, 1, 255);
  test_rrr_op(ctx, UniOpRRR::kUMax, 1000, 1000, 1000);
  test_rrr_op(ctx, UniOpRRR::kUMax, 1023, 255, 1023);
  test_rrr_op(ctx, UniOpRRR::kUMax, 255, 1023, 1023);
  test_rrr_op(ctx, UniOpRRR::kUMax, 0xFFFFFFFFu, 255, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kUMax, 255, 0xFFFFFFFFu, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kUMax, 0xFFFFFFFFu, 0xFFFFFF00u, 0xFFFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kUMax, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, UniOpRRR::kSll, 1u, 1u, 1u << 1);
  test_rrr_op(ctx, UniOpRRR::kSll, 2u, 2u, 2u << 2);
  test_rrr_op(ctx, UniOpRRR::kSll, 1u, 22u, 1u << 22);
  test_rrr_op(ctx, UniOpRRR::kSll, 1u, 31u, 1u << 31);
  test_rrr_op(ctx, UniOpRRR::kSll, 1000u, 2u, 4000u);
  test_rrr_op(ctx, UniOpRRR::kSll, 0x7FFFFFFFu, 1u, 0xFFFFFFFEu);

  test_rrr_op(ctx, UniOpRRR::kSrl, 1u, 1u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSrl, 2u, 2u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSrl, 1u, 22u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSrl, 1u, 31u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSrl, 1000u, 2u, 250u);
  test_rrr_op(ctx, UniOpRRR::kSrl, 0x7FFFFFFFu, 1u, 0x7FFFFFFFu >> 1);

  test_rrr_op(ctx, UniOpRRR::kSra, 1u, 1u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSra, 2u, 2u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSra, 1u, 22u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSra, 1u, 31u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSra, 0x7FFFFFFFu, 1u, 0x7FFFFFFFu >> 1);
  test_rrr_op(ctx, UniOpRRR::kSra, 0xF0000000u, 4u, 0xFF000000u);
  test_rrr_op(ctx, UniOpRRR::kSra, 0x80000000u, 0u, 0x80000000u);
  test_rrr_op(ctx, UniOpRRR::kSra, 0x80000000u, 31u, 0xFFFFFFFFu);

  test_rrr_op(ctx, UniOpRRR::kRol, 0x00000008u, 8u, 0x00000800u);
  test_rrr_op(ctx, UniOpRRR::kRol, 0x11223344u, 0u, 0x11223344u);
  test_rrr_op(ctx, UniOpRRR::kRol, 0x11223344u, 8u, 0x22334411u);
  test_rrr_op(ctx, UniOpRRR::kRol, 0x11223344u, 16u, 0x33441122u);
  test_rrr_op(ctx, UniOpRRR::kRol, 0xFCFFDABBu, 1u, 0xF9FFB577u);

  test_rrr_op(ctx, UniOpRRR::kRor, 0x00000008u, 8u, 0x08000000u);
  test_rrr_op(ctx, UniOpRRR::kRor, 0x11223344u, 0u, 0x11223344u);
  test_rrr_op(ctx, UniOpRRR::kRor, 0x11223344u, 8u, 0x44112233u);
  test_rrr_op(ctx, UniOpRRR::kRor, 0x11223344u, 16u, 0x33441122u);
  test_rrr_op(ctx, UniOpRRR::kRor, 0xF0000000u, 1u, 0x78000000u);

  test_rrr_op(ctx, UniOpRRR::kSBound, 0, 244u, 0);
  test_rrr_op(ctx, UniOpRRR::kSBound, 42, 244u, 42u);
  test_rrr_op(ctx, UniOpRRR::kSBound, 244, 244u, 244u);
  test_rrr_op(ctx, UniOpRRR::kSBound, 1111, 244u, 244u);
  test_rrr_op(ctx, UniOpRRR::kSBound, 9999999, 111244u, 111244u);
  test_rrr_op(ctx, UniOpRRR::kSBound, 0x7FFFFFFF, 0x7FFFFFFFu, 0x7FFFFFFFu);
  test_rrr_op(ctx, UniOpRRR::kSBound, uint32_t(int32_t(-1)), 1000u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSBound, uint32_t(INT32_MIN), 100000u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSBound, uint32_t(INT32_MAX), 0u, 0u);
  test_rrr_op(ctx, UniOpRRR::kSBound, uint32_t(INT32_MAX), 100000u, 100000u);
  test_rrr_op(ctx, UniOpRRR::kSBound, uint32_t(INT32_MAX), uint32_t(INT32_MAX), uint32_t(INT32_MAX));
}

// ujit::UniCompiler - Tests - SIMD - Functions
// ============================================

// The following variations are supported:
//   - 0 - separate destination & source registers
//   - 1 - destination register is a source register as well
//   - 2 - source is a memory operand
//   - 3 - source register is a GP register (only for broadcasts from a GP register, otherwise maps to 0)
static constexpr uint32_t kNumVariationsVV = 3;
static constexpr uint32_t kNumVariationsVV_Broadcast = 4;

static TestVVFunc create_func_vv(JitContext& ctx, VecWidth vw, UniOpVV op, Variation variation = Variation{0}) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(vw);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, const void*>());
  EXPECT_NOT_NULL(node);

  Gp dst_ptr = uc.new_gpz("dst_ptr");
  Gp src_ptr = uc.new_gpz("src_ptr");

  node->set_arg(0, dst_ptr);
  node->set_arg(1, src_ptr);

  Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");

  // There are some instructions that fill the high part of the register, so just zero the destination to make
  // sure that we can test this function (that the low part is actually zeroed and doesn't contain garbage).
  uc.v_zero_i(dst_vec);

  if (variation == 3u && (op == UniOpVV::kBroadcastU8  ||
                          op == UniOpVV::kBroadcastU8Z ||
                          op == UniOpVV::kBroadcastU32 ||
                          op == UniOpVV::kBroadcastU64 ||
                          op == UniOpVV::kBroadcastF32 ||
                          op == UniOpVV::kBroadcastF64)) {
    // This is used to test broadcasts from a GP register to a vector register.
    Gp src_gp = uc.new_gpz("src_gp");

    switch (op) {
      case UniOpVV::kBroadcastU8:
      case UniOpVV::kBroadcastU8Z:
        uc.load_u8(src_gp, mem_ptr(src_ptr));
        uc.emit_2v(op, dst_vec, src_gp);
        break;

      case UniOpVV::kBroadcastU16:
      case UniOpVV::kBroadcastU16Z:
        uc.load_u16(src_gp, mem_ptr(src_ptr));
        uc.emit_2v(op, dst_vec, src_gp);
        break;

      case UniOpVV::kBroadcastU32:
      case UniOpVV::kBroadcastF32:
        uc.load_u32(src_gp, mem_ptr(src_ptr));
        uc.emit_2v(op, dst_vec, src_gp);
        break;

      case UniOpVV::kBroadcastU64:
      case UniOpVV::kBroadcastF64:
        // Prevent using 64-bit registers on 32-bit architectures (that would fail).
        if (uc.is_64bit()) {
          uc.load_u64(src_gp, mem_ptr(src_ptr));
          uc.emit_2v(op, dst_vec, src_gp);
        }
        else {
          uc.emit_2v(op, dst_vec, mem_ptr(src_ptr));
        }
        break;

      default:
        ASMJIT_NOT_REACHED();
    }
  }
  else if (variation == 2u) {
    uc.emit_2v(op, dst_vec, mem_ptr(src_ptr));
  }
  else if (variation == 1u) {
    uc.v_loaduvec(dst_vec, mem_ptr(src_ptr));
    uc.emit_2v(op, dst_vec, dst_vec);
  }
  else {
    Vec src_vec = uc.new_vec_with_width(vw, "src_vec");
    uc.v_loaduvec(src_vec, mem_ptr(src_ptr));
    uc.emit_2v(op, dst_vec, src_vec);
  }

  uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

  uc.end_func();
  return ctx.finish<TestVVFunc>();
}

// The following variations are supported:
//   - 0 - separate destination & source registers
//   - 1 - destination register is a source register as well
//   - 2 - source is a memory operand
static constexpr uint32_t kNumVariationsVVI = 3;

static TestVVFunc create_func_vvi(JitContext& ctx, VecWidth vw, UniOpVVI op, uint32_t imm, Variation variation = Variation{0}) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(vw);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, const void*>());
  EXPECT_NOT_NULL(node);

  Gp dst_ptr = uc.new_gpz("dst_ptr");
  Gp src_ptr = uc.new_gpz("src_ptr");

  node->set_arg(0, dst_ptr);
  node->set_arg(1, src_ptr);

  Vec src_vec = uc.new_vec_with_width(vw, "src_vec");

  switch (variation.value) {
    default:
    case 0: {
      // There are some instructions that fill the high part of the register, so just zero the destination to make
      // sure that we can test this function (that the low part is actually zeroed and doesn't contain garbage).
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.v_loaduvec(src_vec, mem_ptr(src_ptr));
      uc.emit_2vi(op, dst_vec, src_vec, imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 1: {
      uc.v_loaduvec(src_vec, mem_ptr(src_ptr));
      uc.emit_2vi(op, src_vec, src_vec, imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), src_vec);
      break;
    }

    case 2: {
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.emit_2vi(op, dst_vec, mem_ptr(src_ptr), imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);
      break;
    }
  }

  uc.end_func();
  return ctx.finish<TestVVFunc>();
}

// The following variations are supported:
//   - 0 - separate destination & source registers
//   - 1 - destination register is the same as the first source register
//   - 2 - destination register is the same as the second source register
//   - 3 - separate destination & source registers, the second source is a memory operand
//   - 4 - destination register is the same as the first source register, second source is a memory operand
static constexpr uint32_t kNumVariationsVVV = 5;

static TestVVVFunc create_func_vvv(JitContext& ctx, VecWidth vw, UniOpVVV op, Variation variation = Variation{0}) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(vw);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  Gp dst_ptr = uc.new_gpz("dst_ptr");
  Gp src1_ptr = uc.new_gpz("src1_ptr");
  Gp src2_ptr = uc.new_gpz("src2_ptr");

  node->set_arg(0, dst_ptr);
  node->set_arg(1, src1_ptr);
  node->set_arg(2, src2_ptr);

  Vec src1_vec = uc.new_vec_with_width(vw, "src1_vec");
  Vec src2_vec = uc.new_vec_with_width(vw, "src2_vec");

  switch (variation.value) {
    default:
    case 0: {
      // There are some instructions that fill the high part of the register, so just zero the destination to make
      // sure that we can test this function (that the low part is actually zeroed and doesn't contain garbage).
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3v(op, dst_vec, src1_vec, src2_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 1: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3v(op, src1_vec, src1_vec, src2_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), src1_vec);

      break;
    }

    case 2: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3v(op, src2_vec, src1_vec, src2_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), src2_vec);

      break;
    }

    case 3: {
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.emit_3v(op, dst_vec, src1_vec, mem_ptr(src2_ptr));
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 4: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.emit_3v(op, src1_vec, src1_vec, mem_ptr(src2_ptr));
      uc.v_storeuvec(mem_ptr(dst_ptr), src1_vec);

      break;
    }
  }

  uc.end_func();
  return ctx.finish<TestVVVFunc>();
}

static constexpr uint32_t kNumVariationsVVVI = 5;

static TestVVVFunc create_func_vvvi(JitContext& ctx, VecWidth vw, UniOpVVVI op, uint32_t imm, Variation variation = Variation{0}) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(vw);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  Gp dst_ptr = uc.new_gpz("dst_ptr");
  Gp src1_ptr = uc.new_gpz("src1_ptr");
  Gp src2_ptr = uc.new_gpz("src2_ptr");

  node->set_arg(0, dst_ptr);
  node->set_arg(1, src1_ptr);
  node->set_arg(2, src2_ptr);

  Vec src1_vec = uc.new_vec_with_width(vw, "src1_vec");
  Vec src2_vec = uc.new_vec_with_width(vw, "src2_vec");

  switch (variation.value) {
    default:
    case 0: {
      // There are some instructions that fill the high part of the register, so just zero the destination to make
      // sure that we can test this function (that the low part is actually zeroed and doesn't contain garbage).
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3vi(op, dst_vec, src1_vec, src2_vec, imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 1: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3vi(op, src1_vec, src1_vec, src2_vec, imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), src1_vec);

      break;
    }

    case 2: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
      uc.emit_3vi(op, src2_vec, src1_vec, src2_vec, imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), src2_vec);

      break;
    }

    case 3: {
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.emit_3vi(op, dst_vec, src1_vec, mem_ptr(src2_ptr), imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 4: {
      uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
      uc.emit_3vi(op, src1_vec, src1_vec, mem_ptr(src2_ptr), imm);
      uc.v_storeuvec(mem_ptr(dst_ptr), src1_vec);

      break;
    }
  }

  uc.end_func();
  return ctx.finish<TestVVVFunc>();
}

// The following variations are supported:
//   - 0 - separate destination & source registers
//   - 1 - destination register is the first source register
//   - 2 - destination register is the second source register
//   - 3 - destination register is the third source register
static constexpr uint32_t kNumVariationsVVVV = 4;

static TestVVVVFunc create_func_vvvv(JitContext& ctx, VecWidth vw, UniOpVVVV op, Variation variation = Variation{0}) {
  ctx.prepare();

  UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);
  uc.init_vec_width(vw);

  FuncNode* node = uc.add_func(FuncSignature::build<void, void*, const void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  Gp dst_ptr = uc.new_gpz("dst_ptr");
  Gp src1_ptr = uc.new_gpz("src1_ptr");
  Gp src2_ptr = uc.new_gpz("src2_ptr");
  Gp src3_ptr = uc.new_gpz("src3_ptr");

  node->set_arg(0, dst_ptr);
  node->set_arg(1, src1_ptr);
  node->set_arg(2, src2_ptr);
  node->set_arg(3, src3_ptr);

  Vec src1_vec = uc.new_vec_with_width(vw, "src1_vec");
  Vec src2_vec = uc.new_vec_with_width(vw, "src2_vec");
  Vec src3_vec = uc.new_vec_with_width(vw, "src3_vec");

  uc.v_loaduvec(src1_vec, mem_ptr(src1_ptr));
  uc.v_loaduvec(src2_vec, mem_ptr(src2_ptr));
  uc.v_loaduvec(src3_vec, mem_ptr(src3_ptr));

  switch (variation.value) {
    default:
    case 0: {
      // There are some instructions that fill the high part of the register, so just zero the destination to make
      // sure that we can test this function (that the low part is actually zeroed and doesn't contain garbage).
      Vec dst_vec = uc.new_vec_with_width(vw, "dst_vec");
      uc.v_zero_i(dst_vec);

      uc.emit_4v(op, dst_vec, src1_vec, src2_vec, src3_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), dst_vec);

      break;
    }

    case 1: {
      uc.emit_4v(op, src1_vec, src1_vec, src2_vec, src3_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), src1_vec);

      break;
    }

    case 2: {
      uc.emit_4v(op, src2_vec, src1_vec, src2_vec, src3_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), src2_vec);

      break;
    }

    case 3: {
      uc.emit_4v(op, src3_vec, src1_vec, src2_vec, src3_vec);
      uc.v_storeuvec(mem_ptr(dst_ptr), src3_vec);

      break;
    }
  }

  uc.end_func();
  return ctx.finish<TestVVVVFunc>();
}
// ujit::UniCompiler - Tests - SIMD - Vector Overlay
// =================================================

enum class VecElementType : uint8_t {
  kInt8,
  kInt16,
  kInt32,
  kInt64,
  kUInt8,
  kUInt16,
  kUInt32,
  kUInt64,
  kFloat32,
  kFloat64
};

struct VecOpInfo {
  uint32_t _data;

  ASMJIT_INLINE_NODEBUG uint32_t count() const noexcept { return _data >> 28; }

  ASMJIT_INLINE_NODEBUG VecElementType ret() const noexcept { return VecElementType((_data >> 24) & 0xFu); }
  ASMJIT_INLINE_NODEBUG VecElementType arg(uint32_t i) const noexcept { return VecElementType((_data >> (i * 4)) & 0xFu); }

  static ASMJIT_INLINE_NODEBUG VecOpInfo make(VecElementType ret, VecElementType arg0) noexcept {
    return VecOpInfo{(1u << 28) | (uint32_t(ret) << 24) | uint32_t(arg0)};
  }

  static ASMJIT_INLINE_NODEBUG VecOpInfo make(VecElementType ret, VecElementType arg0, VecElementType arg1) noexcept {
    return VecOpInfo{(1u << 28) | (uint32_t(ret) << 24) | uint32_t(arg0) | (uint32_t(arg1) << 4)};
  }

  static ASMJIT_INLINE_NODEBUG VecOpInfo make(VecElementType ret, VecElementType arg0, VecElementType arg1, VecElementType arg2) noexcept {
    return VecOpInfo{(1u << 28) | (uint32_t(ret) << 24) | uint32_t(arg0) | (uint32_t(arg1) << 4) | (uint32_t(arg2) << 8)};
  }

  static ASMJIT_INLINE_NODEBUG VecOpInfo make(VecElementType ret, VecElementType arg0, VecElementType arg1, VecElementType arg2, VecElementType arg3) noexcept {
    return VecOpInfo{(1u << 28) | (uint32_t(ret) << 24) | uint32_t(arg0) | (uint32_t(arg1) << 4) | (uint32_t(arg2) << 8) | (uint32_t(arg3) << 12)};
  }
};

template<uint32_t kW>
struct alignas(16) VecOverlay {
  union {
    int8_t data_i8[kW];
    uint8_t data_u8[kW];

    int16_t data_i16[kW / 2u];
    uint16_t data_u16[kW / 2u];

    int32_t data_i32[kW / 4u];
    uint32_t data_u32[kW / 4u];

    int64_t data_i64[kW / 8u];
    uint64_t data_u64[kW / 8u];

    float data_f32[kW / 4u];
    double data_f64[kW / 8u];
  };

  template<typename T>
  ASMJIT_INLINE_NODEBUG T* data() noexcept;

  template<typename T>
  ASMJIT_INLINE_NODEBUG const T* data() const noexcept;

  template<typename T>
  ASMJIT_INLINE_NODEBUG T get(size_t index) const noexcept;

  template<typename T>
  ASMJIT_INLINE_NODEBUG void set(size_t index, const T& value) noexcept;

  template<uint32_t kOtherW>
  ASMJIT_INLINE_NODEBUG void copy_16b_from(const VecOverlay<kOtherW>& other) noexcept {
    data_u64[0] = other.data_u64[0];
    data_u64[1] = other.data_u64[1];
  }
};

template<typename T>
struct VecAccess;

template<>
struct VecAccess<int8_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int8_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_i8; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const int8_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_i8; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int8_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_i8[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, int8_t value) noexcept { vec.data_i8[index] = value; }
};

template<>
struct VecAccess<int16_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int16_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_i16; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const int16_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_i16; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int16_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_i16[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, int16_t value) noexcept { vec.data_i16[index] = value; }
};

template<>
struct VecAccess<int32_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int32_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_i32; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const int32_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_i32; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int32_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_i32[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, int32_t value) noexcept { vec.data_i32[index] = value; }
};

template<>
struct VecAccess<int64_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int64_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_i64; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const int64_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_i64; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG int64_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_i64[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, int64_t value) noexcept { vec.data_i64[index] = value; }
};

template<>
struct VecAccess<uint8_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint8_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_u8; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const uint8_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_u8; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint8_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_u8[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, uint8_t value) noexcept { vec.data_u8[index] = value; }
};

template<>
struct VecAccess<uint16_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint16_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_u16; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const uint16_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_u16; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint16_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_u16[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, uint16_t value) noexcept { vec.data_u16[index] = value; }
};

template<>
struct VecAccess<uint32_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint32_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_u32; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const uint32_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_u32; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint32_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_u32[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, uint32_t value) noexcept { vec.data_u32[index] = value; }
};

template<>
struct VecAccess<uint64_t> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint64_t* data(VecOverlay<kW>& vec) noexcept { return vec.data_u64; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const uint64_t* data(const VecOverlay<kW>& vec) noexcept { return vec.data_u64; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG uint64_t get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_u64[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, uint64_t value) noexcept { vec.data_u64[index] = value; }
};

template<>
struct VecAccess<float> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG float* data(VecOverlay<kW>& vec) noexcept { return vec.data_f32; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const float* data(const VecOverlay<kW>& vec) noexcept { return vec.data_f32; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG float get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_f32[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, float value) noexcept { vec.data_f32[index] = value; }
};

template<>
struct VecAccess<double> {
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG double* data(VecOverlay<kW>& vec) noexcept { return vec.data_f64; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG const double* data(const VecOverlay<kW>& vec) noexcept { return vec.data_f64; }

  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG double get(const VecOverlay<kW>& vec, size_t index) noexcept { return vec.data_f64[index]; }
  template<uint32_t kW> static ASMJIT_INLINE_NODEBUG void set(VecOverlay<kW>& vec, size_t index, double value) noexcept { vec.data_f64[index] = value; }
};

template<uint32_t kW>
template<typename T>
ASMJIT_INLINE_NODEBUG T* VecOverlay<kW>::data() noexcept { return VecAccess<T>::data(*this); }

template<uint32_t kW>
template<typename T>
ASMJIT_INLINE_NODEBUG const T* VecOverlay<kW>::data() const noexcept { return VecAccess<T>::data(*this); }

template<uint32_t kW>
template<typename T>
ASMJIT_INLINE_NODEBUG T VecOverlay<kW>::get(size_t index) const noexcept { return VecAccess<T>::get(*this, index); }

template<uint32_t kW>
template<typename T>
ASMJIT_INLINE_NODEBUG void VecOverlay<kW>::set(size_t index, const T& value) noexcept { return VecAccess<T>::set(*this, index, value); }

template<typename T> struct TypeNameToString {};
template<> struct TypeNameToString<int8_t  > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "int8"; } };
template<> struct TypeNameToString<int16_t > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "int16"; } };
template<> struct TypeNameToString<int32_t > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "int32"; } };
template<> struct TypeNameToString<int64_t > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "int64"; } };
template<> struct TypeNameToString<uint8_t > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "uint8"; } };
template<> struct TypeNameToString<uint16_t> { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "uint16"; } };
template<> struct TypeNameToString<uint32_t> { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "uint32"; } };
template<> struct TypeNameToString<uint64_t> { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "uint64"; } };
template<> struct TypeNameToString<float   > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "float32"; } };
template<> struct TypeNameToString<double  > { static ASMJIT_INLINE_NODEBUG const char* get() noexcept { return "float64"; } };

template<uint32_t kW>
static bool vec_eq(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
  return memcmp(a.data_u8, b.data_u8, kW) == 0;
}

template<typename T>
static bool float_eq(const T& a, const T& b) noexcept {
  return a == b || (std::isnan(a) && std::isnan(b));
}

template<uint32_t kW>
static bool vec_eq(const VecOverlay<kW>& a, const VecOverlay<kW>& b, VecElementType element_type) noexcept {
  if (element_type == VecElementType::kFloat32) {
    size_t count = kW / sizeof(float);
    for (size_t i = 0; i < count; i++) {
      if (!float_eq(a.data_f32[i], b.data_f32[i])) {
        return false;
      }
    }
    return true;
  }
  else if (element_type == VecElementType::kFloat64) {
    size_t count = kW / sizeof(double);
    for (size_t i = 0; i < count; i++) {
      if (!float_eq(a.data_f64[i], b.data_f64[i])) {
        return false;
      }
    }
    return true;
  }
  else {
    return vec_eq(a, b);
  }
}

template<uint32_t kW>
static ASMJIT_NOINLINE String vec_stringify(const VecOverlay<kW>& vec, VecElementType element_type) noexcept {
  String s;
  s.append('{');

  switch (element_type) {
    case VecElementType::kInt8   : { for (uint32_t i = 0; i < kW    ; i++) s.append_format("%s%d"  , i == 0 ? "" : ", ", vec.data_i8[i]); break; }
    case VecElementType::kInt16  : { for (uint32_t i = 0; i < kW / 2; i++) s.append_format("%s%d"  , i == 0 ? "" : ", ", vec.data_i16[i]); break; }
    case VecElementType::kInt32  : { for (uint32_t i = 0; i < kW / 4; i++) s.append_format("%s%d"  , i == 0 ? "" : ", ", vec.data_i32[i]); break; }
    case VecElementType::kInt64  : { for (uint32_t i = 0; i < kW / 8; i++) s.append_format("%s%lld", i == 0 ? "" : ", ", (long long)vec.data_i64[i]); break; }
    case VecElementType::kUInt8  : { for (uint32_t i = 0; i < kW    ; i++) s.append_format("%s%u"  , i == 0 ? "" : ", ", vec.data_u8[i]); break; }
    case VecElementType::kUInt16 : { for (uint32_t i = 0; i < kW / 2; i++) s.append_format("%s%u"  , i == 0 ? "" : ", ", vec.data_u16[i]); break; }
    case VecElementType::kUInt32 : { for (uint32_t i = 0; i < kW / 4; i++) s.append_format("%s%u"  , i == 0 ? "" : ", ", vec.data_u32[i]); break; }
    case VecElementType::kUInt64 : { for (uint32_t i = 0; i < kW / 8; i++) s.append_format("%s%llu", i == 0 ? "" : ", ", (unsigned long long)vec.data_u64[i]); break; }
    case VecElementType::kFloat32: { for (uint32_t i = 0; i < kW / 4; i++) s.append_format("%s%.20f"  , i == 0 ? "" : ", ", double(vec.data_f32[i])); break; }
    case VecElementType::kFloat64: { for (uint32_t i = 0; i < kW / 8; i++) s.append_format("%s%.20f"  , i == 0 ? "" : ", ", double(vec.data_f64[i])); break; }

    default:
      ASMJIT_NOT_REACHED();
  }

  s.append('}');
  return s;
}

// ujit::UniCompiler - Tests - SIMD - Metadata
// ===========================================

static const char* vec_op_name_vv(UniOpVV op) noexcept {
  switch (op) {
    case UniOpVV::kMov               : return "v_mov";
    case UniOpVV::kMovU64            : return "v_mov_u64";
    case UniOpVV::kBroadcastU8Z      : return "v_broadcast_u8z";
    case UniOpVV::kBroadcastU16Z     : return "v_broadcast_u16z";
    case UniOpVV::kBroadcastU8       : return "v_broadcast_u8";
    case UniOpVV::kBroadcastU16      : return "v_broadcast_u16";
    case UniOpVV::kBroadcastU32      : return "v_broadcast_u32";
    case UniOpVV::kBroadcastU64      : return "v_broadcast_u64";
    case UniOpVV::kBroadcastF32      : return "v_broadcast_f32";
    case UniOpVV::kBroadcastF64      : return "v_broadcast_f64";
    case UniOpVV::kBroadcastV128_U32 : return "v_broadcast_v128_u32";
    case UniOpVV::kBroadcastV128_U64 : return "v_broadcast_v128_u64";
    case UniOpVV::kBroadcastV128_F32 : return "v_broadcast_v128_f32";
    case UniOpVV::kBroadcastV128_F64 : return "v_broadcast_v128_f64";
    case UniOpVV::kBroadcastV256_U32 : return "v_broadcast_v256_u32";
    case UniOpVV::kBroadcastV256_U64 : return "v_broadcast_v256_u64";
    case UniOpVV::kBroadcastV256_F32 : return "v_broadcast_v256_f32";
    case UniOpVV::kBroadcastV256_F64 : return "v_broadcast_v256_f64";
    case UniOpVV::kAbsI8             : return "v_abs_i8";
    case UniOpVV::kAbsI16            : return "v_abs_i16";
    case UniOpVV::kAbsI32            : return "v_abs_i32";
    case UniOpVV::kAbsI64            : return "v_abs_i64";
    case UniOpVV::kNotU32            : return "v_not_u32";
    case UniOpVV::kNotU64            : return "v_not_u64";
    case UniOpVV::kCvtI8LoToI16      : return "v_cvt_i8_lo_to_i16";
    case UniOpVV::kCvtI8HiToI16      : return "v_cvt_i8_hi_to_i16";
    case UniOpVV::kCvtU8LoToU16      : return "v_cvt_u8_lo_to_u16";
    case UniOpVV::kCvtU8HiToU16      : return "v_cvt_u8_hi_to_u16";
    case UniOpVV::kCvtI8ToI32        : return "v_cvt_i8_to_i32";
    case UniOpVV::kCvtU8ToU32        : return "v_cvt_u8_to_u32";
    case UniOpVV::kCvtI16LoToI32     : return "v_cvt_i16_lo_to_i32";
    case UniOpVV::kCvtI16HiToI32     : return "v_cvt_i16_hi_to_i32";
    case UniOpVV::kCvtU16LoToU32     : return "v_cvt_u16_lo_to_u32";
    case UniOpVV::kCvtU16HiToU32     : return "v_cvt_u16_hi_to_u32";
    case UniOpVV::kCvtI32LoToI64     : return "v_cvt_i32_lo_to_i64";
    case UniOpVV::kCvtI32HiToI64     : return "v_cvt_i32_hi_to_i64";
    case UniOpVV::kCvtU32LoToU64     : return "v_cvt_u32_lo_to_u64";
    case UniOpVV::kCvtU32HiToU64     : return "v_cvt_u32_hi_to_u64";
    case UniOpVV::kAbsF32S           : return "s_abs_f32";
    case UniOpVV::kAbsF64S           : return "s_abs_f64";
    case UniOpVV::kAbsF32            : return "v_abs_f32";
    case UniOpVV::kAbsF64            : return "v_abs_f64";
    case UniOpVV::kNegF32S           : return "s_neg_f32";
    case UniOpVV::kNegF64S           : return "s_neg_f64";
    case UniOpVV::kNegF32            : return "v_neg_f32";
    case UniOpVV::kNegF64            : return "v_neg_f64";
    case UniOpVV::kNotF32            : return "v_not_f32";
    case UniOpVV::kNotF64            : return "v_not_f64";
    case UniOpVV::kTruncF32S         : return "v_trunc_f32s";
    case UniOpVV::kTruncF64S         : return "v_trunc_f64s";
    case UniOpVV::kTruncF32          : return "v_trunc_f32";
    case UniOpVV::kTruncF64          : return "v_trunc_f64";
    case UniOpVV::kFloorF32S         : return "v_floor_f32s";
    case UniOpVV::kFloorF64S         : return "v_floor_f64s";
    case UniOpVV::kFloorF32          : return "v_floor_f32";
    case UniOpVV::kFloorF64          : return "v_floor_f64";
    case UniOpVV::kCeilF32S          : return "v_ceil_f32s";
    case UniOpVV::kCeilF64S          : return "v_ceil_f64s";
    case UniOpVV::kCeilF32           : return "v_ceil_f32";
    case UniOpVV::kCeilF64           : return "v_ceil_f64";
    case UniOpVV::kRoundEvenF32S     : return "v_round_even_f32s";
    case UniOpVV::kRoundEvenF64S     : return "v_round_even_f64s";
    case UniOpVV::kRoundEvenF32      : return "v_round_even_f32";
    case UniOpVV::kRoundEvenF64      : return "v_round_even_f64";
    case UniOpVV::kRoundHalfAwayF32S : return "v_round_half_away_f32s";
    case UniOpVV::kRoundHalfAwayF64S : return "v_round_half_away_f64s";
    case UniOpVV::kRoundHalfAwayF32  : return "v_round_half_away_f32";
    case UniOpVV::kRoundHalfAwayF64  : return "v_round_half_away_f64";
    case UniOpVV::kRoundHalfUpF32S   : return "v_round_half_up_f32s";
    case UniOpVV::kRoundHalfUpF64S   : return "v_round_half_up_f64s";
    case UniOpVV::kRoundHalfUpF32    : return "v_round_half_up_f32";
    case UniOpVV::kRoundHalfUpF64    : return "v_round_half_up_f64";
    case UniOpVV::kRcpF32            : return "v_rcp_f32";
    case UniOpVV::kRcpF64            : return "v_rcp_f64";
    case UniOpVV::kSqrtF32S          : return "v_sqrt_f32s";
    case UniOpVV::kSqrtF64S          : return "v_sqrt_f64s";
    case UniOpVV::kSqrtF32           : return "v_sqrt_f32";
    case UniOpVV::kSqrtF64           : return "v_sqrt_f64";
    case UniOpVV::kCvtF32ToF64S      : return "v_cvt_f32_to_f64s";
    case UniOpVV::kCvtF64ToF32S      : return "v_cvt_f64_to_f32s";
    case UniOpVV::kCvtI32ToF32       : return "v_cvt_i32_to_f32";
    case UniOpVV::kCvtF32LoToF64     : return "v_cvt_f32_lo_to_f64";
    case UniOpVV::kCvtF32HiToF64     : return "v_cvt_f32_hi_to_f64";
    case UniOpVV::kCvtF64ToF32Lo     : return "v_cvt_f64_to_f32_lo";
    case UniOpVV::kCvtF64ToF32Hi     : return "v_cvt_f64_to_f32_hi";
    case UniOpVV::kCvtI32LoToF64     : return "v_cvt_i32_lo_to_f64";
    case UniOpVV::kCvtI32HiToF64     : return "v_cvt_i32_hi_to_f64";
    case UniOpVV::kCvtTruncF32ToI32  : return "v_cvt_trunc_f32_to_i32";
    case UniOpVV::kCvtTruncF64ToI32Lo: return "v_cvt_trunc_f64_to_i32_lo";
    case UniOpVV::kCvtTruncF64ToI32Hi: return "v_cvt_trunc_f64_to_i32_hi";
    case UniOpVV::kCvtRoundF32ToI32  : return "v_cvt_round_f32_to_i32";
    case UniOpVV::kCvtRoundF64ToI32Lo: return "v_cvt_round_f64_to_i32_lo";
    case UniOpVV::kCvtRoundF64ToI32Hi: return "v_cvt_round_f64_to_i32_hi";
  }

  ASMJIT_NOT_REACHED();
}

static VecOpInfo vec_op_info_vv(UniOpVV op) noexcept {
  using VE = VecElementType;

  switch (op) {
    case UniOpVV::kMov               : return VecOpInfo::make(VE::kUInt8, VE::kUInt8);
    case UniOpVV::kMovU64            : return VecOpInfo::make(VE::kUInt8, VE::kUInt8);
    case UniOpVV::kBroadcastU8Z      : return VecOpInfo::make(VE::kUInt8, VE::kUInt8);
    case UniOpVV::kBroadcastU16Z     : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVV::kBroadcastU8       : return VecOpInfo::make(VE::kUInt8, VE::kUInt8);
    case UniOpVV::kBroadcastU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVV::kBroadcastU32      : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVV::kBroadcastU64      : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVV::kBroadcastF32      : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kBroadcastF64      : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kBroadcastV128_U32 : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVV::kBroadcastV128_U64 : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVV::kBroadcastV128_F32 : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kBroadcastV128_F64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kBroadcastV256_U32 : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVV::kBroadcastV256_U64 : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVV::kBroadcastV256_F32 : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kBroadcastV256_F64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kAbsI8             : return VecOpInfo::make(VE::kUInt8, VE::kInt8);
    case UniOpVV::kAbsI16            : return VecOpInfo::make(VE::kUInt16, VE::kInt16);
    case UniOpVV::kAbsI32            : return VecOpInfo::make(VE::kUInt32, VE::kInt32);
    case UniOpVV::kAbsI64            : return VecOpInfo::make(VE::kUInt64, VE::kInt64);
    case UniOpVV::kNotU32            : return VecOpInfo::make(VE::kUInt32, VE::kInt32);
    case UniOpVV::kNotU64            : return VecOpInfo::make(VE::kUInt64, VE::kInt64);
    case UniOpVV::kCvtI8LoToI16      : return VecOpInfo::make(VE::kInt16, VE::kInt8);
    case UniOpVV::kCvtI8HiToI16      : return VecOpInfo::make(VE::kInt16, VE::kInt8);
    case UniOpVV::kCvtU8LoToU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt8);
    case UniOpVV::kCvtU8HiToU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt8);
    case UniOpVV::kCvtI8ToI32        : return VecOpInfo::make(VE::kInt32, VE::kInt8);
    case UniOpVV::kCvtU8ToU32        : return VecOpInfo::make(VE::kUInt32, VE::kUInt8);
    case UniOpVV::kCvtI16LoToI32     : return VecOpInfo::make(VE::kInt32, VE::kInt16);
    case UniOpVV::kCvtI16HiToI32     : return VecOpInfo::make(VE::kInt32, VE::kInt16);
    case UniOpVV::kCvtU16LoToU32     : return VecOpInfo::make(VE::kUInt32, VE::kUInt16);
    case UniOpVV::kCvtU16HiToU32     : return VecOpInfo::make(VE::kUInt32, VE::kUInt16);
    case UniOpVV::kCvtI32LoToI64     : return VecOpInfo::make(VE::kInt64, VE::kInt32);
    case UniOpVV::kCvtI32HiToI64     : return VecOpInfo::make(VE::kInt64, VE::kInt32);
    case UniOpVV::kCvtU32LoToU64     : return VecOpInfo::make(VE::kUInt64, VE::kUInt32);
    case UniOpVV::kCvtU32HiToU64     : return VecOpInfo::make(VE::kUInt64, VE::kUInt32);
    case UniOpVV::kAbsF32S           : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kAbsF64S           : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kAbsF32            : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kAbsF64            : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kNegF32S           : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kNegF64S           : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kNegF32            : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kNegF64            : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kNotF32            : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVV::kNotF64            : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVV::kTruncF32S         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kTruncF64S         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kTruncF32          : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kTruncF64          : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kFloorF32S         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kFloorF64S         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kFloorF32          : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kFloorF64          : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kCeilF32S          : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kCeilF64S          : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kCeilF32           : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kCeilF64           : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundEvenF32S     : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundEvenF64S     : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundEvenF32      : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundEvenF64      : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundHalfAwayF32S : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundHalfAwayF64S : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundHalfAwayF32  : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundHalfAwayF64  : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundHalfUpF32S   : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundHalfUpF64S   : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRoundHalfUpF32    : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRoundHalfUpF64    : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kRcpF32            : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kRcpF64            : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kSqrtF32S          : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kSqrtF64S          : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kSqrtF32           : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVV::kSqrtF64           : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVV::kCvtF32ToF64S      : return VecOpInfo::make(VE::kFloat64, VE::kFloat32);
    case UniOpVV::kCvtF64ToF32S      : return VecOpInfo::make(VE::kFloat32, VE::kFloat64);
    case UniOpVV::kCvtI32ToF32       : return VecOpInfo::make(VE::kFloat32, VE::kInt32);
    case UniOpVV::kCvtF32LoToF64     : return VecOpInfo::make(VE::kFloat64, VE::kFloat32);
    case UniOpVV::kCvtF32HiToF64     : return VecOpInfo::make(VE::kFloat64, VE::kFloat32);
    case UniOpVV::kCvtF64ToF32Lo     : return VecOpInfo::make(VE::kFloat32, VE::kFloat64);
    case UniOpVV::kCvtF64ToF32Hi     : return VecOpInfo::make(VE::kFloat32, VE::kFloat64);
    case UniOpVV::kCvtI32LoToF64     : return VecOpInfo::make(VE::kFloat64, VE::kInt32);
    case UniOpVV::kCvtI32HiToF64     : return VecOpInfo::make(VE::kFloat64, VE::kInt32);
    case UniOpVV::kCvtTruncF32ToI32  : return VecOpInfo::make(VE::kInt32, VE::kFloat32);
    case UniOpVV::kCvtTruncF64ToI32Lo: return VecOpInfo::make(VE::kInt32, VE::kFloat64);
    case UniOpVV::kCvtTruncF64ToI32Hi: return VecOpInfo::make(VE::kInt32, VE::kFloat64);
    case UniOpVV::kCvtRoundF32ToI32  : return VecOpInfo::make(VE::kInt32, VE::kFloat32);
    case UniOpVV::kCvtRoundF64ToI32Lo: return VecOpInfo::make(VE::kInt32, VE::kFloat64);
    case UniOpVV::kCvtRoundF64ToI32Hi: return VecOpInfo::make(VE::kInt32, VE::kFloat64);
  }

  ASMJIT_NOT_REACHED();
}

static const char* vec_op_name_vvi(UniOpVVI op) noexcept {
  switch (op) {
    case UniOpVVI::kSllU16         : return "v_sll_u16";
    case UniOpVVI::kSllU32         : return "v_sll_u32";
    case UniOpVVI::kSllU64         : return "v_sll_u64";
    case UniOpVVI::kSrlU16         : return "v_srl_u16";
    case UniOpVVI::kSrlU32         : return "v_srl_u32";
    case UniOpVVI::kSrlU64         : return "v_srl_u64";
    case UniOpVVI::kSraI16         : return "v_sra_i16";
    case UniOpVVI::kSraI32         : return "v_sra_i32";
    case UniOpVVI::kSraI64         : return "v_sra_i64";
    case UniOpVVI::kSllbU128       : return "v_sllb_u128";
    case UniOpVVI::kSrlbU128       : return "v_srlb_u128";
    case UniOpVVI::kSwizzleU16x4   : return "v_swizzle_u16x4";
    case UniOpVVI::kSwizzleLoU16x4 : return "v_swizzle_lo_u16x4";
    case UniOpVVI::kSwizzleHiU16x4 : return "v_swizzle_hi_u16x4";
    case UniOpVVI::kSwizzleU32x4   : return "v_swizzle_u32x4";
    case UniOpVVI::kSwizzleU64x2   : return "v_swizzle_u64x2";
    case UniOpVVI::kSwizzleF32x4   : return "v_swizzle_f32x4";
    case UniOpVVI::kSwizzleF64x2   : return "v_swizzle_f64x2";
    case UniOpVVI::kSwizzleU64x4   : return "v_swizzle_u64x4";
    case UniOpVVI::kSwizzleF64x4   : return "v_swizzle_f64x4";
    case UniOpVVI::kExtractV128_I32: return "v_extract_v128_i32";
    case UniOpVVI::kExtractV128_I64: return "v_extract_v128_i64";
    case UniOpVVI::kExtractV128_F32: return "v_extract_v128_f32";
    case UniOpVVI::kExtractV128_F64: return "v_extract_v128_f64";
    case UniOpVVI::kExtractV256_I32: return "v_extract_v256_i32";
    case UniOpVVI::kExtractV256_I64: return "v_extract_v256_i64";
    case UniOpVVI::kExtractV256_F32: return "v_extract_v256_f32";
    case UniOpVVI::kExtractV256_F64: return "v_extract_v256_f64";

#if defined(ASMJIT_UJIT_AARCH64)
    case UniOpVVI::kSrlRndU16      : return "v_srl_rnd_u16";
    case UniOpVVI::kSrlRndU32      : return "v_srl_rnd_u32";
    case UniOpVVI::kSrlRndU64      : return "v_srl_rnd_u64";
    case UniOpVVI::kSrlAccU16      : return "v_srl_acc_u16";
    case UniOpVVI::kSrlAccU32      : return "v_srl_acc_u32";
    case UniOpVVI::kSrlAccU64      : return "v_srl_acc_u64";
    case UniOpVVI::kSrlRndAccU16   : return "v_srl_rnd_acc_u16";
    case UniOpVVI::kSrlRndAccU32   : return "v_srl_rnd_acc_u32";
    case UniOpVVI::kSrlRndAccU64   : return "v_srl_rnd_acc_u64";
    case UniOpVVI::kSrlnLoU16      : return "v_srln_lo_u16";
    case UniOpVVI::kSrlnHiU16      : return "v_srln_hi_u16";
    case UniOpVVI::kSrlnLoU32      : return "v_srln_lo_u32";
    case UniOpVVI::kSrlnHiU32      : return "v_srln_hi_u32";
    case UniOpVVI::kSrlnLoU64      : return "v_srln_lo_u64";
    case UniOpVVI::kSrlnHiU64      : return "v_srln_hi_u64";
    case UniOpVVI::kSrlnRndLoU16   : return "v_srln_rnd_lo_u16";
    case UniOpVVI::kSrlnRndHiU16   : return "v_srln_rnd_hi_u16";
    case UniOpVVI::kSrlnRndLoU32   : return "v_srln_rnd_lo_u32";
    case UniOpVVI::kSrlnRndHiU32   : return "v_srln_rnd_hi_u32";
    case UniOpVVI::kSrlnRndLoU64   : return "v_srln_rnd_lo_u64";
    case UniOpVVI::kSrlnRndHiU64   : return "v_srln_rnd_hi_u64";
#endif // ASMJIT_UJIT_AARCH64
  }

  ASMJIT_NOT_REACHED();
}

static VecOpInfo vec_op_info_vvi(UniOpVVI op) noexcept {
  using VE = VecElementType;

  switch (op) {
    case UniOpVVI::kSllU16         : return VecOpInfo::make(VE::kUInt16 , VE::kUInt16);
    case UniOpVVI::kSllU32         : return VecOpInfo::make(VE::kUInt32 , VE::kUInt32);
    case UniOpVVI::kSllU64         : return VecOpInfo::make(VE::kUInt64 , VE::kUInt64);
    case UniOpVVI::kSrlU16         : return VecOpInfo::make(VE::kUInt16 , VE::kUInt16);
    case UniOpVVI::kSrlU32         : return VecOpInfo::make(VE::kUInt32 , VE::kUInt32);
    case UniOpVVI::kSrlU64         : return VecOpInfo::make(VE::kUInt64 , VE::kUInt64);
    case UniOpVVI::kSraI16         : return VecOpInfo::make(VE::kInt16  , VE::kInt16);
    case UniOpVVI::kSraI32         : return VecOpInfo::make(VE::kInt32  , VE::kInt32);
    case UniOpVVI::kSraI64         : return VecOpInfo::make(VE::kInt64  , VE::kInt64);
    case UniOpVVI::kSllbU128       : return VecOpInfo::make(VE::kUInt8  , VE::kUInt8);
    case UniOpVVI::kSrlbU128       : return VecOpInfo::make(VE::kUInt8  , VE::kUInt8);
    case UniOpVVI::kSwizzleU16x4   : return VecOpInfo::make(VE::kUInt16 , VE::kUInt16);
    case UniOpVVI::kSwizzleLoU16x4 : return VecOpInfo::make(VE::kUInt16 , VE::kUInt16);
    case UniOpVVI::kSwizzleHiU16x4 : return VecOpInfo::make(VE::kUInt16 , VE::kUInt16);
    case UniOpVVI::kSwizzleU32x4   : return VecOpInfo::make(VE::kUInt32 , VE::kUInt32);
    case UniOpVVI::kSwizzleU64x2   : return VecOpInfo::make(VE::kUInt64 , VE::kUInt64);
    case UniOpVVI::kSwizzleF32x4   : return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVVI::kSwizzleF64x2   : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVVI::kSwizzleU64x4   : return VecOpInfo::make(VE::kUInt64 , VE::kUInt64);
    case UniOpVVI::kSwizzleF64x4   : return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVVI::kExtractV128_I32: return VecOpInfo::make(VE::kInt32  , VE::kInt32);
    case UniOpVVI::kExtractV128_I64: return VecOpInfo::make(VE::kInt64  , VE::kInt64);
    case UniOpVVI::kExtractV128_F32: return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVVI::kExtractV128_F64: return VecOpInfo::make(VE::kFloat64, VE::kFloat64);
    case UniOpVVI::kExtractV256_I32: return VecOpInfo::make(VE::kUInt32 , VE::kUInt32);
    case UniOpVVI::kExtractV256_I64: return VecOpInfo::make(VE::kUInt64 , VE::kUInt64);
    case UniOpVVI::kExtractV256_F32: return VecOpInfo::make(VE::kFloat32, VE::kFloat32);
    case UniOpVVI::kExtractV256_F64: return VecOpInfo::make(VE::kFloat64, VE::kFloat64);

#if defined(ASMJIT_UJIT_AARCH64)
    case UniOpVVI::kSrlRndU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVVI::kSrlRndU32      : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVVI::kSrlRndU64      : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVVI::kSrlAccU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVVI::kSrlAccU32      : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVVI::kSrlAccU64      : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVVI::kSrlRndAccU16   : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVVI::kSrlRndAccU32   : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVVI::kSrlRndAccU64   : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVVI::kSrlnLoU16      : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVVI::kSrlnHiU16      : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVVI::kSrlnLoU32      : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVVI::kSrlnHiU32      : return VecOpInfo::make(VE::kUInt16, VE::kUInt16);
    case UniOpVVI::kSrlnLoU64      : return VecOpInfo::make(VE::kUInt32, VE::kUInt32);
    case UniOpVVI::kSrlnHiU64      : return VecOpInfo::make(VE::kUInt64, VE::kUInt64);
    case UniOpVVI::kSrlnRndLoU16   : return VecOpInfo::make(VE::kUInt8 , VE::kUInt16);
    case UniOpVVI::kSrlnRndHiU16   : return VecOpInfo::make(VE::kUInt8 , VE::kUInt16);
    case UniOpVVI::kSrlnRndLoU32   : return VecOpInfo::make(VE::kUInt16, VE::kUInt32);
    case UniOpVVI::kSrlnRndHiU32   : return VecOpInfo::make(VE::kUInt16, VE::kUInt32);
    case UniOpVVI::kSrlnRndLoU64   : return VecOpInfo::make(VE::kUInt32, VE::kUInt64);
    case UniOpVVI::kSrlnRndHiU64   : return VecOpInfo::make(VE::kUInt32, VE::kUInt64);
#endif // ASMJIT_UJIT_AARCH64
  }

  ASMJIT_NOT_REACHED();
}

static const char* vec_op_name_vvv(UniOpVVV op) noexcept {
  switch (op) {
    case UniOpVVV::kAndU32         : return "v_and_u32";
    case UniOpVVV::kAndU64         : return "v_and_u64";
    case UniOpVVV::kOrU32          : return "v_or_u32";
    case UniOpVVV::kOrU64          : return "v_or_u64";
    case UniOpVVV::kXorU32         : return "v_xor_u32";
    case UniOpVVV::kXorU64         : return "v_xor_u64";
    case UniOpVVV::kAndnU32        : return "v_andn_u32";
    case UniOpVVV::kAndnU64        : return "v_andn_u64";
    case UniOpVVV::kBicU32         : return "v_bic_u32";
    case UniOpVVV::kBicU64         : return "v_bic_u64";
    case UniOpVVV::kAvgrU8         : return "v_avgr_u8";
    case UniOpVVV::kAvgrU16        : return "v_avgr_u16";
    case UniOpVVV::kAddU8          : return "v_add_u8";
    case UniOpVVV::kAddU16         : return "v_add_u16";
    case UniOpVVV::kAddU32         : return "v_add_u32";
    case UniOpVVV::kAddU64         : return "v_add_u64";
    case UniOpVVV::kSubU8          : return "v_sub_u8";
    case UniOpVVV::kSubU16         : return "v_sub_u16";
    case UniOpVVV::kSubU32         : return "v_sub_u32";
    case UniOpVVV::kSubU64         : return "v_sub_u64";
    case UniOpVVV::kAddsI8         : return "v_adds_i8";
    case UniOpVVV::kAddsU8         : return "v_adds_u8";
    case UniOpVVV::kAddsI16        : return "v_adds_i16";
    case UniOpVVV::kAddsU16        : return "v_adds_u16";
    case UniOpVVV::kSubsI8         : return "v_subs_i8";
    case UniOpVVV::kSubsU8         : return "v_subs_u8";
    case UniOpVVV::kSubsI16        : return "v_subs_i16";
    case UniOpVVV::kSubsU16        : return "v_subs_u16";
    case UniOpVVV::kMulU16         : return "v_mul_u16";
    case UniOpVVV::kMulU32         : return "v_mul_u32";
    case UniOpVVV::kMulU64         : return "v_mul_u64";
    case UniOpVVV::kMulhI16        : return "v_mulh_i16";
    case UniOpVVV::kMulhU16        : return "v_mulh_u16";
    case UniOpVVV::kMulU64_LoU32   : return "v_mul_u64_lo_u32";
    case UniOpVVV::kMHAddI16_I32   : return "v_mhadd_i16_i32";
    case UniOpVVV::kMinI8          : return "v_min_i8";
    case UniOpVVV::kMinU8          : return "v_min_u8";
    case UniOpVVV::kMinI16         : return "v_min_i16";
    case UniOpVVV::kMinU16         : return "v_min_u16";
    case UniOpVVV::kMinI32         : return "v_min_i32";
    case UniOpVVV::kMinU32         : return "v_min_u32";
    case UniOpVVV::kMinI64         : return "v_min_i64";
    case UniOpVVV::kMinU64         : return "v_min_u64";
    case UniOpVVV::kMaxI8          : return "v_max_i8";
    case UniOpVVV::kMaxU8          : return "v_max_u8";
    case UniOpVVV::kMaxI16         : return "v_max_i16";
    case UniOpVVV::kMaxU16         : return "v_max_u16";
    case UniOpVVV::kMaxI32         : return "v_max_i32";
    case UniOpVVV::kMaxU32         : return "v_max_u32";
    case UniOpVVV::kMaxI64         : return "v_max_i64";
    case UniOpVVV::kMaxU64         : return "v_max_u64";
    case UniOpVVV::kCmpEqU8        : return "v_cmp_eq_u8";
    case UniOpVVV::kCmpEqU16       : return "v_cmp_eq_u16";
    case UniOpVVV::kCmpEqU32       : return "v_cmp_eq_u32";
    case UniOpVVV::kCmpEqU64       : return "v_cmp_eq_u64";
    case UniOpVVV::kCmpGtI8        : return "v_cmp_gt_i8";
    case UniOpVVV::kCmpGtU8        : return "v_cmp_gt_u8";
    case UniOpVVV::kCmpGtI16       : return "v_cmp_gt_i16";
    case UniOpVVV::kCmpGtU16       : return "v_cmp_gt_u16";
    case UniOpVVV::kCmpGtI32       : return "v_cmp_gt_i32";
    case UniOpVVV::kCmpGtU32       : return "v_cmp_gt_u32";
    case UniOpVVV::kCmpGtI64       : return "v_cmp_gt_i64";
    case UniOpVVV::kCmpGtU64       : return "v_cmp_gt_u64";
    case UniOpVVV::kCmpGeI8        : return "v_cmp_ge_i8";
    case UniOpVVV::kCmpGeU8        : return "v_cmp_ge_u8";
    case UniOpVVV::kCmpGeI16       : return "v_cmp_ge_i16";
    case UniOpVVV::kCmpGeU16       : return "v_cmp_ge_u16";
    case UniOpVVV::kCmpGeI32       : return "v_cmp_ge_i32";
    case UniOpVVV::kCmpGeU32       : return "v_cmp_ge_u32";
    case UniOpVVV::kCmpGeI64       : return "v_cmp_ge_i64";
    case UniOpVVV::kCmpGeU64       : return "v_cmp_ge_u64";
    case UniOpVVV::kCmpLtI8        : return "v_cmp_lt_i8";
    case UniOpVVV::kCmpLtU8        : return "v_cmp_lt_u8";
    case UniOpVVV::kCmpLtI16       : return "v_cmp_lt_i16";
    case UniOpVVV::kCmpLtU16       : return "v_cmp_lt_u16";
    case UniOpVVV::kCmpLtI32       : return "v_cmp_lt_i32";
    case UniOpVVV::kCmpLtU32       : return "v_cmp_lt_u32";
    case UniOpVVV::kCmpLtI64       : return "v_cmp_lt_i64";
    case UniOpVVV::kCmpLtU64       : return "v_cmp_lt_u64";
    case UniOpVVV::kCmpLeI8        : return "v_cmp_le_i8";
    case UniOpVVV::kCmpLeU8        : return "v_cmp_le_u8";
    case UniOpVVV::kCmpLeI16       : return "v_cmp_le_i16";
    case UniOpVVV::kCmpLeU16       : return "v_cmp_le_u16";
    case UniOpVVV::kCmpLeI32       : return "v_cmp_le_i32";
    case UniOpVVV::kCmpLeU32       : return "v_cmp_le_u32";
    case UniOpVVV::kCmpLeI64       : return "v_cmp_le_i64";
    case UniOpVVV::kCmpLeU64       : return "v_cmp_le_u64";
    case UniOpVVV::kAndF32         : return "v_and_f32";
    case UniOpVVV::kAndF64         : return "v_and_f64";
    case UniOpVVV::kOrF32          : return "v_or_f32";
    case UniOpVVV::kOrF64          : return "v_or_f64";
    case UniOpVVV::kXorF32         : return "v_xor_f32";
    case UniOpVVV::kXorF64         : return "v_xor_f64";
    case UniOpVVV::kAndnF32        : return "v_andn_f32";
    case UniOpVVV::kAndnF64        : return "v_andn_f64";
    case UniOpVVV::kBicF32         : return "v_bic_f32";
    case UniOpVVV::kBicF64         : return "v_bic_f64";
    case UniOpVVV::kAddF32S        : return "v_add_f32s";
    case UniOpVVV::kAddF64S        : return "v_add_f64s";
    case UniOpVVV::kAddF32         : return "v_add_f32";
    case UniOpVVV::kAddF64         : return "v_add_f64";
    case UniOpVVV::kSubF32S        : return "v_sub_f32s";
    case UniOpVVV::kSubF64S        : return "v_sub_f64s";
    case UniOpVVV::kSubF32         : return "v_sub_f32";
    case UniOpVVV::kSubF64         : return "v_sub_f64";
    case UniOpVVV::kMulF32S        : return "v_mul_f32s";
    case UniOpVVV::kMulF64S        : return "v_mul_f64s";
    case UniOpVVV::kMulF32         : return "v_mul_f32";
    case UniOpVVV::kMulF64         : return "v_mul_f64";
    case UniOpVVV::kDivF32S        : return "v_div_f32s";
    case UniOpVVV::kDivF64S        : return "v_div_f64s";
    case UniOpVVV::kDivF32         : return "v_div_f32";
    case UniOpVVV::kDivF64         : return "v_div_f64";
    case UniOpVVV::kModF32S        : return "v_mod_f32s";
    case UniOpVVV::kModF64S        : return "v_mod_f64s";
    case UniOpVVV::kModF32         : return "v_mod_f32";
    case UniOpVVV::kModF64         : return "v_mod_f64";
    case UniOpVVV::kMinF32S        : return "v_min_f32s";
    case UniOpVVV::kMinF64S        : return "v_min_f64s";
    case UniOpVVV::kMinF32         : return "v_min_f32";
    case UniOpVVV::kMinF64         : return "v_min_f64";
    case UniOpVVV::kMaxF32S        : return "v_max_f32s";
    case UniOpVVV::kMaxF64S        : return "v_max_f64s";
    case UniOpVVV::kMaxF32         : return "v_max_f32";
    case UniOpVVV::kMaxF64         : return "v_max_f64";
    case UniOpVVV::kCmpEqF32S      : return "v_cmp_eq_f32s";
    case UniOpVVV::kCmpEqF64S      : return "v_cmp_eq_f64s";
    case UniOpVVV::kCmpEqF32       : return "v_cmp_eq_f32";
    case UniOpVVV::kCmpEqF64       : return "v_cmp_eq_f64";
    case UniOpVVV::kCmpNeF32S      : return "v_cmp_ne_f32s";
    case UniOpVVV::kCmpNeF64S      : return "v_cmp_ne_f64s";
    case UniOpVVV::kCmpNeF32       : return "v_cmp_ne_f32";
    case UniOpVVV::kCmpNeF64       : return "v_cmp_ne_f64";
    case UniOpVVV::kCmpGtF32S      : return "v_cmp_gt_f32s";
    case UniOpVVV::kCmpGtF64S      : return "v_cmp_gt_f64s";
    case UniOpVVV::kCmpGtF32       : return "v_cmp_gt_f32";
    case UniOpVVV::kCmpGtF64       : return "v_cmp_gt_f64";
    case UniOpVVV::kCmpGeF32S      : return "v_cmp_ge_f32s";
    case UniOpVVV::kCmpGeF64S      : return "v_cmp_ge_f64s";
    case UniOpVVV::kCmpGeF32       : return "v_cmp_ge_f32";
    case UniOpVVV::kCmpGeF64       : return "v_cmp_ge_f64";
    case UniOpVVV::kCmpLtF32S      : return "v_cmp_lt_f32s";
    case UniOpVVV::kCmpLtF64S      : return "v_cmp_lt_f64s";
    case UniOpVVV::kCmpLtF32       : return "v_cmp_lt_f32";
    case UniOpVVV::kCmpLtF64       : return "v_cmp_lt_f64";
    case UniOpVVV::kCmpLeF32S      : return "v_cmp_le_f32s";
    case UniOpVVV::kCmpLeF64S      : return "v_cmp_le_f64s";
    case UniOpVVV::kCmpLeF32       : return "v_cmp_le_f32";
    case UniOpVVV::kCmpLeF64       : return "v_cmp_le_f64";
    case UniOpVVV::kCmpOrdF32S     : return "v_cmp_ord_f32s";
    case UniOpVVV::kCmpOrdF64S     : return "v_cmp_ord_f64s";
    case UniOpVVV::kCmpOrdF32      : return "v_cmp_ord_f32";
    case UniOpVVV::kCmpOrdF64      : return "v_cmp_ord_f64";
    case UniOpVVV::kCmpUnordF32S   : return "v_cmp_unord_f32s";
    case UniOpVVV::kCmpUnordF64S   : return "v_cmp_unord_f64s";
    case UniOpVVV::kCmpUnordF32    : return "v_cmp_unord_f32";
    case UniOpVVV::kCmpUnordF64    : return "v_cmp_unord_f64";
    case UniOpVVV::kHAddF64        : return "v_hadd_f64";
    case UniOpVVV::kCombineLoHiU64 : return "v_combine_lo_hi_u64";
    case UniOpVVV::kCombineLoHiF64 : return "v_combine_lo_hi_f64";
    case UniOpVVV::kCombineHiLoU64 : return "v_combine_hi_lo_u64";
    case UniOpVVV::kCombineHiLoF64 : return "v_combine_hi_lo_f64";
    case UniOpVVV::kInterleaveLoU8 : return "v_interleave_lo_u8";
    case UniOpVVV::kInterleaveHiU8 : return "v_interleave_hi_u8";
    case UniOpVVV::kInterleaveLoU16: return "v_interleave_lo_u16";
    case UniOpVVV::kInterleaveHiU16: return "v_interleave_hi_u16";
    case UniOpVVV::kInterleaveLoU32: return "v_interleave_lo_u32";
    case UniOpVVV::kInterleaveHiU32: return "v_interleave_hi_u32";
    case UniOpVVV::kInterleaveLoU64: return "v_interleave_lo_u64";
    case UniOpVVV::kInterleaveHiU64: return "v_interleave_hi_u64";
    case UniOpVVV::kInterleaveLoF32: return "v_interleave_lo_f32";
    case UniOpVVV::kInterleaveHiF32: return "v_interleave_hi_f32";
    case UniOpVVV::kInterleaveLoF64: return "v_interleave_lo_f64";
    case UniOpVVV::kInterleaveHiF64: return "v_interleave_hi_f64";
    case UniOpVVV::kPacksI16_I8    : return "v_packs_i16_i8";
    case UniOpVVV::kPacksI16_U8    : return "v_packs_i16_u8";
    case UniOpVVV::kPacksI32_I16   : return "v_packs_i32_i16";
    case UniOpVVV::kPacksI32_U16   : return "v_packs_i32_u16";
    case UniOpVVV::kSwizzlev_U8    : return "v_swizzlev_u8";

#if defined(ASMJIT_UJIT_AARCH64)
    case UniOpVVV::kMulwLoI8       : return "v_mulw_lo_i8";
    case UniOpVVV::kMulwLoU8       : return "v_mulw_lo_u8";
    case UniOpVVV::kMulwHiI8       : return "v_mulw_hi_i8";
    case UniOpVVV::kMulwHiU8       : return "v_mulw_hi_u8";
    case UniOpVVV::kMulwLoI16      : return "v_mulw_lo_i16";
    case UniOpVVV::kMulwLoU16      : return "v_mulw_lo_u16";
    case UniOpVVV::kMulwHiI16      : return "v_mulw_hi_i16";
    case UniOpVVV::kMulwHiU16      : return "v_mulw_hi_u16";
    case UniOpVVV::kMulwLoI32      : return "v_mulw_lo_i32";
    case UniOpVVV::kMulwLoU32      : return "v_mulw_lo_u32";
    case UniOpVVV::kMulwHiI32      : return "v_mulw_hi_i32";
    case UniOpVVV::kMulwHiU32      : return "v_mulw_hi_u32";
    case UniOpVVV::kMAddwLoI8      : return "v_maddw_lo_i8";
    case UniOpVVV::kMAddwLoU8      : return "v_maddw_lo_u8";
    case UniOpVVV::kMAddwHiI8      : return "v_maddw_hi_i8";
    case UniOpVVV::kMAddwHiU8      : return "v_maddw_hi_u8";
    case UniOpVVV::kMAddwLoI16     : return "v_maddw_lo_i16";
    case UniOpVVV::kMAddwLoU16     : return "v_maddw_lo_u16";
    case UniOpVVV::kMAddwHiI16     : return "v_maddw_hi_i16";
    case UniOpVVV::kMAddwHiU16     : return "v_maddw_hi_u16";
    case UniOpVVV::kMAddwLoI32     : return "v_maddw_lo_i32";
    case UniOpVVV::kMAddwLoU32     : return "v_maddw_lo_u32";
    case UniOpVVV::kMAddwHiI32     : return "v_maddw_hi_i32";
    case UniOpVVV::kMAddwHiU32     : return "v_maddw_hi_u32";
#endif // ASMJIT_UJIT_AARCH64

#if defined(ASMJIT_UJIT_X86)
    case UniOpVVV::kPermuteU8       : return "v_permute_u8";
    case UniOpVVV::kPermuteU16      : return "v_permute_u16";
    case UniOpVVV::kPermuteU32      : return "v_permute_u32";
    case UniOpVVV::kPermuteU64      : return "v_permute_u64";
#endif // ASMJIT_UJIT_X86
  }

  ASMJIT_NOT_REACHED();
}

static VecOpInfo vec_op_info_vvv(UniOpVVV op) noexcept {
  using VE = VecElementType;

  switch (op) {
    case UniOpVVV::kAndU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kAndU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kOrU32          : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kOrU64          : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kXorU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kXorU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAndnU32        : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kAndnU64        : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kBicU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kBicU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAvgrU8         : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kAvgrU16        : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kAddU8          : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kAddU16         : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kAddU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kAddU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kSubU8          : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kSubU16         : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kSubU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kSubU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAddsI8         : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kAddsU8         : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kAddsI16        : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kAddsU16        : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kSubsI8         : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kSubsU8         : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kSubsI16        : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kSubsU16        : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMulU16         : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMulU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMulU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kMulhI16        : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMulhU16        : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMulU64_LoU32   : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt32);
    case UniOpVVV::kMHAddI16_I32   : return VecOpInfo::make(VE::kInt32, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMinI8          : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMinU8          : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMinI16         : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMinU16         : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMinI32         : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMinU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMinI64         : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kMinU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kMaxI8          : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMaxU8          : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMaxI16         : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMaxU16         : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMaxI32         : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMaxU32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMaxI64         : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kMaxU64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCmpEqU8        : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kCmpEqU16       : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kCmpEqU32       : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kCmpEqU64       : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCmpGtI8        : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kCmpGtU8        : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kCmpGtI16       : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kCmpGtU16       : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kCmpGtI32       : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kCmpGtU32       : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kCmpGtI64       : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kCmpGtU64       : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCmpGeI8        : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kCmpGeU8        : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kCmpGeI16       : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kCmpGeU16       : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kCmpGeI32       : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kCmpGeU32       : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kCmpGeI64       : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kCmpGeU64       : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCmpLtI8        : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kCmpLtU8        : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kCmpLtI16       : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kCmpLtU16       : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kCmpLtI32       : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kCmpLtU32       : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kCmpLtI64       : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kCmpLtU64       : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCmpLeI8        : return VecOpInfo::make(VE::kInt8, VE::kInt8, VE::kInt8);
    case UniOpVVV::kCmpLeU8        : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kCmpLeI16       : return VecOpInfo::make(VE::kInt16, VE::kInt16, VE::kInt16);
    case UniOpVVV::kCmpLeU16       : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kCmpLeI32       : return VecOpInfo::make(VE::kInt32, VE::kInt32, VE::kInt32);
    case UniOpVVV::kCmpLeU32       : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kCmpLeI64       : return VecOpInfo::make(VE::kInt64, VE::kInt64, VE::kInt64);
    case UniOpVVV::kCmpLeU64       : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAndF32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kAndF64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kOrF32          : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kOrF64          : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kXorF32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kXorF64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAndnF32        : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kAndnF64        : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kBicF32         : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kBicF64         : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kAddF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kAddF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kAddF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kAddF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kSubF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kSubF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kSubF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kSubF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMulF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMulF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMulF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMulF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kDivF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kDivF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kDivF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kDivF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kModF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kModF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kModF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kModF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMinF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMinF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMinF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMinF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMaxF32S        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMaxF64S        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kMaxF32         : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kMaxF64         : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpEqF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpEqF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpEqF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpEqF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpNeF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpNeF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpNeF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpNeF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpGtF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpGtF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpGtF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpGtF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpGeF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpGeF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpGeF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpGeF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpLtF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpLtF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpLtF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpLtF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpLeF32S      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpLeF64S      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpLeF32       : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpLeF64       : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpOrdF32S     : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpOrdF64S     : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpOrdF32      : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpOrdF64      : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpUnordF32S   : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpUnordF64S   : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCmpUnordF32    : return VecOpInfo::make(VE::kUInt32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kCmpUnordF64    : return VecOpInfo::make(VE::kUInt64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kHAddF64        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCombineLoHiU64 : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCombineLoHiF64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kCombineHiLoU64 : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kCombineHiLoF64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kInterleaveLoU8 : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kInterleaveHiU8 : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kInterleaveLoU16: return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kInterleaveHiU16: return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kInterleaveLoU32: return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kInterleaveHiU32: return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kInterleaveLoU64: return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kInterleaveHiU64: return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVV::kInterleaveLoF32: return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kInterleaveHiF32: return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVV::kInterleaveLoF64: return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kInterleaveHiF64: return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVV::kPacksI16_I8    : return VecOpInfo::make(VE::kInt8, VE::kInt16, VE::kInt16);
    case UniOpVVV::kPacksI16_U8    : return VecOpInfo::make(VE::kUInt8, VE::kInt16, VE::kInt16);
    case UniOpVVV::kPacksI32_I16   : return VecOpInfo::make(VE::kInt16, VE::kInt32, VE::kInt32);
    case UniOpVVV::kPacksI32_U16   : return VecOpInfo::make(VE::kUInt16, VE::kInt32, VE::kInt32);
    case UniOpVVV::kSwizzlev_U8    : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);

#if defined(ASMJIT_UJIT_AARCH64)
    case UniOpVVV::kMulwLoI8       : return VecOpInfo::make(VE::kInt16, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMulwLoU8       : return VecOpInfo::make(VE::kUInt16, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMulwHiI8       : return VecOpInfo::make(VE::kInt16, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMulwHiU8       : return VecOpInfo::make(VE::kUInt16, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMulwLoI16      : return VecOpInfo::make(VE::kInt32, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMulwLoU16      : return VecOpInfo::make(VE::kUInt32, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMulwHiI16      : return VecOpInfo::make(VE::kInt32, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMulwHiU16      : return VecOpInfo::make(VE::kUInt32, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMulwLoI32      : return VecOpInfo::make(VE::kInt64, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMulwLoU32      : return VecOpInfo::make(VE::kUInt64, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMulwHiI32      : return VecOpInfo::make(VE::kInt64, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMulwHiU32      : return VecOpInfo::make(VE::kUInt64, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMAddwLoI8      : return VecOpInfo::make(VE::kInt16, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMAddwLoU8      : return VecOpInfo::make(VE::kUInt16, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMAddwHiI8      : return VecOpInfo::make(VE::kInt16, VE::kInt8, VE::kInt8);
    case UniOpVVV::kMAddwHiU8      : return VecOpInfo::make(VE::kUInt16, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kMAddwLoI16     : return VecOpInfo::make(VE::kInt32, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMAddwLoU16     : return VecOpInfo::make(VE::kUInt32, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMAddwHiI16     : return VecOpInfo::make(VE::kInt32, VE::kInt16, VE::kInt16);
    case UniOpVVV::kMAddwHiU16     : return VecOpInfo::make(VE::kUInt32, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kMAddwLoI32     : return VecOpInfo::make(VE::kInt64, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMAddwLoU32     : return VecOpInfo::make(VE::kUInt64, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kMAddwHiI32     : return VecOpInfo::make(VE::kInt64, VE::kInt32, VE::kInt32);
    case UniOpVVV::kMAddwHiU32     : return VecOpInfo::make(VE::kUInt64, VE::kUInt32, VE::kUInt32);
#endif // ASMJIT_UJIT_AARCH64

#if defined(ASMJIT_UJIT_X86)
    case UniOpVVV::kPermuteU8      : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVV::kPermuteU16     : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVV::kPermuteU32     : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVV::kPermuteU64     : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
#endif // ASMJIT_UJIT_X86
  }

  ASMJIT_NOT_REACHED();
}

static const char* vec_op_name_vvvi(UniOpVVVI op) noexcept {
  switch (op) {
    case UniOpVVVI::kAlignr_U128           : return "v_alignr_u128";
    case UniOpVVVI::kInterleaveShuffleU32x4: return "v_interleave_shuffle_u32x4";
    case UniOpVVVI::kInterleaveShuffleU64x2: return "v_interleave_shuffle_u64x2";
    case UniOpVVVI::kInterleaveShuffleF32x4: return "v_interleave_shuffle_f32x4";
    case UniOpVVVI::kInterleaveShuffleF64x2: return "v_interleave_shuffle_f64x2";
    case UniOpVVVI::kInsertV128_U32        : return "v_insert_v128_u32";
    case UniOpVVVI::kInsertV128_F32        : return "v_insert_v128_f32";
    case UniOpVVVI::kInsertV128_U64        : return "v_insert_v128_u64";
    case UniOpVVVI::kInsertV128_F64        : return "v_insert_v128_f64";
    case UniOpVVVI::kInsertV256_U32        : return "v_insert_v256_u32";
    case UniOpVVVI::kInsertV256_F32        : return "v_insert_v256_f32";
    case UniOpVVVI::kInsertV256_U64        : return "v_insert_v256_u64";
    case UniOpVVVI::kInsertV256_F64        : return "v_insert_v256_f64";
  }

  ASMJIT_NOT_REACHED();
}

static VecOpInfo vec_op_info_vvvi(UniOpVVVI op) noexcept {
  using VE = VecElementType;

  switch (op) {
    case UniOpVVVI::kAlignr_U128           : return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVVI::kInterleaveShuffleU32x4: return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVVI::kInterleaveShuffleU64x2: return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVVI::kInterleaveShuffleF32x4: return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVI::kInterleaveShuffleF64x2: return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVI::kInsertV128_U32        : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVVI::kInsertV128_F32        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVI::kInsertV128_U64        : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVVI::kInsertV128_F64        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVI::kInsertV256_U32        : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVVI::kInsertV256_F32        : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVI::kInsertV256_U64        : return VecOpInfo::make(VE::kUInt64, VE::kUInt64, VE::kUInt64);
    case UniOpVVVI::kInsertV256_F64        : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64);

    default:
      ASMJIT_NOT_REACHED();
  }
}

static const char* vec_op_name_vvvv(UniOpVVVV op) noexcept {
  switch (op) {
    case UniOpVVVV::kBlendV_U8: return "v_blendv_u8";
    case UniOpVVVV::kMAddU16  : return "v_madd_u16";
    case UniOpVVVV::kMAddU32  : return "v_madd_u32";
    case UniOpVVVV::kMAddF32S : return "v_madd_f32s";
    case UniOpVVVV::kMAddF64S : return "v_madd_f64s";
    case UniOpVVVV::kMAddF32  : return "v_madd_f32";
    case UniOpVVVV::kMAddF64  : return "v_madd_f64";
    case UniOpVVVV::kMSubF32S : return "v_msub_f32s";
    case UniOpVVVV::kMSubF64S : return "v_msub_f64s";
    case UniOpVVVV::kMSubF32  : return "v_msub_f32";
    case UniOpVVVV::kMSubF64  : return "v_msub_f64";
    case UniOpVVVV::kNMAddF32S: return "v_nmadd_f32s";
    case UniOpVVVV::kNMAddF64S: return "v_nmadd_f64s";
    case UniOpVVVV::kNMAddF32 : return "v_nmadd_f32";
    case UniOpVVVV::kNMAddF64 : return "v_nmadd_f64";
    case UniOpVVVV::kNMSubF32S: return "v_nmsub_f32s";
    case UniOpVVVV::kNMSubF64S: return "v_nmsub_f64s";
    case UniOpVVVV::kNMSubF32 : return "v_nmsub_f32";
    case UniOpVVVV::kNMSubF64 : return "v_nmsub_f64";
  }

  ASMJIT_NOT_REACHED();
}

static VecOpInfo vec_op_info_vvvv(UniOpVVVV op) noexcept {
  using VE = VecElementType;

  switch (op) {
    case UniOpVVVV::kBlendV_U8: return VecOpInfo::make(VE::kUInt8, VE::kUInt8, VE::kUInt8, VE::kUInt8);
    case UniOpVVVV::kMAddU16  : return VecOpInfo::make(VE::kUInt16, VE::kUInt16, VE::kUInt16, VE::kUInt16);
    case UniOpVVVV::kMAddU32  : return VecOpInfo::make(VE::kUInt32, VE::kUInt32, VE::kUInt32, VE::kUInt32);
    case UniOpVVVV::kMAddF32S : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kMAddF64S : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kMAddF32  : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kMAddF64  : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kMSubF32S : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kMSubF64S : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kMSubF32  : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kMSubF64  : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kNMAddF32S: return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kNMAddF64S: return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kNMAddF32 : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kNMAddF64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kNMSubF32S: return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kNMSubF64S: return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
    case UniOpVVVV::kNMSubF32 : return VecOpInfo::make(VE::kFloat32, VE::kFloat32, VE::kFloat32, VE::kFloat32);
    case UniOpVVVV::kNMSubF64 : return VecOpInfo::make(VE::kFloat64, VE::kFloat64, VE::kFloat64, VE::kFloat64);
  }

  ASMJIT_NOT_REACHED();
}

// ujit::UniCompiler - Tests - SIMD - Float To Int - Machine Behavior
// ==================================================================

template<FloatToIntOutsideRangeBehavior behavior, typename IntT, typename FloatT>
static ASMJIT_INLINE_NODEBUG int32_t cvt_float_to_int_trunc(const FloatT& x) noexcept {
  constexpr IntT min_value = axl::min_value<IntT>();
  constexpr IntT max_value = axl::max_value<IntT>();
  constexpr IntT zero = IntT(0);

  if (std::isnan(x)) {
    return behavior == FloatToIntOutsideRangeBehavior::kSmallestValue ? min_value : zero;
  }

  if (x < FloatT(min_value)) {
    return min_value;
  }

  if (x > FloatT(max_value)) {
    return behavior == FloatToIntOutsideRangeBehavior::kSmallestValue ? min_value : max_value;
  }

  return IntT(x);
}

template<FloatToIntOutsideRangeBehavior behavior, typename IntT, typename FloatT>
static ASMJIT_INLINE_NODEBUG int32_t cvt_float_to_int_round(const FloatT& x) noexcept {
  constexpr IntT min_value = axl::min_value<IntT>();
  constexpr IntT max_value = axl::max_value<IntT>();
  constexpr IntT zero = IntT(0);

  if (std::isnan(x)) {
    return behavior == FloatToIntOutsideRangeBehavior::kSmallestValue ? min_value : zero;
  }

  if (x < FloatT(min_value)) {
    return min_value;
  }

  if (x > FloatT(max_value)) {
    return behavior == FloatToIntOutsideRangeBehavior::kSmallestValue ? min_value : max_value;
  }

  return IntT(std::nearbyint(x));
}

// ujit::UniCompiler - Tests - SIMD - Data Generators & Constraints
// ================================================================

// Data generator, which is used to fill the content of SIMD registers.
class DataGenInt {
public:
  TestUtils::Random rng;
  uint32_t step;

  struct Float32Data {
    uint32_t u32;
    float f32;
  };

  ASMJIT_INLINE explicit DataGenInt(uint64_t seed) noexcept
    : rng(seed),
      step(0) {}

  ASMJIT_NOINLINE uint64_t next_uint64() noexcept {
    if (++step >= 256) {
      step = 0;
    }

    // NOTE: Nothing really elaborate - sometimes we want to test also numbers
    // that random number generators won't return often, so we hardcode some.
    switch (step) {
      case   0: return 0u;
      case   1: return 0u;
      case   2: return 0u;
      case   6: return 1u;
      case   7: return 0u;
      case  10: return 0u;
      case  11: return 0xFFu;
      case  15: return 0xFFFFu;
      case  17: return 0xFFFFFFFFu;
      case  21: return 0xFFFFFFFFFFFFFFFFu;
      case  24: return 1u;
      case  40: return 0xFFu;
      case  55: return 0x8080808080808080u;
      case  66: return 0x80000080u;
      case  69: return 1u;
      case  79: return 0x7F;
      case 122: return 0xFFFFu;
      case 123: return 0xFFFFu;
      case 124: return 0xFFFFu;
      case 127: return 1u;
      case 130: return 0xFFu;
      case 142: return 0x7FFFu;
      case 143: return 0x7FFFu;
      case 144: return 0u;
      case 145: return 0x7FFFu;
      default : return rng.next_uint64();
    }
  }

  ASMJIT_NOINLINE float next_float32() noexcept {
    if (++step >= 256) {
      step = 0;
    }

    switch (step) {
      case   0: return 0.0f;
      case   1: return 0.0f;
      case   2: return 0.0f;
      case   6: return 1.0f;
      case   7: return 0.0f;
      case  10: return 0.00001f;
      case  11: return 2.0f;
      case  12: return -axl::infinity<float>();
      case  15: return 3.0f;
      case  17: return 256.0f;
      case  21: return 0.5f;
      case  23: return axl::quiet_nan<float>();
      case  24: return 0.25f;
      case  27: return axl::quiet_nan<float>();
      case  29: return axl::infinity<float>();
      case  31: return axl::quiet_nan<float>();
      case  35: return axl::quiet_nan<float>();
      case  40: return 5.12323f;
      case  45: return -axl::infinity<float>();
      case  55: return 100.5f;
      case  66: return 0.1f;
      case  69: return 0.2f;
      case  79: return 0.3f;
      case  89: return -13005961.0f;
      case  99: return -axl::infinity<float>();
      case 100:
      case 102:
      case 104:
      case 106:
      case 108: return float(rng.next_double());
      case 110:
      case 112:
      case 114:
      case 116:
      case 118: return -float(rng.next_double());
      case 122: return 10.3f;
      case 123: return 20.3f;
      case 124: return -100.3f;
      case 127: return 1.3f;
      case 130: return axl::quiet_nan<float>();
      case 135: return -axl::infinity<float>();
      case 142: return 1.0f;
      case 143: return 1.5f;
      case 144: return 2.0f;
      case 145: return axl::infinity<float>();
      case 155: return -1.5f;
      case 165: return -0.5f;
      case 175: return -1.0f;
      case 245: return 2.5f;

      default: {
        float sign = rng.next_uint32() < 0x7FFFFFF ? 1.0f : -1.0f;
        return float(rng.next_double() * double(rng.next_uint32() & 0xFFFFFFu)) * sign;
      }
    }
  }

  ASMJIT_NOINLINE double next_float64() noexcept {
    if (++step >= 256) {
      step = 0;
    }

    switch (step) {
      case   0: return 0.0;
      case   1: return 0.0;
      case   2: return 0.0;
      case   6: return 1.0;
      case   7: return 0.0;
      case  10: return 0.00001;
      case  11: return 2.0;
      case  12: return -axl::infinity<double>();
      case  15: return 3.0;
      case  17: return 256.0;
      case  21: return 0.5;
      case  23: return axl::quiet_nan<double>();
      case  24: return 0.25;
      case  27: return axl::quiet_nan<double>();
      case  29: return axl::infinity<double>();
      case  31: return axl::quiet_nan<double>();
      case  35: return axl::quiet_nan<double>();
      case  40: return 5.12323;
      case  45: return -axl::infinity<double>();
      case  55: return 100.5;
      case  66: return 0.1;
      case  69: return 0.2;
      case  79: return 0.3;
      case  80: return 4503599627370495.5;
      case  99: return -axl::infinity<double>();
      case 100:
      case 102:
      case 104:
      case 106:
      case 108: return rng.next_double();
      case 110:
      case 112:
      case 114:
      case 116:
      case 118: return -rng.next_double();
      case 122: return 10.3;
      case 123: return 20.3;
      case 124: return -100.3;
      case 125: return 4503599627370496.0;
      case 127: return 1.3;
      case 130: return axl::quiet_nan<double>();
      case 135: return -axl::infinity<double>();
      case 142: return 1.0;
      case 143: return 1.5;
      case 144: return 2.0;
      case 145: return axl::infinity<double>();
      case 155: return -1.5;
      case 165: return -0.5;
      case 175: return -1.0;
      case 245: return 2.5;
      case 248: return -4503599627370495.5;

      default: {
        double sign = rng.next_uint32() < 0x7FFFFFF ? 1.0 : -1.0;
        return double(rng.next_double() * double(rng.next_uint32() & 0x3FFFFFFFu)) * sign;
      }
    }
  }
};

template<typename T>
struct half_minus_1ulp_const;

template<> struct half_minus_1ulp_const<float> { static inline constexpr float value = 0.49999997f; };
template<> struct half_minus_1ulp_const<double> { static inline constexpr double value = 0.49999999999999994; };

// Some SIMD operations are constrained, especially those higher level. So, to successfully test these we
// have to model the constraints in a way that the SIMD instruction we test actually gets the correct input.
// Note that a constraint doesn't have to be always range based, it could be anything.
struct ConstraintNone {
  template<uint32_t kW>
  static ASMJIT_INLINE_NODEBUG void apply(VecOverlay<kW>& v) noexcept { axl::maybe_unused(v); }
};

template<typename ElementT, typename Derived>
struct ConstraintBase {
  template<uint32_t kW>
  static ASMJIT_INLINE void apply(VecOverlay<kW>& v) noexcept {
    ElementT* elements = v.template data<ElementT>();
    for (size_t i = 0; i < kW / sizeof(ElementT); i++) {
      elements[i] = Derived::apply_one(elements[i]);
    }
  }
};

template<uint8_t kMin, uint8_t kMax>
struct ConstraintRangeU8 : public ConstraintBase<uint16_t, ConstraintRangeU8<kMin, kMax>> {
  static ASMJIT_INLINE_NODEBUG uint8_t apply_one(uint8_t x) noexcept { return std::clamp(x, kMin, kMax); }
};

template<uint16_t kMin, uint16_t kMax>
struct ConstraintRangeU16 : public ConstraintBase<uint16_t, ConstraintRangeU16<kMin, kMax>> {
  static ASMJIT_INLINE_NODEBUG uint16_t apply_one(uint16_t x) noexcept { return std::clamp(x, kMin, kMax); }
};

template<uint32_t kMin, uint32_t kMax>
struct ConstraintRangeU32 : public ConstraintBase<uint32_t, ConstraintRangeU32<kMin, kMax>> {
  static ASMJIT_INLINE_NODEBUG uint32_t apply_one(uint32_t x) noexcept { return std::clamp(x, kMin, kMax); }
};

// ujit::UniCompiler - Tests - Generic Operations
// ==============================================

template<typename T>
static ASMJIT_INLINE_NODEBUG std::make_unsigned_t<T> cast_uint(const T& x) noexcept {
  return static_cast<std::make_unsigned_t<T>>(x);
}

template<typename T>
static ASMJIT_INLINE_NODEBUG std::make_signed_t<T> cast_int(const T& x) noexcept {
  return static_cast<std::make_signed_t<T>>(x);
}

static ASMJIT_INLINE_NODEBUG int8_t saturate_i16_to_i8(int16_t x) noexcept {
  return x < int16_t(-128) ? int8_t(-128) :
         x > int16_t( 127) ? int8_t( 127) : int8_t(x & 0xFF);
}

static ASMJIT_INLINE_NODEBUG uint8_t saturate_i16_to_u8(int16_t x) noexcept {
  return x < int16_t(0x00) ? uint8_t(0x00) :
         x > int16_t(0xFF) ? uint8_t(0xFF) : uint8_t(x & 0xFF);
}

static ASMJIT_INLINE_NODEBUG int16_t saturate_i32_to_i16(int32_t x) noexcept {
  return x < int32_t(-32768) ? int16_t(-32768) :
         x > int32_t( 32767) ? int16_t( 32767) : int16_t(x & 0xFFFF);
}

static ASMJIT_INLINE_NODEBUG uint16_t saturate_i32_to_u16(int32_t x) noexcept {
  return x < int32_t(0x0000) ? uint16_t(0x0000) :
         x > int32_t(0xFFFF) ? uint16_t(0xFFFF) : uint16_t(x & 0xFFFF);
}

template<typename T, typename Derived> struct op_each_vv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++) {
      out.set(i, Derived::apply_one(a.template get<T>(i)));
    }
    return out;
  }
};

template<typename T, typename Derived> struct op_each_vvi {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++) {
      out.set(i, Derived::apply_one(a.template get<T>(i), imm));
    }
    return out;
  }
};

template<typename T, typename Derived> struct op_each_vvv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++) {
      out.set(i, Derived::apply_one(a.template get<T>(i), b.template get<T>(i)));
    }
    return out;
  }
};

template<typename T, typename Derived> struct op_each_vvvi {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, uint32_t imm) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++) {
      out.set(i, Derived::apply_one(a.template get<T>(i), b.template get<T>(i), imm));
    }
    return out;
  }
};

template<typename T, typename Derived> struct op_each_vvvv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, const VecOverlay<kW>& c) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++) {
      out.set(i, Derived::apply_one(a.template get<T>(i), b.template get<T>(i), c.template get<T>(i)));
    }
    return out;
  }
};

template<ScalarOpBehavior kB, typename T, typename Derived> struct op_scalar_vv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out {};
    if constexpr (kB == ScalarOpBehavior::kPreservingVec128) {
      out.copy_16b_from(a);
    }
    out.set(0, Derived::apply_one(a.template get<T>(0)));
    return out;
  }
};

template<ScalarOpBehavior kB, typename T, typename Derived> struct op_scalar_vvi {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    VecOverlay<kW> out {};
    if constexpr (kB == ScalarOpBehavior::kPreservingVec128) {
      out.copy_16b_from(a);
    }
    out.set(0, Derived::apply_one(a.template get<T>(0), imm));
    return out;
  }
};

template<ScalarOpBehavior kB, typename T, typename Derived> struct op_scalar_vvv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out {};
    if constexpr (kB == ScalarOpBehavior::kPreservingVec128) {
      out.copy_16b_from(a);
    }
    out.set(0, Derived::apply_one(a.template get<T>(0), b.template get<T>(0)));
    return out;
  }
};

template<ScalarOpBehavior kB, typename T, typename Derived> struct op_scalar_vvvv {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, const VecOverlay<kW>& c) noexcept {
    VecOverlay<kW> out {};
    if constexpr (kB == ScalarOpBehavior::kPreservingVec128) {
      out.copy_16b_from(a);
    }
    out.set(0, Derived::apply_one(a.template get<T>(0), b.template get<T>(0), c.template get<T>(0)));
    return out;
  }
};

// ujit::UniCompiler - Tests - Generic Operations - VV
// ===================================================

struct vec_op_mov : public op_each_vv<uint32_t, vec_op_mov> {
  static ASMJIT_INLINE_NODEBUG uint32_t apply_one(const uint32_t& a) noexcept { return a; }
};

struct vec_op_mov_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    out.data_u64[0] = a.data_u64[0];
    return out;
  }
};

struct vec_op_broadcast_u8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW; i++)
      out.data_u8[i] = a.data_u8[0];
    return out;
  }
};

struct vec_op_broadcast_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / 2u; i++) {
      out.data_u16[i] = a.data_u16[0];
    }
    return out;
  }
};

struct vec_op_broadcast_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / 4u; i++) {
      out.data_u32[i] = a.data_u32[0];
    }
    return out;
  }
};

struct vec_op_broadcast_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t i = 0; i < kW / 8u; i++) {
      out.data_u64[i] = a.data_u64[0];
    }
    return out;
  }
};

struct vec_op_broadcast_u128 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};

    for (uint32_t i = 0; i < kW / 8u; i += 2) {
      out.data_u64[i + 0] = a.data_u64[0];
      out.data_u64[i + 1] = a.data_u64[1];
    }
    return out;
  }
};

struct vec_op_broadcast_u256 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    if constexpr (kW < 32) {
      out = a;
    }
    else {
      for (uint32_t i = 0; i < kW / 8u; i += 4) {
        out.data_u64[i + 0] = a.data_u64[0];
        out.data_u64[i + 1] = a.data_u64[1];
        out.data_u64[i + 2] = a.data_u64[2];
        out.data_u64[i + 3] = a.data_u64[3];
      }
    }
    return out;
  }
};

template<typename T> struct vec_op_abs : public op_each_vv<T, vec_op_abs<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return a < 0 ? T(cast_uint(T(0)) - cast_uint(a)) : a; }
};

template<typename T> struct vec_op_neg : public op_each_vv<T, vec_op_neg<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return T(cast_uint(T(0)) - cast_uint(a)); }
};

template<typename T> struct vec_op_not : public op_each_vv<T, vec_op_not<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return T(~a); }
};

struct vec_op_cvt_i8_lo_to_i16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i16[off / 2 + 0] = a.data_i8[off / 2 + 0];
      out.data_i16[off / 2 + 1] = a.data_i8[off / 2 + 1];
      out.data_i16[off / 2 + 2] = a.data_i8[off / 2 + 2];
      out.data_i16[off / 2 + 3] = a.data_i8[off / 2 + 3];
      out.data_i16[off / 2 + 4] = a.data_i8[off / 2 + 4];
      out.data_i16[off / 2 + 5] = a.data_i8[off / 2 + 5];
      out.data_i16[off / 2 + 6] = a.data_i8[off / 2 + 6];
      out.data_i16[off / 2 + 7] = a.data_i8[off / 2 + 7];
    }
    return out;
  }
};

struct vec_op_cvt_i8_hi_to_i16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i16[off / 2 + 0] = a.data_i8[kW / 2 + off / 2 + 0];
      out.data_i16[off / 2 + 1] = a.data_i8[kW / 2 + off / 2 + 1];
      out.data_i16[off / 2 + 2] = a.data_i8[kW / 2 + off / 2 + 2];
      out.data_i16[off / 2 + 3] = a.data_i8[kW / 2 + off / 2 + 3];
      out.data_i16[off / 2 + 4] = a.data_i8[kW / 2 + off / 2 + 4];
      out.data_i16[off / 2 + 5] = a.data_i8[kW / 2 + off / 2 + 5];
      out.data_i16[off / 2 + 6] = a.data_i8[kW / 2 + off / 2 + 6];
      out.data_i16[off / 2 + 7] = a.data_i8[kW / 2 + off / 2 + 7];
    }
    return out;
  }
};

struct vec_op_cvt_u8_lo_to_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u8[off / 2 + 0];
      out.data_u16[off / 2 + 1] = a.data_u8[off / 2 + 1];
      out.data_u16[off / 2 + 2] = a.data_u8[off / 2 + 2];
      out.data_u16[off / 2 + 3] = a.data_u8[off / 2 + 3];
      out.data_u16[off / 2 + 4] = a.data_u8[off / 2 + 4];
      out.data_u16[off / 2 + 5] = a.data_u8[off / 2 + 5];
      out.data_u16[off / 2 + 6] = a.data_u8[off / 2 + 6];
      out.data_u16[off / 2 + 7] = a.data_u8[off / 2 + 7];
    }
    return out;
  }
};

struct vec_op_cvt_u8_hi_to_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u8[kW / 2 + off / 2 + 0];
      out.data_u16[off / 2 + 1] = a.data_u8[kW / 2 + off / 2 + 1];
      out.data_u16[off / 2 + 2] = a.data_u8[kW / 2 + off / 2 + 2];
      out.data_u16[off / 2 + 3] = a.data_u8[kW / 2 + off / 2 + 3];
      out.data_u16[off / 2 + 4] = a.data_u8[kW / 2 + off / 2 + 4];
      out.data_u16[off / 2 + 5] = a.data_u8[kW / 2 + off / 2 + 5];
      out.data_u16[off / 2 + 6] = a.data_u8[kW / 2 + off / 2 + 6];
      out.data_u16[off / 2 + 7] = a.data_u8[kW / 2 + off / 2 + 7];
    }
    return out;
  }
};

struct vec_op_cvt_i8_to_i32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 4 + 0] = a.data_i8[off / 4 + 0];
      out.data_i32[off / 4 + 1] = a.data_i8[off / 4 + 1];
      out.data_i32[off / 4 + 2] = a.data_i8[off / 4 + 2];
      out.data_i32[off / 4 + 3] = a.data_i8[off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_u8_to_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u8[off / 4 + 0];
      out.data_u32[off / 4 + 1] = a.data_u8[off / 4 + 1];
      out.data_u32[off / 4 + 2] = a.data_u8[off / 4 + 2];
      out.data_u32[off / 4 + 3] = a.data_u8[off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_i16_lo_to_i32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 4 + 0] = a.data_i16[off / 4 + 0];
      out.data_i32[off / 4 + 1] = a.data_i16[off / 4 + 1];
      out.data_i32[off / 4 + 2] = a.data_i16[off / 4 + 2];
      out.data_i32[off / 4 + 3] = a.data_i16[off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_i16_hi_to_i32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 4 + 0] = a.data_i16[kW / 4 + off / 4 + 0];
      out.data_i32[off / 4 + 1] = a.data_i16[kW / 4 + off / 4 + 1];
      out.data_i32[off / 4 + 2] = a.data_i16[kW / 4 + off / 4 + 2];
      out.data_i32[off / 4 + 3] = a.data_i16[kW / 4 + off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_u16_lo_to_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u16[off / 4 + 0];
      out.data_u32[off / 4 + 1] = a.data_u16[off / 4 + 1];
      out.data_u32[off / 4 + 2] = a.data_u16[off / 4 + 2];
      out.data_u32[off / 4 + 3] = a.data_u16[off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_u16_hi_to_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u16[kW / 4 + off / 4 + 0];
      out.data_u32[off / 4 + 1] = a.data_u16[kW / 4 + off / 4 + 1];
      out.data_u32[off / 4 + 2] = a.data_u16[kW / 4 + off / 4 + 2];
      out.data_u32[off / 4 + 3] = a.data_u16[kW / 4 + off / 4 + 3];
    }
    return out;
  }
};

struct vec_op_cvt_i32_lo_to_i64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i64[off / 8 + 0] = a.data_i32[off / 8 + 0];
      out.data_i64[off / 8 + 1] = a.data_i32[off / 8 + 1];
    }
    return out;
  }
};

struct vec_op_cvt_i32_hi_to_i64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i64[off / 8 + 0] = a.data_i32[kW / 8 + off / 8 + 0];
      out.data_i64[off / 8 + 1] = a.data_i32[kW / 8 + off / 8 + 1];
    }
    return out;
  }
};

struct vec_op_cvt_u32_lo_to_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u32[off / 8 + 0];
      out.data_u64[off / 8 + 1] = a.data_u32[off / 8 + 1];
    }
    return out;
  }
};

struct vec_op_cvt_u32_hi_to_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u32[kW / 8 + off / 8 + 0];
      out.data_u64[off / 8 + 1] = a.data_u32[kW / 8 + off / 8 + 1];
    }
    return out;
  }
};

template<typename T> struct vec_op_fabs : public op_each_vv<T, vec_op_fabs<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::fabs(a); }
};

template<typename T> struct vec_op_trunc : public op_each_vv<T, vec_op_trunc<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::trunc(a); }
};

template<typename T> struct vec_op_floor : public op_each_vv<T, vec_op_floor<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::floor(a); }
};

template<typename T> struct vec_op_ceil : public op_each_vv<T, vec_op_ceil<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::ceil(a); }
};

template<typename T> struct vec_op_round_even : public op_each_vv<T, vec_op_round_even<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::nearbyint(a); }
};

template<typename T> struct vec_op_round_half_away : public op_each_vv<T, vec_op_round_half_away<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::trunc(fadd(a, fxor(half_minus_1ulp_const<T>::value, fsign(a)))); }
};

template<typename T> struct vec_op_round_half_up : public op_each_vv<T, vec_op_round_half_up<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::floor(fadd(a, half_minus_1ulp_const<T>::value)); }
};

template<typename T> struct vec_op_sqrt : public op_each_vv<T, vec_op_sqrt<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return fsqrt(a); }
};

template<typename T> struct vec_op_rcp : public op_each_vv<T, vec_op_rcp<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return fdiv(T(1), a); }
};

struct vec_op_cvt_i32_to_f32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_f32[off / 4 + 0] = float(a.data_i32[off / 4 + 0]);
      out.data_f32[off / 4 + 1] = float(a.data_i32[off / 4 + 1]);
      out.data_f32[off / 4 + 2] = float(a.data_i32[off / 4 + 2]);
      out.data_f32[off / 4 + 3] = float(a.data_i32[off / 4 + 3]);
    }
    return out;
  }
};

template<bool kHi>
struct vec_op_cvt_f32_to_f64_impl {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    uint32_t adj = kHi ? kW / 8 : 0u;
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_f64[off / 8 + 0] = double(a.data_f32[off / 8 + adj + 0]);
      out.data_f64[off / 8 + 1] = double(a.data_f32[off / 8 + adj + 1]);
    }
    return out;
  }
};

struct vec_op_cvt_f32_lo_to_f64 : public vec_op_cvt_f32_to_f64_impl<false> {};
struct vec_op_cvt_f32_hi_to_f64 : public vec_op_cvt_f32_to_f64_impl<true> {};

template<bool kHi>
struct vec_op_cvt_f64_to_f32_impl {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    uint32_t adj = kHi ? kW / 8 : 0u;
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_f32[off / 8 + adj + 0] = float(a.data_f64[off / 8 + 0]);
      out.data_f32[off / 8 + adj + 1] = float(a.data_f64[off / 8 + 1]);
    }
    return out;
  }
};

struct vec_op_cvt_f64_to_f32_lo : public vec_op_cvt_f64_to_f32_impl<false> {};
struct vec_op_cvt_f64_to_f32_hi : public vec_op_cvt_f64_to_f32_impl<true> {};

template<bool kHi>
struct vec_op_cvt_i32_to_f64_impl {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    uint32_t adj = kHi ? kW / 8 : 0u;
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_f64[off / 8 + 0] = double(a.data_i32[off / 8 + adj + 0]);
      out.data_f64[off / 8 + 1] = double(a.data_i32[off / 8 + adj + 1]);
    }
    return out;
  }
};

struct vec_op_cvt_i32_lo_to_f64 : public vec_op_cvt_i32_to_f64_impl<false> {};
struct vec_op_cvt_i32_hi_to_f64 : public vec_op_cvt_i32_to_f64_impl<true> {};

template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_trunc_f32_to_i32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 4 + 0] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f32[off / 4 + 0]);
      out.data_i32[off / 4 + 1] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f32[off / 4 + 1]);
      out.data_i32[off / 4 + 2] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f32[off / 4 + 2]);
      out.data_i32[off / 4 + 3] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f32[off / 4 + 3]);
    }
    return out;
  }
};

template<FloatToIntOutsideRangeBehavior behavior, bool kHi>
struct vec_op_cvt_trunc_f64_to_i32_impl {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    uint32_t adj = kHi ? kW / 8 : 0u;
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 8 + adj + 0] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f64[off / 8 + 0]);
      out.data_i32[off / 8 + adj + 1] = cvt_float_to_int_trunc<behavior, int32_t>(a.data_f64[off / 8 + 1]);
    }
    return out;
  }
};

template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_trunc_f64_to_i32_lo : vec_op_cvt_trunc_f64_to_i32_impl<behavior, false> {};

template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_trunc_f64_to_i32_hi : vec_op_cvt_trunc_f64_to_i32_impl<behavior, true> {};

template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_round_f32_to_i32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 4 + 0] = cvt_float_to_int_round<behavior, int32_t>(a.data_f32[off / 4 + 0]);
      out.data_i32[off / 4 + 1] = cvt_float_to_int_round<behavior, int32_t>(a.data_f32[off / 4 + 1]);
      out.data_i32[off / 4 + 2] = cvt_float_to_int_round<behavior, int32_t>(a.data_f32[off / 4 + 2]);
      out.data_i32[off / 4 + 3] = cvt_float_to_int_round<behavior, int32_t>(a.data_f32[off / 4 + 3]);
    }
    return out;
  }
};

template<FloatToIntOutsideRangeBehavior behavior, bool kHi>
struct vec_op_cvt_round_f64_to_i32_impl {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    uint32_t adj = kHi ? kW / 8 : 0u;
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i32[off / 8 + adj + 0] = cvt_float_to_int_round<behavior, int32_t>(a.data_f64[off / 8 + 0]);
      out.data_i32[off / 8 + adj + 1] = cvt_float_to_int_round<behavior, int32_t>(a.data_f64[off / 8 + 1]);
    }
    return out;
  }
};

template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_round_f64_to_i32_lo : vec_op_cvt_round_f64_to_i32_impl<behavior, false> {};
template<FloatToIntOutsideRangeBehavior behavior>
struct vec_op_cvt_round_f64_to_i32_hi : vec_op_cvt_round_f64_to_i32_impl<behavior, true> {};

struct scalar_op_cvt_f32_to_f64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    out.data_f64[0] = a.data_f32[0];
    return out;
  }
};

struct scalar_op_cvt_f64_to_f32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a) noexcept {
    VecOverlay<kW> out{};
    out.data_f32[0] = a.data_f64[0];
    return out;
  }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_trunc : public op_scalar_vv<kB, T, scalar_op_trunc<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::trunc(a); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_floor : public op_scalar_vv<kB, T, scalar_op_floor<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::floor(a); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_ceil : public op_scalar_vv<kB, T, scalar_op_ceil<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::ceil(a); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_round_even : public op_scalar_vv<kB, T, scalar_op_round_even<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::nearbyint(a); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_round_half_away : public op_scalar_vv<kB, T, scalar_op_round_half_away<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::trunc(fadd(a, fxor(half_minus_1ulp_const<T>::value, fsign(a)))); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_round_half_up : public op_scalar_vv<kB, T, scalar_op_round_half_up<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return std::floor(fadd(a, half_minus_1ulp_const<T>::value)); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_sqrt : public op_scalar_vv<kB, T, scalar_op_sqrt<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a) noexcept { return fsqrt(a); }
};

// ujit::UniCompiler - Tests - Generic Operations - VVI
// ====================================================

template<typename T> struct vec_op_slli : public op_each_vvi<T, vec_op_slli<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_uint(a) << imm); }
};

template<typename T> struct vec_op_srli : public op_each_vvi<T, vec_op_srli<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_uint(a) >> imm); }
};

template<typename T> struct vec_op_rsrli : public op_each_vvi<T, vec_op_rsrli<T>> {
  static ASMJIT_INLINE T apply_one(const T& a, uint32_t imm) noexcept {
    T add = T((a & (T(1) << (imm - 1))) != 0);
    return T((cast_uint(a) >> imm) + cast_uint(add));
  }
};

template<typename T> struct vec_op_srai : public op_each_vvi<T, vec_op_srai<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_int(a) >> imm); }
};

struct vec_op_sllb_u128 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i < imm ? uint8_t(0) : a.data_u8[off + i - imm];
      }
    }
    return out;
  }
};

struct vec_op_srlb_u128 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i + imm < 16u ? a.data_u8[off + i + imm] : uint8_t(0);
      }
    }
    return out;
  }
};

struct vec_op_swizzle_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u16[off / 2 + 0 + A];
      out.data_u16[off / 2 + 1] = a.data_u16[off / 2 + 0 + B];
      out.data_u16[off / 2 + 2] = a.data_u16[off / 2 + 0 + C];
      out.data_u16[off / 2 + 3] = a.data_u16[off / 2 + 0 + D];
      out.data_u16[off / 2 + 4] = a.data_u16[off / 2 + 4 + A];
      out.data_u16[off / 2 + 5] = a.data_u16[off / 2 + 4 + B];
      out.data_u16[off / 2 + 6] = a.data_u16[off / 2 + 4 + C];
      out.data_u16[off / 2 + 7] = a.data_u16[off / 2 + 4 + D];
    }
    return out;
  }
};

struct vec_op_swizzle_lo_u16x4 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u16[off / 2 + A];
      out.data_u16[off / 2 + 1] = a.data_u16[off / 2 + B];
      out.data_u16[off / 2 + 2] = a.data_u16[off / 2 + C];
      out.data_u16[off / 2 + 3] = a.data_u16[off / 2 + D];
      memcpy(out.data_u8 + off + 8, a.data_u8 + off + 8, 8);
    }
    return out;
  }
};

struct vec_op_swizzle_hi_u16x4 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      memcpy(out.data_u8 + off, a.data_u8 + off, 8);
      out.data_u16[off / 2 + 4] = a.data_u16[off / 2 + 4 + A];
      out.data_u16[off / 2 + 5] = a.data_u16[off / 2 + 4 + B];
      out.data_u16[off / 2 + 6] = a.data_u16[off / 2 + 4 + C];
      out.data_u16[off / 2 + 7] = a.data_u16[off / 2 + 4 + D];
    }
    return out;
  }
};

struct vec_op_swizzle_u32x4 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u32[off / 4 + A];
      out.data_u32[off / 4 + 1] = a.data_u32[off / 4 + B];
      out.data_u32[off / 4 + 2] = a.data_u32[off / 4 + C];
      out.data_u32[off / 4 + 3] = a.data_u32[off / 4 + D];
    }
    return out;
  }
};

struct vec_op_swizzle_u64x2 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, uint32_t imm) noexcept {
    uint32_t B = (imm >>  8) & 0x1;
    uint32_t A = (imm >>  0) & 0x1;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + A];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + B];
    }
    return out;
  }
};

// ujit::UniCompiler - Tests - SIMD - Generic Operations - VVV
// ===========================================================

template<typename T> struct vec_op_and : public op_each_vvv<T, vec_op_and<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a & b); }
};

template<typename T> struct vec_op_or : public op_each_vvv<T, vec_op_or<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a | b); }
};

template<typename T> struct vec_op_xor : public op_each_vvv<T, vec_op_xor<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a ^ b); }
};

template<typename T> struct vec_op_andn : public op_each_vvv<T, vec_op_andn<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(~a & b); }
};

template<typename T> struct vec_op_bic : public op_each_vvv<T, vec_op_bic<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a & ~b); }
};

template<typename T> struct vec_op_add : public op_each_vvv<T, vec_op_add<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(cast_uint(a) + cast_uint(b)); }
};

template<typename T> struct vec_op_adds : public op_each_vvv<T, vec_op_adds<T>> {
  static ASMJIT_INLINE T apply_one(const T& a, const T& b) noexcept {
    axl::OverflowFlag of{};
    T result = axl::add_overflow(a, b, of);

    if (!axl::did_overflow(of)) {
      return result;
    }

    if constexpr (std::is_unsigned_v<T>) {
      return axl::max_value<T>();
    }
    else {
      return b > T(0) ? axl::max_value<T>() : axl::min_value<T>();
    }
  }
};

template<typename T> struct vec_op_sub : public op_each_vvv<T, vec_op_sub<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(cast_uint(a) - cast_uint(b)); }
};

template<typename T> struct vec_op_subs : public op_each_vvv<T, vec_op_subs<T>> {
  static ASMJIT_INLINE T apply_one(const T& a, const T& b) noexcept {
    axl::OverflowFlag of{};
    T result = axl::sub_overflow(a, b, of);

    if (!axl::did_overflow(of)) {
      return result;
    }

    if constexpr (std::is_unsigned_v<T>) {
      return axl::min_value<T>();
    }
    else {
      return b > T(0) ? axl::min_value<T>() : axl::max_value<T>();
    }
  }
};

template<typename T> struct vec_op_mul : public op_each_vvv<T, vec_op_mul<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T((uint64_t(a) * uint64_t(b)) & uint64_t(~T(0))); }
};

template<typename T> struct vec_op_mulhi : public op_each_vvv<T, vec_op_mulhi<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept {
    uint64_t result = uint64_t(int64_t(cast_int(a))) * uint64_t(int64_t(cast_int(b)));
    return T(T(result >> (sizeof(T) * 8u)) & T(~T(0)));
  }
};

template<typename T> struct vec_op_mulhu : public op_each_vvv<T, vec_op_mulhu<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept {
    uint64_t result = uint64_t(a) * uint64_t(b);
    return T(result >> (sizeof(T) * 8u)) & uint64_t(~T(0));
  }
};

struct vec_op_mul_u64_lo_u32 : public op_each_vvv<uint64_t, vec_op_mul_u64_lo_u32> {
  static ASMJIT_INLINE_NODEBUG uint64_t apply_one(const uint64_t& a, const uint64_t& b) noexcept {
    return uint64_t(a) * uint64_t(b & 0xFFFFFFFFu);
  }
};

struct vec_op_mhadd_i16_i32 : public op_each_vvv<uint32_t, vec_op_mhadd_i16_i32> {
  static ASMJIT_INLINE_NODEBUG uint32_t apply_one(const uint32_t& a, const uint32_t& b) noexcept {
    uint32_t al = uint32_t(int32_t(int16_t(a & 0xFFFF)));
    uint32_t ah = uint32_t(int32_t(int16_t(a >> 16)));

    uint32_t bl = uint32_t(int32_t(int16_t(b & 0xFFFF)));
    uint32_t bh = uint32_t(int32_t(int16_t(b >> 16)));

    return al * bl + ah * bh;
  }
};

template<typename T> struct vec_op_madd : public op_each_vvvv<T, vec_op_madd<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return T((uint64_t(a) * uint64_t(b) + uint64_t(c)) & uint64_t(~T(0))); }
};

template<typename T> struct vec_op_min : public op_each_vvv<T, vec_op_min<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a < b ? a : b; }
};

template<typename T> struct vec_op_max : public op_each_vvv<T, vec_op_max<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a > b ? a : b; }
};

template<typename T> struct vec_op_cmp_eq : public op_each_vvv<T, vec_op_cmp_eq<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a == b ? axl::bit_ones<T> : T(0); }
};

template<typename T> struct vec_op_cmp_ne : public op_each_vvv<T, vec_op_cmp_ne<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a != b ? axl::bit_ones<T> : T(0); }
};

template<typename T> struct vec_op_cmp_gt : public op_each_vvv<T, vec_op_cmp_gt<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a >  b ? axl::bit_ones<T> : T(0); }
};

template<typename T> struct vec_op_cmp_ge : public op_each_vvv<T, vec_op_cmp_ge<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a >= b ? axl::bit_ones<T> : T(0); }
};

template<typename T> struct vec_op_cmp_lt : public op_each_vvv<T, vec_op_cmp_lt<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a <  b ? axl::bit_ones<T> : T(0); }
};

template<typename T> struct vec_op_cmp_le : public op_each_vvv<T, vec_op_cmp_le<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a <= b ? axl::bit_ones<T> : T(0); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fadd : public op_scalar_vvv<kB, T, scalar_op_fadd<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fadd(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fsub : public op_scalar_vvv<kB, T, scalar_op_fsub<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fsub(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmul : public op_scalar_vvv<kB, T, scalar_op_fmul<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fmul(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fdiv : public op_scalar_vvv<kB, T, scalar_op_fdiv<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fdiv(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmin_ternary : public op_scalar_vvv<kB, T, scalar_op_fmin_ternary<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a < b ? a : b; }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmax_ternary : public op_scalar_vvv<kB, T, scalar_op_fmax_ternary<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a > b ? a : b; }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmin_finite : public op_scalar_vvv<kB, T, scalar_op_fmin_finite<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return std::isnan(a) ? b : std::isnan(b) ? a : axl::min(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmax_finite : public op_scalar_vvv<kB, T, scalar_op_fmax_finite<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return std::isnan(a) ? b : std::isnan(b) ? a : axl::max(a, b); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmadd_nofma : public op_scalar_vvvv<kB, T, scalar_op_fmadd_nofma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(a, b, c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmsub_nofma : public op_scalar_vvvv<kB, T, scalar_op_fmsub_nofma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(a, b, -c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fnmadd_nofma : public op_scalar_vvvv<kB, T, scalar_op_fnmadd_nofma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(-a, b, c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fnmsub_nofma : public op_scalar_vvvv<kB, T, scalar_op_fnmsub_nofma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(-a, b, -c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmadd_fma : public op_scalar_vvvv<kB, T, scalar_op_fmadd_fma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(a, b, c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fmsub_fma : public op_scalar_vvvv<kB, T, scalar_op_fmsub_fma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(a, b, -c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fnmadd_fma : public op_scalar_vvvv<kB, T, scalar_op_fnmadd_fma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(-a, b, c); }
};

template<ScalarOpBehavior kB, typename T> struct scalar_op_fnmsub_fma : public op_scalar_vvvv<kB, T, scalar_op_fnmsub_fma<kB, T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(-a, b, -c); }
};

template<typename T> struct vec_op_fadd : public op_each_vvv<T, vec_op_fadd<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fadd(a, b); }
};

template<typename T> struct vec_op_fsub : public op_each_vvv<T, vec_op_fsub<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fsub(a, b); }
};

template<typename T> struct vec_op_fmul : public op_each_vvv<T, vec_op_fmul<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fmul(a, b); }
};

template<typename T> struct vec_op_fdiv : public op_each_vvv<T, vec_op_fdiv<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return fdiv(a, b); }
};

template<typename T> struct vec_op_fmin_ternary : public op_each_vvv<T, vec_op_fmin_ternary<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a < b ? a : b; }
};

template<typename T> struct vec_op_fmax_ternary : public op_each_vvv<T, vec_op_fmax_ternary<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a > b ? a : b; }
};

template<typename T> struct vec_op_fmin_finite : public op_each_vvv<T, vec_op_fmin_finite<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return std::isnan(a) ? b : std::isnan(b) ? a : axl::min(a, b); }
};

template<typename T> struct vec_op_fmax_finite : public op_each_vvv<T, vec_op_fmax_finite<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return std::isnan(a) ? b : std::isnan(b) ? a : axl::max(a, b); }
};

template<typename T> struct vec_op_fmadd_nofma : public op_each_vvvv<T, vec_op_fmadd_nofma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(a, b, c); }
};

template<typename T> struct vec_op_fmsub_nofma : public op_each_vvvv<T, vec_op_fmsub_nofma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(a, b, -c); }
};

template<typename T> struct vec_op_fnmadd_nofma : public op_each_vvvv<T, vec_op_fnmadd_nofma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(-a, b, c); }
};

template<typename T> struct vec_op_fnmsub_nofma : public op_each_vvvv<T, vec_op_fnmsub_nofma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_nofma_ref(-a, b, -c); }
};

template<typename T> struct vec_op_fmadd_fma : public op_each_vvvv<T, vec_op_fmadd_fma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(a, b, c); }
};

template<typename T> struct vec_op_fmsub_fma : public op_each_vvvv<T, vec_op_fmsub_fma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(a, b, -c); }
};

template<typename T> struct vec_op_fnmadd_fma : public op_each_vvvv<T, vec_op_fnmadd_fma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(-a, b, c); }
};

template<typename T> struct vec_op_fnmsub_fma : public op_each_vvvv<T, vec_op_fnmsub_fma<T>> {
  static ASMJIT_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return fmadd_fma_ref(-a, b, -c); }
};

template<typename T>
struct cmp_result {
  typedef T Result;

  static ASMJIT_INLINE_NODEBUG Result make(bool result) noexcept { return result ? Result(~Result(0)) : Result(0); }
};

template<>
struct cmp_result<float> {
  typedef uint32_t Result;

  static ASMJIT_INLINE_NODEBUG Result make(bool result) noexcept { return result ? ~Result(0) : Result(0); }
};

template<>
struct cmp_result<double> {
  typedef uint64_t Result;

  static ASMJIT_INLINE_NODEBUG Result make(bool result) noexcept { return result ? ~Result(0) : Result(0); }
};

template<typename T> struct vec_op_fcmpo_eq : public op_each_vvv<T, vec_op_fcmpo_eq<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(a == b); }
};

template<typename T> struct vec_op_fcmpu_ne : public op_each_vvv<T, vec_op_fcmpu_ne<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(!(a == b)); }
};

template<typename T> struct vec_op_fcmpo_gt : public op_each_vvv<T, vec_op_fcmpo_gt<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(a > b); }
};

template<typename T> struct vec_op_fcmpo_ge : public op_each_vvv<T, vec_op_fcmpo_ge<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(a >= b); }
};

template<typename T> struct vec_op_fcmpo_lt : public op_each_vvv<T, vec_op_fcmpo_lt<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(a < b); }
};

template<typename T> struct vec_op_fcmpo_le : public op_each_vvv<T, vec_op_fcmpo_le<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(a <= b); }
};

template<typename T> struct vec_op_fcmp_ord : public op_each_vvv<T, vec_op_fcmp_ord<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(!std::isnan(a) && !std::isnan(b)); }
};

template<typename T> struct vec_op_fcmp_unord : public op_each_vvv<T, vec_op_fcmp_unord<T>> {
  static ASMJIT_INLINE_NODEBUG typename cmp_result<T>::Result apply_one(const T& a, const T& b) noexcept { return cmp_result<T>::make(std::isnan(a) || std::isnan(b)); }
};

struct vec_op_hadd_f64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_f64[off / 8 + 0] = a.data_f64[off / 8 + 0] + a.data_f64[off / 8 + 1];
      out.data_f64[off / 8 + 1] = b.data_f64[off / 8 + 0] + b.data_f64[off / 8 + 1];
    }
    return out;
  }
};

struct vec_op_combine_lo_hi_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = b.data_u64[off / 8 + 1];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + 0];
    }
    return out;
  }
};

struct vec_op_combine_hi_lo_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = b.data_u64[off / 8 + 0];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + 1];
    }
    return out;
  }
};

struct vec_op_interleave_lo_u8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 8; i++) {
        out.data_u8[off + i * 2 + 0] = a.data_u8[off + i];
        out.data_u8[off + i * 2 + 1] = b.data_u8[off + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_hi_u8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 8; i++) {
        out.data_u8[off + i * 2 + 0] = a.data_u8[off + 8 + i];
        out.data_u8[off + i * 2 + 1] = b.data_u8[off + 8 + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_lo_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 4; i++) {
        out.data_u16[off / 2 + i * 2 + 0] = a.data_u16[off / 2 + i];
        out.data_u16[off / 2 + i * 2 + 1] = b.data_u16[off / 2 + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_hi_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 4; i++) {
        out.data_u16[off / 2 + i * 2 + 0] = a.data_u16[off / 2 + 4 + i];
        out.data_u16[off / 2 + i * 2 + 1] = b.data_u16[off / 2 + 4 + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_lo_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 2; i++) {
        out.data_u32[off / 4 + i * 2 + 0] = a.data_u32[off / 4 + i];
        out.data_u32[off / 4 + i * 2 + 1] = b.data_u32[off / 4 + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_hi_u32 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 2; i++) {
        out.data_u32[off / 4 + i * 2 + 0] = a.data_u32[off / 4 + 2 + i];
        out.data_u32[off / 4 + i * 2 + 1] = b.data_u32[off / 4 + 2 + i];
      }
    }
    return out;
  }
};

struct vec_op_interleave_lo_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + 0];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + 0];
    }
    return out;
  }
};

struct vec_op_interleave_hi_u64 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + 1];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + 1];
    }
    return out;
  }
};

// ujit::UniCompiler - Tests - SIMD - Generic Operations - VVVI
// ============================================================

struct vec_op_alignr_u128 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, uint32_t imm) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i + imm < 16 ? b.data_u8[off + i + imm] : a.data_u8[off + i + imm - 16];
      }
    }
    return out;
  }
};

struct vec_op_interleave_shuffle_u32x4 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u32[off / 4 + A];
      out.data_u32[off / 4 + 1] = a.data_u32[off / 4 + B];
      out.data_u32[off / 4 + 2] = b.data_u32[off / 4 + C];
      out.data_u32[off / 4 + 3] = b.data_u32[off / 4 + D];
    }
    return out;
  }
};

struct vec_op_interleave_shuffle_u64x2 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b, uint32_t imm) noexcept {
    uint32_t B = (imm >>  8) & 0x1;
    uint32_t A = (imm >>  0) & 0x1;

    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + A];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + B];
    }
    return out;
  }
};

struct vec_op_packs_i16_i8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i8[off +  0] = saturate_i16_to_i8(a.data_i16[off / 2 + 0]);
      out.data_i8[off +  1] = saturate_i16_to_i8(a.data_i16[off / 2 + 1]);
      out.data_i8[off +  2] = saturate_i16_to_i8(a.data_i16[off / 2 + 2]);
      out.data_i8[off +  3] = saturate_i16_to_i8(a.data_i16[off / 2 + 3]);
      out.data_i8[off +  4] = saturate_i16_to_i8(a.data_i16[off / 2 + 4]);
      out.data_i8[off +  5] = saturate_i16_to_i8(a.data_i16[off / 2 + 5]);
      out.data_i8[off +  6] = saturate_i16_to_i8(a.data_i16[off / 2 + 6]);
      out.data_i8[off +  7] = saturate_i16_to_i8(a.data_i16[off / 2 + 7]);
      out.data_i8[off +  8] = saturate_i16_to_i8(b.data_i16[off / 2 + 0]);
      out.data_i8[off +  9] = saturate_i16_to_i8(b.data_i16[off / 2 + 1]);
      out.data_i8[off + 10] = saturate_i16_to_i8(b.data_i16[off / 2 + 2]);
      out.data_i8[off + 11] = saturate_i16_to_i8(b.data_i16[off / 2 + 3]);
      out.data_i8[off + 12] = saturate_i16_to_i8(b.data_i16[off / 2 + 4]);
      out.data_i8[off + 13] = saturate_i16_to_i8(b.data_i16[off / 2 + 5]);
      out.data_i8[off + 14] = saturate_i16_to_i8(b.data_i16[off / 2 + 6]);
      out.data_i8[off + 15] = saturate_i16_to_i8(b.data_i16[off / 2 + 7]);
    }
    return out;
  }
};

struct vec_op_packs_i16_u8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u8[off +  0] = saturate_i16_to_u8(a.data_i16[off / 2 + 0]);
      out.data_u8[off +  1] = saturate_i16_to_u8(a.data_i16[off / 2 + 1]);
      out.data_u8[off +  2] = saturate_i16_to_u8(a.data_i16[off / 2 + 2]);
      out.data_u8[off +  3] = saturate_i16_to_u8(a.data_i16[off / 2 + 3]);
      out.data_u8[off +  4] = saturate_i16_to_u8(a.data_i16[off / 2 + 4]);
      out.data_u8[off +  5] = saturate_i16_to_u8(a.data_i16[off / 2 + 5]);
      out.data_u8[off +  6] = saturate_i16_to_u8(a.data_i16[off / 2 + 6]);
      out.data_u8[off +  7] = saturate_i16_to_u8(a.data_i16[off / 2 + 7]);
      out.data_u8[off +  8] = saturate_i16_to_u8(b.data_i16[off / 2 + 0]);
      out.data_u8[off +  9] = saturate_i16_to_u8(b.data_i16[off / 2 + 1]);
      out.data_u8[off + 10] = saturate_i16_to_u8(b.data_i16[off / 2 + 2]);
      out.data_u8[off + 11] = saturate_i16_to_u8(b.data_i16[off / 2 + 3]);
      out.data_u8[off + 12] = saturate_i16_to_u8(b.data_i16[off / 2 + 4]);
      out.data_u8[off + 13] = saturate_i16_to_u8(b.data_i16[off / 2 + 5]);
      out.data_u8[off + 14] = saturate_i16_to_u8(b.data_i16[off / 2 + 6]);
      out.data_u8[off + 15] = saturate_i16_to_u8(b.data_i16[off / 2 + 7]);
    }
    return out;
  }
};

struct vec_op_packs_i32_i16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i16[off / 2 + 0] = saturate_i32_to_i16(a.data_i32[off / 4 + 0]);
      out.data_i16[off / 2 + 1] = saturate_i32_to_i16(a.data_i32[off / 4 + 1]);
      out.data_i16[off / 2 + 2] = saturate_i32_to_i16(a.data_i32[off / 4 + 2]);
      out.data_i16[off / 2 + 3] = saturate_i32_to_i16(a.data_i32[off / 4 + 3]);
      out.data_i16[off / 2 + 4] = saturate_i32_to_i16(b.data_i32[off / 4 + 0]);
      out.data_i16[off / 2 + 5] = saturate_i32_to_i16(b.data_i32[off / 4 + 1]);
      out.data_i16[off / 2 + 6] = saturate_i32_to_i16(b.data_i32[off / 4 + 2]);
      out.data_i16[off / 2 + 7] = saturate_i32_to_i16(b.data_i32[off / 4 + 3]);
    }
    return out;
  }
};

struct vec_op_packs_i32_u16 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = saturate_i32_to_u16(a.data_i32[off / 4 + 0]);
      out.data_u16[off / 2 + 1] = saturate_i32_to_u16(a.data_i32[off / 4 + 1]);
      out.data_u16[off / 2 + 2] = saturate_i32_to_u16(a.data_i32[off / 4 + 2]);
      out.data_u16[off / 2 + 3] = saturate_i32_to_u16(a.data_i32[off / 4 + 3]);
      out.data_u16[off / 2 + 4] = saturate_i32_to_u16(b.data_i32[off / 4 + 0]);
      out.data_u16[off / 2 + 5] = saturate_i32_to_u16(b.data_i32[off / 4 + 1]);
      out.data_u16[off / 2 + 6] = saturate_i32_to_u16(b.data_i32[off / 4 + 2]);
      out.data_u16[off / 2 + 7] = saturate_i32_to_u16(b.data_i32[off / 4 + 3]);
    }
    return out;
  }
};

// ujit::UniCompiler - Tests - SIMD - Generic Operations - VVVV
// ============================================================

struct vec_op_blendv_bits : public op_each_vvvv<uint32_t, vec_op_blendv_bits> {
  static ASMJIT_INLINE_NODEBUG uint32_t apply_one(const uint32_t& a, const uint32_t& b, const uint32_t& c) noexcept { return ((a & ~c) | (b & c)); }
};

struct vec_op_swizzlev_u8 {
  template<uint32_t kW>
  static ASMJIT_INLINE VecOverlay<kW> apply(const VecOverlay<kW>& a, const VecOverlay<kW>& b) noexcept {
    VecOverlay<kW> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        size_t sel = b.data_u8[off + i] & (0x8F); // 3 bits ignored.
        out.data_u8[off + i] = sel & 0x80 ? uint8_t(0) : a.data_u8[off + sel];
      }
    }
    return out;
  }
};

struct vec_op_div255_u16 : public op_each_vv<uint16_t, vec_op_div255_u16> {
  static ASMJIT_INLINE uint16_t apply_one(const uint16_t& a) noexcept {
    uint32_t x = a + 0x80u;
    return uint16_t((x + (x >> 8)) >> 8);
  }
};

struct vec_op_div65535_u32 : public op_each_vv<uint32_t, vec_op_div65535_u32> {
  static ASMJIT_INLINE uint32_t apply_one(const uint32_t& a) noexcept {
    uint32_t x = a + 0x8000u;
    return uint32_t((x + (x >> 16)) >> 16);
  }
};

// ujit::UniCompiler - Tests - SIMD - Utilities
// ============================================

template<uint32_t kW>
static void fill_random_bytes(DataGenInt& dg, VecOverlay<kW>& dst) noexcept {
  for (uint32_t i = 0; i < kW / 8u; i++) {
    dst.data_u64[i] = dg.next_uint64();
  }
}

template<uint32_t kW>
static void fill_random_f32(DataGenInt& dg, VecOverlay<kW>& dst) noexcept {
  for (uint32_t i = 0; i < kW / 4u; i++) {
    dst.data_f32[i] = dg.next_float32();
  }
}

template<uint32_t kW>
static void fill_random_f64(DataGenInt& dg, VecOverlay<kW>& dst) noexcept {
  for (uint32_t i = 0; i < kW / 8u; i++) {
    dst.data_f64[i] = dg.next_float64();
  }
}

template<uint32_t kW>
static void fill_random_data(DataGenInt& dg, VecOverlay<kW>& dst, VecElementType element_type) noexcept {
  switch (element_type) {
    case VecElementType::kFloat32:
      fill_random_f32(dg, dst);
      break;

    case VecElementType::kFloat64:
      fill_random_f64(dg, dst);
      break;

    default:
      fill_random_bytes(dg, dst);
      break;
  }
}

// ujit::UniCompiler - Tests - SIMD - Verification
// ===============================================

template<uint32_t kW>
static ASMJIT_NOINLINE void test_vecop_vv_failed(UniOpVV op, Variation variation, const VecOverlay<kW>& arg0, const VecOverlay<kW>& observed, const VecOverlay<kW>& expected, const char* assembly) noexcept {
  VecOpInfo op_info = vec_op_info_vv(op);

  String arg0_str = vec_stringify(arg0, op_info.arg(0));
  String observed_str = vec_stringify(observed, op_info.ret());
  String expected_str = vec_stringify(expected, op_info.ret());

  EXPECT(false)
    .message("Operation '%s' (variation %u) failed:\n"
             "  Input #1: %s\n"
             "  Expected: %s\n"
             "  Observed: %s\n"
             "Assembly:\n%s",
             vec_op_name_vv(op),
             variation.value,
             arg0_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<uint32_t kW>
static ASMJIT_NOINLINE void test_vecop_vvi_failed(UniOpVVI op, Variation variation, const VecOverlay<kW>& arg0, const VecOverlay<kW>& observed, const VecOverlay<kW>& expected, uint32_t imm, const char* assembly) noexcept {
  VecOpInfo op_info = vec_op_info_vvi(op);

  String arg0_str = vec_stringify(arg0, op_info.arg(0));
  String observed_str = vec_stringify(observed, op_info.ret());
  String expected_str = vec_stringify(expected, op_info.ret());

  EXPECT(false)
    .message("Operation '%s' (variation %u) failed:\n"
             "  Input #1: %s\n"
             "  ImmValue: %u (0x%08X)\n"
             "  Expected: %s\n"
             "  Observed: %s\n"
             "Assembly:\n%s",
             vec_op_name_vvi(op),
             variation.value,
             arg0_str.data(),
             imm, imm,
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<uint32_t kW>
static ASMJIT_NOINLINE void test_vecop_vvv_failed(UniOpVVV op, Variation variation, const VecOverlay<kW>& arg0, const VecOverlay<kW>& arg1, const VecOverlay<kW>& observed, const VecOverlay<kW>& expected, const char* assembly) noexcept {
  VecOpInfo op_info = vec_op_info_vvv(op);

  String arg0_str = vec_stringify(arg0, op_info.arg(0));
  String arg1_str = vec_stringify(arg1, op_info.arg(1));
  String observed_str = vec_stringify(observed, op_info.ret());
  String expected_str = vec_stringify(expected, op_info.ret());

  EXPECT(false)
    .message("Operation '%s' (variation %u) failed:\n"
             "  Input #1: %s\n"
             "  Input #2: %s\n"
             "  Expected: %s\n"
             "  Observed: %s\n"
             "Assembly:\n%s",
             vec_op_name_vvv(op),
             variation.value,
             arg0_str.data(),
             arg1_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<uint32_t kW>
static ASMJIT_NOINLINE void test_vecop_vvvi_failed(UniOpVVVI op, Variation variation, const VecOverlay<kW>& arg0, const VecOverlay<kW>& arg1, const VecOverlay<kW>& observed, const VecOverlay<kW>& expected, uint32_t imm, const char* assembly) noexcept {
  VecOpInfo op_info = vec_op_info_vvvi(op);

  String arg0_str = vec_stringify(arg0, op_info.arg(0));
  String arg1_str = vec_stringify(arg1, op_info.arg(1));
  String observed_str = vec_stringify(observed, op_info.ret());
  String expected_str = vec_stringify(expected, op_info.ret());

  EXPECT(false)
    .message("Operation '%s' (variation %u) failed:\n"
             "  Input #1: %s\n"
             "  Input #2: %s\n"
             "  ImmValue: %u (0x%08X)\n"
             "  Expected: %s\n"
             "  Observed: %s\n"
             "Assembly:\n%s",
             vec_op_name_vvvi(op),
             variation.value,
             arg0_str.data(),
             arg1_str.data(),
             imm, imm,
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<uint32_t kW>
static ASMJIT_NOINLINE void test_vecop_vvvv_failed(UniOpVVVV op, Variation variation, const VecOverlay<kW>& arg0, const VecOverlay<kW>& arg1, const VecOverlay<kW>& arg2, const VecOverlay<kW>& observed, const VecOverlay<kW>& expected, const char* assembly) noexcept {
  VecOpInfo op_info = vec_op_info_vvvv(op);

  String arg0_str = vec_stringify(arg0, op_info.arg(0));
  String arg1_str = vec_stringify(arg1, op_info.arg(1));
  String arg2_str = vec_stringify(arg2, op_info.arg(2));
  String observed_str = vec_stringify(observed, op_info.ret());
  String expected_str = vec_stringify(expected, op_info.ret());

  EXPECT(false)
    .message("Operation '%s' (variation %u) failed\n"
             "  Input #1: %s\n"
             "  Input #2: %s\n"
             "  Input #3: %s\n"
             "  Expected: %s\n"
             "  Observed: %s\n"
             "Assembly:\n%s",
             vec_op_name_vvvv(op),
             variation.value,
             arg0_str.data(),
             arg1_str.data(),
             arg2_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

// ujit::UniCompiler - Tests - Basics - Condition Building
// =======================================================

TEST_CASE(uni_compiler_condition_building) {
  UniGp lhs_reg(OperandSignature::from_reg_type_and_group(RegType::kGp32, RegGroup::kGp), 3);
  UniGp rhs_reg(OperandSignature::from_reg_type_and_group(RegType::kGp32, RegGroup::kGp), 7);
  Mem rhs_mem = mem_ptr(rhs_reg);
  Imm rhs_imm = imm(100);

  auto expect_uni_cond = [&](UniCondition uc, UniOpCond op, CondCode cond) {
    EXPECT_EQ(uc.a, lhs_reg);
    if (uc.b.is_reg()) {
      EXPECT_EQ(uc.b, rhs_reg);
    }
    if (uc.b.is_mem()) {
      EXPECT_EQ(uc.b, rhs_mem);
    }
    if (uc.b.is_imm()) {
      EXPECT_EQ(uc.b, rhs_imm);
    }

    EXPECT_EQ(uc.op, op);
    EXPECT_EQ(uc.cond, cond);
  };

  expect_uni_cond(and_z(lhs_reg, rhs_reg), UniOpCond::kAssignAnd, CondCode::kZero);
  expect_uni_cond(and_z(lhs_reg, rhs_mem), UniOpCond::kAssignAnd, CondCode::kZero);
  expect_uni_cond(and_z(lhs_reg, rhs_imm), UniOpCond::kAssignAnd, CondCode::kZero);
  expect_uni_cond(and_nz(lhs_reg, rhs_reg), UniOpCond::kAssignAnd, CondCode::kNotZero);
  expect_uni_cond(and_nz(lhs_reg, rhs_mem), UniOpCond::kAssignAnd, CondCode::kNotZero);
  expect_uni_cond(and_nz(lhs_reg, rhs_imm), UniOpCond::kAssignAnd, CondCode::kNotZero);

  expect_uni_cond(or_z(lhs_reg, rhs_reg), UniOpCond::kAssignOr, CondCode::kZero);
  expect_uni_cond(or_z(lhs_reg, rhs_mem), UniOpCond::kAssignOr, CondCode::kZero);
  expect_uni_cond(or_z(lhs_reg, rhs_imm), UniOpCond::kAssignOr, CondCode::kZero);
  expect_uni_cond(or_nz(lhs_reg, rhs_reg), UniOpCond::kAssignOr, CondCode::kNotZero);
  expect_uni_cond(or_nz(lhs_reg, rhs_mem), UniOpCond::kAssignOr, CondCode::kNotZero);
  expect_uni_cond(or_nz(lhs_reg, rhs_imm), UniOpCond::kAssignOr, CondCode::kNotZero);

  expect_uni_cond(xor_z(lhs_reg, rhs_reg), UniOpCond::kAssignXor, CondCode::kZero);
  expect_uni_cond(xor_z(lhs_reg, rhs_mem), UniOpCond::kAssignXor, CondCode::kZero);
  expect_uni_cond(xor_z(lhs_reg, rhs_imm), UniOpCond::kAssignXor, CondCode::kZero);
  expect_uni_cond(xor_nz(lhs_reg, rhs_reg), UniOpCond::kAssignXor, CondCode::kNotZero);
  expect_uni_cond(xor_nz(lhs_reg, rhs_mem), UniOpCond::kAssignXor, CondCode::kNotZero);
  expect_uni_cond(xor_nz(lhs_reg, rhs_imm), UniOpCond::kAssignXor, CondCode::kNotZero);

  expect_uni_cond(add_z(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kZero);
  expect_uni_cond(add_z(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kZero);
  expect_uni_cond(add_z(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kZero);
  expect_uni_cond(add_nz(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kNotZero);
  expect_uni_cond(add_nz(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kNotZero);
  expect_uni_cond(add_nz(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kNotZero);

  expect_uni_cond(add_c(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kCarry);
  expect_uni_cond(add_c(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kCarry);
  expect_uni_cond(add_c(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kCarry);
  expect_uni_cond(add_nc(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kNotCarry);
  expect_uni_cond(add_nc(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kNotCarry);
  expect_uni_cond(add_nc(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kNotCarry);

  expect_uni_cond(add_s(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kSign);
  expect_uni_cond(add_s(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kSign);
  expect_uni_cond(add_s(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kSign);
  expect_uni_cond(add_ns(lhs_reg, rhs_reg), UniOpCond::kAssignAdd, CondCode::kNotSign);
  expect_uni_cond(add_ns(lhs_reg, rhs_mem), UniOpCond::kAssignAdd, CondCode::kNotSign);
  expect_uni_cond(add_ns(lhs_reg, rhs_imm), UniOpCond::kAssignAdd, CondCode::kNotSign);

  expect_uni_cond(sub_z(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kZero);
  expect_uni_cond(sub_z(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kZero);
  expect_uni_cond(sub_z(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kZero);
  expect_uni_cond(sub_nz(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kNotZero);
  expect_uni_cond(sub_nz(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kNotZero);
  expect_uni_cond(sub_nz(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kNotZero);

  // X86 vs AArch64 - X86 uses regular carry, but AArch64 has it reversed - so UniCompiler uses UnsignedLT/GE.
  expect_uni_cond(sub_c(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kUnsignedLT);
  expect_uni_cond(sub_c(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kUnsignedLT);
  expect_uni_cond(sub_c(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kUnsignedLT);
  expect_uni_cond(sub_nc(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kUnsignedGE);
  expect_uni_cond(sub_nc(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kUnsignedGE);
  expect_uni_cond(sub_nc(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kUnsignedGE);

  expect_uni_cond(sub_s(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kSign);
  expect_uni_cond(sub_s(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kSign);
  expect_uni_cond(sub_s(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kSign);
  expect_uni_cond(sub_ns(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kNotSign);
  expect_uni_cond(sub_ns(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kNotSign);
  expect_uni_cond(sub_ns(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kNotSign);

  expect_uni_cond(sub_ugt(lhs_reg, rhs_reg), UniOpCond::kAssignSub, CondCode::kUnsignedGT);
  expect_uni_cond(sub_ugt(lhs_reg, rhs_mem), UniOpCond::kAssignSub, CondCode::kUnsignedGT);
  expect_uni_cond(sub_ugt(lhs_reg, rhs_imm), UniOpCond::kAssignSub, CondCode::kUnsignedGT);

  expect_uni_cond(shr_z(lhs_reg, rhs_reg), UniOpCond::kAssignShr, CondCode::kZero);
  expect_uni_cond(shr_z(lhs_reg, rhs_mem), UniOpCond::kAssignShr, CondCode::kZero);
  expect_uni_cond(shr_z(lhs_reg, rhs_imm), UniOpCond::kAssignShr, CondCode::kZero);
  expect_uni_cond(shr_nz(lhs_reg, rhs_reg), UniOpCond::kAssignShr, CondCode::kNotZero);
  expect_uni_cond(shr_nz(lhs_reg, rhs_mem), UniOpCond::kAssignShr, CondCode::kNotZero);
  expect_uni_cond(shr_nz(lhs_reg, rhs_imm), UniOpCond::kAssignShr, CondCode::kNotZero);

  expect_uni_cond(cmp_eq(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kEqual);
  expect_uni_cond(cmp_eq(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kEqual);
  expect_uni_cond(cmp_eq(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kEqual);
  expect_uni_cond(cmp_ne(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kNotEqual);
  expect_uni_cond(cmp_ne(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kNotEqual);
  expect_uni_cond(cmp_ne(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kNotEqual);

  expect_uni_cond(scmp_lt(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kSignedLT);
  expect_uni_cond(scmp_lt(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kSignedLT);
  expect_uni_cond(scmp_lt(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kSignedLT);
  expect_uni_cond(scmp_le(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kSignedLE);
  expect_uni_cond(scmp_le(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kSignedLE);
  expect_uni_cond(scmp_le(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kSignedLE);
  expect_uni_cond(scmp_gt(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kSignedGT);
  expect_uni_cond(scmp_gt(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kSignedGT);
  expect_uni_cond(scmp_gt(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kSignedGT);
  expect_uni_cond(scmp_ge(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kSignedGE);
  expect_uni_cond(scmp_ge(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kSignedGE);
  expect_uni_cond(scmp_ge(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kSignedGE);

  expect_uni_cond(ucmp_lt(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kUnsignedLT);
  expect_uni_cond(ucmp_lt(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kUnsignedLT);
  expect_uni_cond(ucmp_lt(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kUnsignedLT);
  expect_uni_cond(ucmp_le(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kUnsignedLE);
  expect_uni_cond(ucmp_le(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kUnsignedLE);
  expect_uni_cond(ucmp_le(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kUnsignedLE);
  expect_uni_cond(ucmp_gt(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kUnsignedGT);
  expect_uni_cond(ucmp_gt(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kUnsignedGT);
  expect_uni_cond(ucmp_gt(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kUnsignedGT);
  expect_uni_cond(ucmp_ge(lhs_reg, rhs_reg), UniOpCond::kCompare, CondCode::kUnsignedGE);
  expect_uni_cond(ucmp_ge(lhs_reg, rhs_mem), UniOpCond::kCompare, CondCode::kUnsignedGE);
  expect_uni_cond(ucmp_ge(lhs_reg, rhs_imm), UniOpCond::kCompare, CondCode::kUnsignedGE);

  expect_uni_cond(test_z(lhs_reg, rhs_reg), UniOpCond::kTest, CondCode::kZero);
  expect_uni_cond(test_z(lhs_reg, rhs_mem), UniOpCond::kTest, CondCode::kZero);
  expect_uni_cond(test_z(lhs_reg, rhs_imm), UniOpCond::kTest, CondCode::kZero);
  expect_uni_cond(test_nz(lhs_reg, rhs_reg), UniOpCond::kTest, CondCode::kNotZero);
  expect_uni_cond(test_nz(lhs_reg, rhs_mem), UniOpCond::kTest, CondCode::kNotZero);
  expect_uni_cond(test_nz(lhs_reg, rhs_imm), UniOpCond::kTest, CondCode::kNotZero);

  expect_uni_cond(bt_z(lhs_reg, rhs_reg), UniOpCond::kBitTest, CondCode::kBTZero);
  expect_uni_cond(bt_z(lhs_reg, rhs_mem), UniOpCond::kBitTest, CondCode::kBTZero);
  expect_uni_cond(bt_z(lhs_reg, rhs_imm), UniOpCond::kBitTest, CondCode::kBTZero);
  expect_uni_cond(bt_nz(lhs_reg, rhs_reg), UniOpCond::kBitTest, CondCode::kBTNotZero);
  expect_uni_cond(bt_nz(lhs_reg, rhs_mem), UniOpCond::kBitTest, CondCode::kBTNotZero);
  expect_uni_cond(bt_nz(lhs_reg, rhs_imm), UniOpCond::kBitTest, CondCode::kBTNotZero);
}


// ujit::UniCompiler - Tests - Integer Operations - VV
// ===================================================

template<VecWidth kVecWidth, UniOpVV kOp, typename GenericOp, typename Constraint>
static ASMJIT_NOINLINE void test_vecop_vv_constraint(JitContext& ctx, Variation variation = Variation{0}) {
  constexpr uint32_t kW = byte_width_from_vec_width(kVecWidth);

  TestVVFunc compiled_apply = create_func_vv(ctx, kVecWidth, kOp, variation);
  DataGenInt dg(kRandomSeed);

  VecOpInfo op_info = vec_op_info_vv(kOp);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW> a {};
    VecOverlay<kW> observed {};
    VecOverlay<kW> expected {};

    fill_random_data(dg, a, op_info.arg(0));
    Constraint::apply(a);

    compiled_apply(&observed, &a);
    expected = GenericOp::apply(a);

    if (!vec_eq(observed, expected, op_info.ret())) {
      test_vecop_vv_failed(kOp, variation, a, observed, expected, ctx.logger_content());
    }
  }

  ctx.rt.release(compiled_apply);
}

template<VecWidth kVecWidth, UniOpVV kOp, typename GenericOp>
static void test_vecop_vv(JitContext& ctx, Variation variation = Variation{0}) {
  return test_vecop_vv_constraint<kVecWidth, kOp, GenericOp, ConstraintNone>(ctx, variation);
}

// ujit::UniCompiler - Tests - SIMD - Integer Operations - VVI
// ===========================================================

template<VecWidth kVecWidth, UniOpVVI kOp, typename GenericOp, typename Constraint>
static ASMJIT_NOINLINE void test_vecop_vvi_constraint(JitContext& ctx, uint32_t imm, Variation variation = Variation{0}) {
  constexpr uint32_t kW = byte_width_from_vec_width(kVecWidth);

  TestVVFunc compiled_apply = create_func_vvi(ctx, kVecWidth, kOp, imm, variation);
  DataGenInt dg(kRandomSeed);

  VecOpInfo op_info = vec_op_info_vvi(kOp);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW> a {};
    VecOverlay<kW> observed {};
    VecOverlay<kW> expected {};

    fill_random_data(dg, a, op_info.arg(0));
    Constraint::apply(a);

    compiled_apply(&observed, &a);
    expected = GenericOp::apply(a, imm);

    if (!vec_eq(observed, expected, op_info.ret())) {
      test_vecop_vvi_failed(kOp, variation, a, observed, expected, imm, ctx.logger_content());
    }
  }

  ctx.rt.release(compiled_apply);
}

template<VecWidth kVecWidth, UniOpVVI kOp, typename GenericOp>
static void test_vecop_vvi(JitContext& ctx, uint32_t imm, Variation variation = Variation{0}) {
  return test_vecop_vvi_constraint<kVecWidth, kOp, GenericOp, ConstraintNone>(ctx, imm, variation);
}

// ujit::UniCompiler - Tests - SIMD - Integer Operations - VVV
// ===========================================================

template<VecWidth kVecWidth, UniOpVVV kOp, typename GenericOp, typename Constraint>
static ASMJIT_NOINLINE void test_vecop_vvv_constraint(JitContext& ctx, Variation variation = Variation{0}) {
  constexpr uint32_t kW = byte_width_from_vec_width(kVecWidth);

  TestVVVFunc compiled_apply = create_func_vvv(ctx, kVecWidth, kOp, variation);
  DataGenInt dg(kRandomSeed);

  VecOpInfo op_info = vec_op_info_vvv(kOp);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW> a {};
    VecOverlay<kW> b {};
    VecOverlay<kW> observed {};
    VecOverlay<kW> expected {};

    fill_random_data(dg, a, op_info.arg(0));
    fill_random_data(dg, b, op_info.arg(1));
    Constraint::apply(a);
    Constraint::apply(b);

    compiled_apply(&observed, &a, &b);
    expected = GenericOp::apply(a, b);

    if (!vec_eq(observed, expected, op_info.ret())) {
      test_vecop_vvv_failed(kOp, variation, a, b, observed, expected, ctx.logger_content());
    }
  }

  ctx.rt.release(compiled_apply);
}

template<VecWidth kVecWidth, UniOpVVV kOp, typename GenericOp>
static void test_vecop_vvv(JitContext& ctx, Variation variation = Variation{0}) {
  return test_vecop_vvv_constraint<kVecWidth, kOp, GenericOp, ConstraintNone>(ctx, variation);
}

// ujit::UniCompiler - Tests - SIMD - Integer Operations - VVVI
// ============================================================

template<VecWidth kVecWidth, UniOpVVVI kOp, typename GenericOp, typename Constraint>
static ASMJIT_NOINLINE void test_vecop_vvvi_constraint(JitContext& ctx, uint32_t imm, Variation variation = Variation{0}) {
  constexpr uint32_t kW = byte_width_from_vec_width(kVecWidth);

  TestVVVFunc compiled_apply = create_func_vvvi(ctx, kVecWidth, kOp, imm, variation);
  DataGenInt dg(kRandomSeed);

  VecOpInfo op_info = vec_op_info_vvvi(kOp);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW> a {};
    VecOverlay<kW> b {};
    VecOverlay<kW> observed {};
    VecOverlay<kW> expected {};

    fill_random_data(dg, a, op_info.arg(0));
    fill_random_data(dg, b, op_info.arg(1));
    Constraint::apply(a);
    Constraint::apply(b);

    compiled_apply(&observed, &a, &b);
    expected = GenericOp::apply(a, b, imm);

    if (!vec_eq(observed, expected, op_info.ret())) {
      test_vecop_vvvi_failed(kOp, variation, a, b, observed, expected, imm, ctx.logger_content());
    }
  }

  ctx.rt.release(compiled_apply);
}

template<VecWidth kVecWidth, UniOpVVVI kOp, typename GenericOp>
static void test_vecop_vvvi(JitContext& ctx, uint32_t imm, Variation variation = Variation{0}) {
  return test_vecop_vvvi_constraint<kVecWidth, kOp, GenericOp, ConstraintNone>(ctx, imm, variation);
}

// ujit::UniCompiler - Tests - SIMD - Integer Operations - VVVV
// ============================================================

template<VecWidth kVecWidth, UniOpVVVV kOp, typename GenericOp, typename Constraint>
static ASMJIT_NOINLINE void test_vecop_vvvv_constraint(JitContext& ctx, Variation variation = Variation{0}) {
  constexpr uint32_t kW = byte_width_from_vec_width(kVecWidth);

  TestVVVVFunc compiled_apply = create_func_vvvv(ctx, kVecWidth, kOp, variation);
  DataGenInt dg(kRandomSeed);

  VecOpInfo op_info = vec_op_info_vvvv(kOp);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW> a {};
    VecOverlay<kW> b {};
    VecOverlay<kW> c {};
    VecOverlay<kW> observed {};
    VecOverlay<kW> expected {};

    fill_random_data(dg, a, op_info.arg(0));
    fill_random_data(dg, b, op_info.arg(1));
    fill_random_data(dg, c, op_info.arg(2));
    Constraint::apply(a);
    Constraint::apply(b);
    Constraint::apply(c);

    compiled_apply(&observed, &a, &b, &c);
    expected = GenericOp::apply(a, b, c);

    if (!vec_eq(observed, expected, op_info.ret())) {
      test_vecop_vvvv_failed(kOp, variation, a, b, c, observed, expected, ctx.logger_content());
    }
  }
}

template<VecWidth kVecWidth, UniOpVVVV kOp, typename GenericOp>
static void test_vecop_vvvv(JitContext& ctx, Variation variation = Variation{0}) {
  return test_vecop_vvvv_constraint<kVecWidth, kOp, GenericOp, ConstraintNone>(ctx, variation);
}

// ujit::UniCompiler - Tests - SIMD - Runner
// =========================================

template<VecWidth kVecWidth>
static ASMJIT_NOINLINE void test_simd_ops(JitContext& ctx) {
  // We need to know some behaviors in advance so we can select the right test function,
  // so create a dummy compiler and extract the necessary information from it.
  ScalarOpBehavior scalar_op_behavior {};
  FMAddOpBehavior fmadd_op_behavior {};
  FloatToIntOutsideRangeBehavior float_to_int_behavior {};

  {
    ctx.prepare();
    UniCompiler uc(&ctx.cc, ctx.features, ctx.cpu_hints);

    scalar_op_behavior = uc.scalar_op_behavior();
    fmadd_op_behavior = uc.fmadd_op_behavior();
    float_to_int_behavior = uc.float_to_int_outside_range_behavior();
  }

  TEST_LOG("  Testing mov");
  {
    test_vecop_vv<kVecWidth, UniOpVV::kMov, vec_op_mov>(ctx);
    test_vecop_vv<kVecWidth, UniOpVV::kMovU64, vec_op_mov_u64>(ctx);
  }

  TEST_LOG("  Testing broadcast");
  {
    // Test all broadcasts - vector based, GP to vector, and memory to vector.
    for (uint32_t v = 0; v < kNumVariationsVV_Broadcast; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU8Z, vec_op_broadcast_u8>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU16Z, vec_op_broadcast_u16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU8, vec_op_broadcast_u8>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU16, vec_op_broadcast_u16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU32, vec_op_broadcast_u32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastU64, vec_op_broadcast_u64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastF32, vec_op_broadcast_u32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastF64, vec_op_broadcast_u64>(ctx, Variation{v});

      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV128_U32, vec_op_broadcast_u128>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV128_U64, vec_op_broadcast_u128>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV128_F32, vec_op_broadcast_u128>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV128_F64, vec_op_broadcast_u128>(ctx, Variation{v});

      if constexpr (kVecWidth > VecWidth::k256) {
        test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV256_U32, vec_op_broadcast_u256>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV256_U64, vec_op_broadcast_u256>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV256_F32, vec_op_broadcast_u256>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kBroadcastV256_F64, vec_op_broadcast_u256>(ctx, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing abs (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kAbsI8, vec_op_abs<int8_t>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kAbsI16, vec_op_abs<int16_t>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kAbsI32, vec_op_abs<int32_t>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kAbsI64, vec_op_abs<int64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing not (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kNotU32, vec_op_not<uint32_t>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kNotU64, vec_op_not<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing cvt (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI8LoToI16, vec_op_cvt_i8_lo_to_i16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI8HiToI16, vec_op_cvt_i8_hi_to_i16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU8LoToU16, vec_op_cvt_u8_lo_to_u16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU8HiToU16, vec_op_cvt_u8_hi_to_u16>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI8ToI32, vec_op_cvt_i8_to_i32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU8ToU32, vec_op_cvt_u8_to_u32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI16LoToI32, vec_op_cvt_i16_lo_to_i32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI16HiToI32, vec_op_cvt_i16_hi_to_i32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU16LoToU32, vec_op_cvt_u16_lo_to_u32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU16HiToU32, vec_op_cvt_u16_hi_to_u32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI32LoToI64, vec_op_cvt_i32_lo_to_i64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI32HiToI64, vec_op_cvt_i32_hi_to_i64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU32LoToU64, vec_op_cvt_u32_lo_to_u64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtU32HiToU64, vec_op_cvt_u32_hi_to_u64>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing abs (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kAbsF32, vec_op_fabs<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kAbsF64, vec_op_fabs<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing not (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kNotF32, vec_op_not<uint32_t>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kNotF64, vec_op_not<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing rounding (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      // Variation 2 means that the source operand is memory, which would ALWAYS zero the rest of the register.
      if (scalar_op_behavior == ScalarOpBehavior::kZeroing || v == 2u) {
        test_vecop_vv<kVecWidth, UniOpVV::kTruncF32S, scalar_op_trunc<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kTruncF64S, scalar_op_trunc<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kFloorF32S, scalar_op_floor<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kFloorF64S, scalar_op_floor<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCeilF32S, scalar_op_ceil<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCeilF64S, scalar_op_ceil<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF32S, scalar_op_round_even<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF64S, scalar_op_round_even<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF32S, scalar_op_round_half_away<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF64S, scalar_op_round_half_away<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF32S, scalar_op_round_half_up<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF64S, scalar_op_round_half_up<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{v});
      }
      else {
        test_vecop_vv<kVecWidth, UniOpVV::kTruncF32S, scalar_op_trunc<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kTruncF64S, scalar_op_trunc<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kFloorF32S, scalar_op_floor<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kFloorF64S, scalar_op_floor<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCeilF32S, scalar_op_ceil<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCeilF64S, scalar_op_ceil<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF32S, scalar_op_round_even<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF64S, scalar_op_round_even<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF32S, scalar_op_round_half_away<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF64S, scalar_op_round_half_away<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF32S, scalar_op_round_half_up<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF64S, scalar_op_round_half_up<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{v});
      }

      test_vecop_vv<kVecWidth, UniOpVV::kTruncF32, vec_op_trunc<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kTruncF64, vec_op_trunc<double>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kFloorF32, vec_op_floor<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kFloorF64, vec_op_floor<double>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCeilF32, vec_op_ceil<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCeilF64, vec_op_ceil<double>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF32, vec_op_round_even<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundEvenF64, vec_op_round_even<double>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF32, vec_op_round_half_away<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfAwayF64, vec_op_round_half_away<double>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF32, vec_op_round_half_up<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRoundHalfUpF64, vec_op_round_half_up<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing rcp (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kRcpF32, vec_op_rcp<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kRcpF64, vec_op_rcp<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing sqrt (float)");
  {
    if (scalar_op_behavior == ScalarOpBehavior::kZeroing) {
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF32S, scalar_op_sqrt<ScalarOpBehavior::kZeroing, float>>(ctx);
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF64S, scalar_op_sqrt<ScalarOpBehavior::kZeroing, double>>(ctx);
    }
    else {
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF32S, scalar_op_sqrt<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF64S, scalar_op_sqrt<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
    }

    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF32, vec_op_sqrt<float>>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kSqrtF64, vec_op_sqrt<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing cvt (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVV; v++) {
      // TODO: [JIT] Re-enable when the content of the remaining part of the register is formalized.
      // test_vecop_vv<kVecWidth, UniOpVV::kCvtF32ToF64S, scalar_op_cvt_f32_to_f64>(ctx);
      // test_vecop_vv<kVecWidth, UniOpVV::kCvtF64ToF32S, scalar_op_cvt_f64_to_f32>(ctx);

      test_vecop_vv<kVecWidth, UniOpVV::kCvtI32ToF32, vec_op_cvt_i32_to_f32>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtF32LoToF64, vec_op_cvt_f32_lo_to_f64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtF32HiToF64, vec_op_cvt_f32_hi_to_f64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtF64ToF32Lo, vec_op_cvt_f64_to_f32_lo>(ctx, Variation{0});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtF64ToF32Hi, vec_op_cvt_f64_to_f32_hi>(ctx, Variation{0});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI32LoToF64, vec_op_cvt_i32_lo_to_f64>(ctx, Variation{v});
      test_vecop_vv<kVecWidth, UniOpVV::kCvtI32HiToF64, vec_op_cvt_i32_hi_to_f64>(ctx, Variation{v});

      if (float_to_int_behavior == FloatToIntOutsideRangeBehavior::kSmallestValue) {
        constexpr FloatToIntOutsideRangeBehavior behavior = FloatToIntOutsideRangeBehavior::kSmallestValue;
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF32ToI32, vec_op_cvt_trunc_f32_to_i32<behavior>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF64ToI32Lo, vec_op_cvt_trunc_f64_to_i32_lo<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF64ToI32Hi, vec_op_cvt_trunc_f64_to_i32_hi<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF32ToI32, vec_op_cvt_round_f32_to_i32<behavior>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF64ToI32Lo, vec_op_cvt_round_f64_to_i32_lo<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF64ToI32Hi, vec_op_cvt_round_f64_to_i32_hi<behavior>>(ctx, Variation{0});
      }
      else {
        constexpr FloatToIntOutsideRangeBehavior behavior = FloatToIntOutsideRangeBehavior::kSaturatedValue;
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF32ToI32, vec_op_cvt_trunc_f32_to_i32<behavior>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF64ToI32Lo, vec_op_cvt_trunc_f64_to_i32_lo<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtTruncF64ToI32Hi, vec_op_cvt_trunc_f64_to_i32_hi<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF32ToI32, vec_op_cvt_round_f32_to_i32<behavior>>(ctx, Variation{v});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF64ToI32Lo, vec_op_cvt_round_f64_to_i32_lo<behavior>>(ctx, Variation{0});
        test_vecop_vv<kVecWidth, UniOpVV::kCvtRoundF64ToI32Hi, vec_op_cvt_round_f64_to_i32_hi<behavior>>(ctx, Variation{0});
      }
    }
  }

  TEST_LOG("  Testing bit shift");
  {
    for (uint32_t v = 0; v < kNumVariationsVVI; v++) {
/*
      for (uint32_t i = 1; i < 8; i++) {
        test_vecop_vvi<kVecWidth, UniOpVVI::kSllU8 , vec_op_slli<uint8_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSrlU8 , vec_op_srli<uint8_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSraI8 , vec_op_srai<int8_t>>(ctx, i, Variation{v});
      }
*/
      for (uint32_t i = 1; i < 16; i++) {
        test_vecop_vvi<kVecWidth, UniOpVVI::kSllU16, vec_op_slli<uint16_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSrlU16, vec_op_srli<uint16_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSraI16, vec_op_srai<int16_t>>(ctx, i, Variation{v});
      }

      for (uint32_t i = 1; i < 32; i++) {
        test_vecop_vvi<kVecWidth, UniOpVVI::kSllU32, vec_op_slli<uint32_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSrlU32, vec_op_srli<uint32_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSraI32, vec_op_srai<int32_t>>(ctx, i, Variation{v});
      }

      for (uint32_t i = 1; i < 64; i++) {
        test_vecop_vvi<kVecWidth, UniOpVVI::kSllU64, vec_op_slli<uint64_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSrlU64, vec_op_srli<uint64_t>>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSraI64, vec_op_srai<int64_t>>(ctx, i, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing sllb_u128 & srlb_u128");
  {
    for (uint32_t v = 0; v < kNumVariationsVVI; v++) {
      for (uint32_t i = 1; i < 16; i++) {
        test_vecop_vvi<kVecWidth, UniOpVVI::kSllbU128, vec_op_sllb_u128>(ctx, i, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSrlbU128, vec_op_srlb_u128>(ctx, i, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing swizzle_[lo|hi]_u16x4");
  {
    for (uint32_t v = 0; v < kNumVariationsVVI; v++) {
      for (uint32_t i = 0; i < 256; i++) {
        uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleLoU16x4, vec_op_swizzle_lo_u16x4>(ctx, imm, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleHiU16x4, vec_op_swizzle_hi_u16x4>(ctx, imm, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleU16x4, vec_op_swizzle_u16>(ctx, imm, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing swizzle_u32x4");
  {
    for (uint32_t v = 0; v < kNumVariationsVVI; v++) {
      for (uint32_t i = 0; i < 256; i++) {
        uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleU32x4, vec_op_swizzle_u32x4>(ctx, imm, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleF32x4, vec_op_swizzle_u32x4>(ctx, imm, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing swizzle_u64x2");
  {
    for (uint32_t v = 0; v < kNumVariationsVVI; v++) {
      for (uint32_t i = 0; i < 4; i++) {
        uint32_t imm = swizzle((i >> 1) & 1, i & 1).value;
        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleU64x2, vec_op_swizzle_u64x2>(ctx, imm, Variation{v});
        test_vecop_vvi<kVecWidth, UniOpVVI::kSwizzleF64x2, vec_op_swizzle_u64x2>(ctx, imm, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing logical (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndU32, vec_op_and<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndU64, vec_op_and<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kOrU32, vec_op_or<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kOrU64, vec_op_or<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kXorU32, vec_op_xor<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kXorU64, vec_op_xor<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndnU32, vec_op_andn<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndnU64, vec_op_andn<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kBicU32, vec_op_bic<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kBicU64, vec_op_bic<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing add / adds (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddU8, vec_op_add<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddU16, vec_op_add<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddU32, vec_op_add<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddU64, vec_op_add<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddsI8, vec_op_adds<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddsI16, vec_op_adds<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddsU8, vec_op_adds<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddsU16, vec_op_adds<uint16_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing sub / subs (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubU8, vec_op_sub<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubU16, vec_op_sub<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubU32, vec_op_sub<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubU64, vec_op_sub<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubsI8, vec_op_subs<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubsI16, vec_op_subs<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubsU8, vec_op_subs<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubsU16, vec_op_subs<uint16_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing mul (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulU16, vec_op_mul<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulU32, vec_op_mul<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulU64, vec_op_mul<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulhI16, vec_op_mulhi<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulhU16, vec_op_mulhu<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulU64_LoU32, vec_op_mul_u64_lo_u32>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing mhadd (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kMHAddI16_I32, vec_op_mhadd_i16_i32>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing madd (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVVV; v++) {
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddU16, vec_op_madd<uint16_t>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddU32, vec_op_madd<uint32_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing min / max (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinI8, vec_op_min<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinI16, vec_op_min<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinI32, vec_op_min<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinI64, vec_op_min<int64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinU8, vec_op_min<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinU16, vec_op_min<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinU32, vec_op_min<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinU64, vec_op_min<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxI8, vec_op_max<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxI16, vec_op_max<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxI32, vec_op_max<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxI64, vec_op_max<int64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxU8, vec_op_max<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxU16, vec_op_max<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxU32, vec_op_max<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxU64, vec_op_max<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing cmp (int)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqU8, vec_op_cmp_eq<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqU16, vec_op_cmp_eq<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqU32, vec_op_cmp_eq<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqU64, vec_op_cmp_eq<uint64_t>>(ctx, Variation{v});
/*
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeU8, vec_op_cmp_ne<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeU16, vec_op_cmp_ne<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeU32, vec_op_cmp_ne<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeU64, vec_op_cmp_ne<uint64_t>>(ctx, Variation{v});
*/
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtI8, vec_op_cmp_gt<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtI16, vec_op_cmp_gt<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtI32, vec_op_cmp_gt<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtI64, vec_op_cmp_gt<int64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtU8, vec_op_cmp_gt<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtU16, vec_op_cmp_gt<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtU32, vec_op_cmp_gt<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtU64, vec_op_cmp_gt<uint64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeI8, vec_op_cmp_ge<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeI16, vec_op_cmp_ge<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeI32, vec_op_cmp_ge<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeI64, vec_op_cmp_ge<int64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeU8, vec_op_cmp_ge<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeU16, vec_op_cmp_ge<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeU32, vec_op_cmp_ge<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeU64, vec_op_cmp_ge<uint64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtI8, vec_op_cmp_lt<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtI16, vec_op_cmp_lt<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtI32, vec_op_cmp_lt<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtI64, vec_op_cmp_lt<int64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtU8, vec_op_cmp_lt<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtU16, vec_op_cmp_lt<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtU32, vec_op_cmp_lt<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtU64, vec_op_cmp_lt<uint64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeI8, vec_op_cmp_le<int8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeI16, vec_op_cmp_le<int16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeI32, vec_op_cmp_le<int32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeI64, vec_op_cmp_le<int64_t>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeU8, vec_op_cmp_le<uint8_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeU16, vec_op_cmp_le<uint16_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeU32, vec_op_cmp_le<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeU64, vec_op_cmp_le<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing logical (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndF32, vec_op_and<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndF64, vec_op_and<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kOrF32, vec_op_or<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kOrF64, vec_op_or<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kXorF32, vec_op_xor<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kXorF64, vec_op_xor<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndnF32, vec_op_andn<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAndnF64, vec_op_andn<uint64_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kBicF32, vec_op_bic<uint32_t>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kBicF64, vec_op_bic<uint64_t>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing arithmetic (float)");
  {
    if (scalar_op_behavior == ScalarOpBehavior::kZeroing) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF32S, scalar_op_fadd<ScalarOpBehavior::kZeroing, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF64S, scalar_op_fadd<ScalarOpBehavior::kZeroing, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF32S, scalar_op_fsub<ScalarOpBehavior::kZeroing, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF64S, scalar_op_fsub<ScalarOpBehavior::kZeroing, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF32S, scalar_op_fmul<ScalarOpBehavior::kZeroing, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF64S, scalar_op_fmul<ScalarOpBehavior::kZeroing, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF32S, scalar_op_fdiv<ScalarOpBehavior::kZeroing, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF64S, scalar_op_fdiv<ScalarOpBehavior::kZeroing, double>>(ctx);
    }
    else {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF32S, scalar_op_fadd<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF64S, scalar_op_fadd<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF32S, scalar_op_fsub<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF64S, scalar_op_fsub<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF32S, scalar_op_fmul<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF64S, scalar_op_fmul<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF32S, scalar_op_fdiv<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF64S, scalar_op_fdiv<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
    }

    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF32, vec_op_fadd<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kAddF64, vec_op_fadd<double>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF32, vec_op_fsub<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kSubF64, vec_op_fsub<double>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF32, vec_op_fmul<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMulF64, vec_op_fmul<double>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF32, vec_op_fdiv<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kDivF64, vec_op_fdiv<double>>(ctx, Variation{v});
    }
  }

  if (fmadd_op_behavior == FMAddOpBehavior::kNoFMA) {
    TEST_LOG("  Testing madd (no-fma) (float)");
    {
      if (scalar_op_behavior == ScalarOpBehavior::kZeroing) {
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32S, scalar_op_fmadd_nofma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64S, scalar_op_fmadd_nofma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32S, scalar_op_fmsub_nofma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64S, scalar_op_fmsub_nofma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32S, scalar_op_fnmadd_nofma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64S, scalar_op_fnmadd_nofma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32S, scalar_op_fnmsub_nofma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64S, scalar_op_fnmsub_nofma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
      }
      else {
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32S, scalar_op_fmadd_nofma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64S, scalar_op_fmadd_nofma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32S, scalar_op_fmsub_nofma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64S, scalar_op_fmsub_nofma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32S, scalar_op_fnmadd_nofma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64S, scalar_op_fnmadd_nofma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32S, scalar_op_fnmsub_nofma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64S, scalar_op_fnmsub_nofma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
      }

      for (uint32_t v = 0; v < kNumVariationsVVVV; v++) {
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32, vec_op_fmadd_nofma<float>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64, vec_op_fmadd_nofma<double>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32, vec_op_fmsub_nofma<float>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64, vec_op_fmsub_nofma<double>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32, vec_op_fnmadd_nofma<float>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64, vec_op_fnmadd_nofma<double>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32, vec_op_fnmsub_nofma<float>>(ctx, Variation{v});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64, vec_op_fnmsub_nofma<double>>(ctx, Variation{v});
      }
    }
  }
  else {
    TEST_LOG("  Testing madd (fma) (float)");
    {
      if (scalar_op_behavior == ScalarOpBehavior::kZeroing) {
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32S, scalar_op_fmadd_fma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64S, scalar_op_fmadd_fma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32S, scalar_op_fmsub_fma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64S, scalar_op_fmsub_fma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32S, scalar_op_fnmadd_fma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64S, scalar_op_fnmadd_fma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32S, scalar_op_fnmsub_fma<ScalarOpBehavior::kZeroing, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64S, scalar_op_fnmsub_fma<ScalarOpBehavior::kZeroing, double>>(ctx, Variation{0});
      }
      else {
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32S, scalar_op_fmadd_fma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64S, scalar_op_fmadd_fma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32S, scalar_op_fmsub_fma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64S, scalar_op_fmsub_fma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32S, scalar_op_fnmadd_fma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64S, scalar_op_fnmadd_fma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32S, scalar_op_fnmsub_fma<ScalarOpBehavior::kPreservingVec128, float>>(ctx, Variation{0});
        test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64S, scalar_op_fnmsub_fma<ScalarOpBehavior::kPreservingVec128, double>>(ctx, Variation{0});
      }
    }

    for (uint32_t v = 0; v < kNumVariationsVVVV; v++) {
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF32, vec_op_fmadd_fma<float>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMAddF64, vec_op_fmadd_fma<double>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF32, vec_op_fmsub_fma<float>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kMSubF64, vec_op_fmsub_fma<double>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF32, vec_op_fnmadd_fma<float>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMAddF64, vec_op_fnmadd_fma<double>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF32, vec_op_fnmsub_fma<float>>(ctx, Variation{v});
      test_vecop_vvvv<kVecWidth, UniOpVVVV::kNMSubF64, vec_op_fnmsub_fma<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing min / max (float)");
  {
#if defined(ASMJIT_UJIT_X86)
    test_vecop_vvv<kVecWidth, UniOpVVV::kMinF32S, scalar_op_fmin_ternary<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMinF64S, scalar_op_fmin_ternary<ScalarOpBehavior::kPreservingVec128, double>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF32S, scalar_op_fmax_ternary<ScalarOpBehavior::kPreservingVec128, float>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF64S, scalar_op_fmax_ternary<ScalarOpBehavior::kPreservingVec128, double>>(ctx);

    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinF32, vec_op_fmin_ternary<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinF64, vec_op_fmin_ternary<double>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF32, vec_op_fmax_ternary<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF64, vec_op_fmax_ternary<double>>(ctx, Variation{v});
    }
#endif

#if defined(ASMJIT_UJIT_AARCH64)
    test_vecop_vvv<kVecWidth, UniOpVVV::kMinF32S, scalar_op_fmin_finite<ScalarOpBehavior::kZeroing, float>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMinF64S, scalar_op_fmin_finite<ScalarOpBehavior::kZeroing, double>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF32S, scalar_op_fmax_finite<ScalarOpBehavior::kZeroing, float>>(ctx);
    test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF64S, scalar_op_fmax_finite<ScalarOpBehavior::kZeroing, double>>(ctx);

    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinF32, vec_op_fmin_finite<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMinF64, vec_op_fmin_finite<double>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF32, vec_op_fmax_finite<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kMaxF64, vec_op_fmax_finite<double>>(ctx, Variation{v});
    }
#endif
  }

  TEST_LOG("  Testing cmp (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqF32, vec_op_fcmpo_eq<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpEqF64, vec_op_fcmpo_eq<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeF32, vec_op_fcmpu_ne<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpNeF64, vec_op_fcmpu_ne<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtF32, vec_op_fcmpo_gt<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGtF64, vec_op_fcmpo_gt<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeF32, vec_op_fcmpo_ge<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpGeF64, vec_op_fcmpo_ge<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtF32, vec_op_fcmpo_lt<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLtF64, vec_op_fcmpo_lt<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeF32, vec_op_fcmpo_le<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpLeF64, vec_op_fcmpo_le<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpOrdF32, vec_op_fcmp_ord<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpOrdF64, vec_op_fcmp_ord<double>>(ctx, Variation{v});

      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpUnordF32, vec_op_fcmp_unord<float>>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCmpUnordF64, vec_op_fcmp_unord<double>>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing hadd (float)");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kHAddF64, vec_op_hadd_f64>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing combine");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kCombineLoHiU64, vec_op_combine_lo_hi_u64>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCombineLoHiF64, vec_op_combine_lo_hi_u64>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCombineHiLoU64, vec_op_combine_hi_lo_u64>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kCombineHiLoF64, vec_op_combine_hi_lo_u64>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing interleave");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveLoU8, vec_op_interleave_lo_u8>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveHiU8, vec_op_interleave_hi_u8>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveLoU16, vec_op_interleave_lo_u16>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveHiU16, vec_op_interleave_hi_u16>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveLoU32, vec_op_interleave_lo_u32>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveHiU32, vec_op_interleave_hi_u32>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveLoU64, vec_op_interleave_lo_u64>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kInterleaveHiU64, vec_op_interleave_hi_u64>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing packs");
  {
    for (uint32_t v = 0; v < kNumVariationsVVV; v++) {
      test_vecop_vvv<kVecWidth, UniOpVVV::kPacksI16_I8, vec_op_packs_i16_i8>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kPacksI16_U8, vec_op_packs_i16_u8>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kPacksI32_I16, vec_op_packs_i32_i16>(ctx, Variation{v});
      test_vecop_vvv<kVecWidth, UniOpVVV::kPacksI32_U16, vec_op_packs_i32_u16>(ctx, Variation{v});
    }
  }

  TEST_LOG("  Testing alignr_u128");
  {
    for (uint32_t v = 0; v < kNumVariationsVVVI; v++) {
      for (uint32_t i = 1; i < 16; i++) {
        test_vecop_vvvi<kVecWidth, UniOpVVVI::kAlignr_U128, vec_op_alignr_u128>(ctx, i, Variation{v});
      }
    }
  }

  TEST_LOG("  Testing interleave_shuffle");
  {
    for (uint32_t v = 0; v < kNumVariationsVVVI; v++) {
      for (uint32_t i = 0; i < 256; i++) {
        uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

        test_vecop_vvvi<kVecWidth, UniOpVVVI::kInterleaveShuffleU32x4, vec_op_interleave_shuffle_u32x4>(ctx, imm, Variation{v});
        test_vecop_vvvi<kVecWidth, UniOpVVVI::kInterleaveShuffleF32x4, vec_op_interleave_shuffle_u32x4>(ctx, imm, Variation{v});
      }

      for (uint32_t i = 0; i < 4; i++) {
        uint32_t imm = swizzle((i >> 1) & 1, i & 1).value;

        test_vecop_vvvi<kVecWidth, UniOpVVVI::kInterleaveShuffleU64x2, vec_op_interleave_shuffle_u64x2>(ctx, imm, Variation{v});
        test_vecop_vvvi<kVecWidth, UniOpVVVI::kInterleaveShuffleF64x2, vec_op_interleave_shuffle_u64x2>(ctx, imm, Variation{v});
      }
    }
  }
}

static void test_gp_ops(JitContext& ctx) {
  test_cond_ops(ctx);
  test_m_ops(ctx);
  test_rm_ops(ctx);
  test_mr_ops(ctx);
  test_rr_ops(ctx);
  test_rrr_ops(ctx);
}

#if defined(ASMJIT_UJIT_X86)
// To verify that JIT implements ALL features with ALL possible CPU flags, we use profiles to select features
// that the JIT compiler will be allowed to use. The features are gradually increased similarly to how new CPU
// generations introduced them. We cannot cover ALL possible CPUs, but that's not even necessary as we test
// individual operations where instructions can be selected on the features available.
struct TargetProfile {
  char name[48];
  CpuFeatures features;
};

template<size_t Index, size_t From, typename... Features>
static void inherit_profile_from(TargetProfile* profiles, Features&&... features) noexcept {
  profiles[Index].features = profiles[Index - 1u].features;
  profiles[Index].features.add(axl::forward<Features>(features)...);
}

template<size_t Index, typename... Features>
static void inherit_profile_prev(TargetProfile* profiles, Features&&... features) noexcept {
  static_assert(Index > 0u);
  inherit_profile_from<Index, Index - 1u>(profiles, axl::forward<Features>(features)...);
}

// Features that must always be available;
static CpuFeatures baseline_features() noexcept {
  using Ext = CpuFeatures::X86;

  CpuFeatures base;
  base.add(Ext::kI486, Ext::kCMOV, Ext::kCMPXCHG8B, Ext::kFPU, Ext::kSSE, Ext::kSSE2);
  return base;
}

TEST_CASE(uni_compiler_x86_gp) {
  init_fpu_ops();

  using Ext = CpuFeatures::X86;

  UniCompilerTests::JitContext ctx;
  asmjit::CpuFeatures host_features = asmjit::CpuInfo::host().features();

  TargetProfile profiles[7] {
    {"baseline", baseline_features()},
    {"popcnt", {}},
    {"lzcnt", {}},
    {"bmi", {}},
    {"bmi2", {}},
    {"adx", {}},
    {"all_host", host_features},
  };

  inherit_profile_prev<1>(profiles, Ext::kPOPCNT);
  inherit_profile_prev<2>(profiles, Ext::kLZCNT);
  inherit_profile_prev<3>(profiles, Ext::kBMI);
  inherit_profile_prev<4>(profiles, Ext::kBMI2, Ext::kMOVBE);
  inherit_profile_prev<5>(profiles, Ext::kADX);

  bool first = true;
  CpuFeatures last_filtered;

  for (const TargetProfile& profile : profiles) {
    CpuFeatures filtered = profile.features;
    filtered.combine<axl::And>(host_features);

    if (!first && filtered == last_filtered) {
      continue;
    }

    if (test_runner.test_variation(profile.name)) {
      ctx.features = filtered;
      test_gp_ops(ctx);

      first = false;
      last_filtered = filtered;
    }
  }
}

TEST_CASE(uni_compiler_x86_simd) {
  init_fpu_ops();

  using Ext = CpuFeatures::X86;

  UniCompilerTests::JitContext ctx;
  asmjit::CpuFeatures host_features = asmjit::CpuInfo::host().features();

  TargetProfile profiles[21] {
    {"sse2", baseline_features()},

    // 1-5
    {"sse3", {}},
    {"ssse3", {}},
    {"sse4.1", {}},
    {"sse4.2", {}},
    {"pclmul", {}},

    // 6-13
    {"avx", {}},
    {"avx2", {}},
    {"f16c", {}},
    {"fma", {}},
    {"avx2_vaes", {}},
    {"avx2_vpclmul", {}},
    {"avx2_ifma+necvt", {}},
    {"avx2_vnni", {}},

    // 14-18
    {"avx512", {}},
    {"avx512_ifma", {}},
    {"avx512_vnni", {}},
    {"avx512_v2", {}},
    {"avx512_bf16", {}},
    {"avx512_fp16", {}},

    {"all_host", {host_features}},
  };

  inherit_profile_prev<1>(profiles, Ext::kSSE3);
  inherit_profile_prev<2>(profiles, Ext::kSSSE3);
  inherit_profile_prev<3>(profiles, Ext::kSSE4_1);
  inherit_profile_prev<4>(profiles, Ext::kSSE4_2, Ext::kPOPCNT);
  inherit_profile_prev<5>(profiles, Ext::kPCLMULQDQ);

  inherit_profile_prev<6>(profiles, Ext::kAVX);
  inherit_profile_prev<7>(profiles, Ext::kAVX2, Ext::kBMI, Ext::kBMI2, Ext::kLZCNT, Ext::kMOVBE);
  inherit_profile_prev<8>(profiles, Ext::kF16C);
  inherit_profile_prev<9>(profiles, Ext::kFMA);
  inherit_profile_from<10, 9>(profiles, Ext::kVAES);
  inherit_profile_from<11, 9>(profiles, Ext::kVPCLMULQDQ);
  inherit_profile_from<12, 9>(profiles, Ext::kAVX_IFMA, Ext::kAVX_NE_CONVERT);
  inherit_profile_from<13, 9>(profiles, Ext::kAVX_VNNI, Ext::kAVX_VNNI_INT8, Ext::kAVX_VNNI_INT16);

  inherit_profile_from<14, 10>(profiles, Ext::kAVX512_F, Ext::kAVX512_BW, Ext::kAVX512_DQ, Ext::kAVX512_CD, Ext::kAVX512_VL);
  inherit_profile_from<15, 13>(profiles, Ext::kAVX512_IFMA);
  inherit_profile_from<16, 13>(profiles, Ext::kAVX512_VNNI);
  inherit_profile_from<17, 13>(profiles, Ext::kAVX512_IFMA, Ext::kAVX512_VNNI,
                                         Ext::kAVX512_BITALG, Ext::kAVX512_VPOPCNTDQ,
                                         Ext::kAVX512_VBMI, Ext::kAVX512_VBMI2,
                                         Ext::kVPCLMULQDQ, Ext::kGFNI, Ext::kVAES);
  inherit_profile_from<18, 17>(profiles, Ext::kAVX512_BF16);
  inherit_profile_from<19, 17>(profiles, Ext::kAVX512_FP16);

  bool first = true;
  CpuFeatures last_filtered;

  for (const TargetProfile& profile : profiles) {
    CpuFeatures filtered = profile.features;
    filtered.combine<axl::And>(host_features);

    if (!first && filtered == last_filtered) {
      continue;
    }

    if (test_runner.test_variation(profile.name)) {
      ctx.features = filtered;

      TEST_LOG("Testing JIT compiler 128-bit SIMD ops [profile=%s]", profile.name);
      test_simd_ops<VecWidth::k128>(ctx);

      if (filtered.x86().has_avx2()) {
        TEST_LOG("Testing JIT compiler 256-bit SIMD ops [profile=%s]", profile.name);
        test_simd_ops<VecWidth::k256>(ctx);
      }

      if (filtered.x86().has_avx512_f()) {
        TEST_LOG("Testing JIT compiler 512-bit SIMD ops [profile=%s]", profile.name);
        test_simd_ops<VecWidth::k512>(ctx);
      }

      first = false;
      last_filtered = filtered;
    }
  }
}
#endif // ASMJIT_UJIT_X86

#if defined(ASMJIT_UJIT_AARCH64)
TEST_CASE(uni_compiler_a64_gp) {
  init_fpu_ops();

  UniCompilerTests::JitContext ctx;
  ctx.features = asmjit::CpuInfo::host().features();
  test_gp_ops(ctx);
}

TEST_CASE(uni_compiler_a64_simd) {
  init_fpu_ops();

  UniCompilerTests::JitContext ctx;
  ctx.features = asmjit::CpuInfo::host().features();
  test_simd_ops<VecWidth::k128>(ctx);
}
#endif // ASMJIT_UJIT_AARCH64

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_UJIT && !ASMJIT_NO_JIT
