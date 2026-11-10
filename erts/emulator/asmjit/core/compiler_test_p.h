// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_COMPILER_TEST_P_H_INCLUDED
#define ASMJIT_CORE_COMPILER_TEST_P_H_INCLUDED

#include <asmjit/core/api_build_test_p.h>

#include <asmjit/axl/commons.h>
#include <asmjit/core/assembler.h>
#include <asmjit/core/logger.h>
#include <asmjit/core/string.h>
#include <asmjit/core/jit_runtime.h>

//! \cond NEVER

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

template<typename T>
static void stringify_array(String& dst, const char* fmt, const T* array, size_t size) {
  dst.append('{');
  for (size_t i = 0; i < size; i++) {
    dst.append_format(fmt, array[i]);
  }
  dst.append('}');
}

class TestingErrorHandler : public ErrorHandler {
public:
  TestingErrorHandler() noexcept : _err(Error::kOk) {}

  void handle_error(Error err, const char* message, [[maybe_unused]] BaseEmitter* origin) override {
    TEST_LOG("FAILURE %s: %s", stringify_error(err), message);
    _err = err;
    _message.assign(message);
  }

  Error _err;
  String _message;
};

template<typename CompilerClass>
class CompilerTester {
public:
  TestingErrorHandler eh;
#ifndef ASMJIT_NO_LOGGING
  StringLogger logger;
#endif

  Environment environment {};
  CpuFeatures cpu_features {};
  CodeHolder code;

  bool verbose {};
  bool can_execute {};

  ASMJIT_NOINLINE CompilerTester(asmjit::Arch arch, ZeroTest::TestRunner& test_runner) noexcept {
    verbose = test_runner._verbose;
    prepare_environment(arch);
  }

  ASMJIT_NOINLINE void prepare_environment(Arch arch) noexcept {
#ifndef ASMJIT_NO_JIT
    environment = Environment::host();
    if (environment.arch() == arch) {
      cpu_features = CpuInfo::host().features();
      can_execute = true;
      return;
    }
#endif

    switch (arch) {
      case Arch::kX86:
      case Arch::kX64: {
        cpu_features.add(CpuFeatures::X86::kADX,
                         CpuFeatures::X86::kAVX,
                         CpuFeatures::X86::kAVX2,
                         CpuFeatures::X86::kBMI,
                         CpuFeatures::X86::kBMI2,
                         CpuFeatures::X86::kCMOV,
                         CpuFeatures::X86::kF16C,
                         CpuFeatures::X86::kFMA,
                         CpuFeatures::X86::kFPU,
                         CpuFeatures::X86::kI486,
                         CpuFeatures::X86::kLZCNT,
                         CpuFeatures::X86::kMMX,
                         CpuFeatures::X86::kMMX2,
                         CpuFeatures::X86::kPOPCNT,
                         CpuFeatures::X86::kSSE,
                         CpuFeatures::X86::kSSE2,
                         CpuFeatures::X86::kSSE3,
                         CpuFeatures::X86::kSSSE3,
                         CpuFeatures::X86::kSSE4_1,
                         CpuFeatures::X86::kSSE4_2);
        break;
      }

      case Arch::kAArch64: {
        cpu_features.add(CpuFeatures::ARM::kAES,
                         CpuFeatures::ARM::kASIMD,
                         CpuFeatures::ARM::kIDIVA,
                         CpuFeatures::ARM::kIDIVT,
                         CpuFeatures::ARM::kPMULL);
        break;
      }

      default: {
        break;
      }
    }

    environment.init(arch);
  }

  ASMJIT_NOINLINE void failed() {

  }

  ASMJIT_NOINLINE void compile(auto emit_fn) {
    EXPECT_OK(code.init(environment, cpu_features));
    code.set_error_handler(&eh);

    CompilerClass cc(&code);

#ifndef ASMJIT_NO_LOGGING
    logger.clear();
    code.set_logger(&logger);
    cc.add_diagnostic_options(DiagnosticOptions::kRAAnnotate | DiagnosticOptions::kRADebugAll);
#endif

    emit_fn(cc);

#ifndef ASMJIT_NO_LOGGING
    if (eh._err != Error::kOk) {
      TEST_LOG(logger.content().data());
    }
#endif

    EXPECT_EQ(eh._err, Error::kOk)
      .message("Emitting failed");

    Error err = cc.finalize();
    if (err != Error::kOk) {
    }

    EXPECT_EQ(err, Error::kOk)
      .message("Finalize failed");

#ifndef ASMJIT_NO_LOGGING
    code.reset_logger();
#endif
  }

  ASMJIT_NOINLINE void execute(auto execute_fn) {
#ifndef ASMJIT_NO_JIT
    if (can_execute) {
      JitRuntime rt;

      void* func_ptr;
      EXPECT_OK(rt._add(&func_ptr, &code));

      String result;
      String expect;
      if (!execute_fn(func_ptr, result, expect)) {
#ifndef ASMJIT_NO_LOGGING
        TEST_LOG(logger.content().data());
#endif
        EXPECT_EQ(result, expect);
      }
    }
#else
    axl::maybe_unused(execute_fn);
#endif
    code.reset();
  }
};

ASMJIT_END_SUB_NAMESPACE

//! \endcond

#endif // ASMJIT_CORE_COMPILER_TEST_P_H_INCLUDED
