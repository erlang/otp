// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ASSEMBLER_TEST_P_H_INCLUDED
#define ASMJIT_CORE_ASSEMBLER_TEST_P_H_INCLUDED

#include <asmjit/core/api_build_test_p.h>
#include <asmjit/core/assembler.h>

//! \cond NEVER

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

struct TestSettings {
  bool verbose;
  bool validate;
};

template<typename AssemblerClass>
class AssemblerTester {
public:
  asmjit::Environment env {};
  uint64_t base_address {};

  asmjit::CodeHolder code;
  AssemblerClass assembler;

  asmjit::Label L0 {};
  bool verbose {};
  bool validate {};

  AssemblerTester(ZeroTest::TestRunner& test_runner, asmjit::Arch arch, uint64_t base_address = 0u) noexcept
    : env(arch),
      base_address(base_address) {
    verbose = test_runner._verbose;
    validate = test_runner.cmd_line().has_arg("--validate");
    prepare();
  }

  ASMJIT_NOINLINE void prepare() noexcept {
    code.reset();
    code.init(env, base_address);
    code.attach(&assembler);
    L0 = assembler.new_label();

    if (validate) {
      assembler.add_diagnostic_options(asmjit::DiagnosticOptions::kValidateAssembler);
    }
  }

  ASMJIT_NOINLINE void expect_valid_instruction(const char* s, const char* expected_opcode, asmjit::Error err = asmjit::Error::kOk) noexcept {
    EXPECT_EQ(err, Error::kOk)
      .message("Failed to assemble '%s': %s", s, asmjit::stringify_error(err));

    asmjit::String encoded_opcode;
    asmjit::Section* text = code.text_section();

    encoded_opcode.append_hex(text->data(), text->buffer_size());
    EXPECT_EQ(encoded_opcode, Span<const char>(expected_opcode, strlen(expected_opcode)))
      .message("Failed to assemble '%s': Result{%s} != Expected{%s}", s, encoded_opcode.data(), expected_opcode);

    prepare();
  }

  ASMJIT_NOINLINE void expect_invalid_instruction(const char* s, asmjit::Error expected_error, asmjit::Error err) noexcept {
    EXPECT_NE(err, Error::kOk)
      .message("Invalid instruction '%s' passed, but should have failed with %s",
        s, asmjit::stringify_error(expected_error));

    EXPECT_EQ(err, expected_error)
      .message("Invalid instruction '%s' failed with %s, but expected error is %s",
        s, asmjit::stringify_error(err), asmjit::stringify_error(expected_error));

    prepare();
  }
};

ASMJIT_END_SUB_NAMESPACE

#define EXPECT_VALID_INSTRUCTION(OPCODE, ...) \
  tester.expect_valid_instruction(#__VA_ARGS__, OPCODE, tester.assembler.__VA_ARGS__)

#define EXPECT_INVALID_INSTRUCTION(ExpectedError, ...) \
  tester.expect_invalid_instruction(#__VA_ARGS__, ExpectedError, tester.assembler.__VA_ARGS__)

//! \endcond

#endif // ASMJIT_CORE_ASSEMBLER_TEST_P_H_INCLUDED
