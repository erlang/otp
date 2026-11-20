// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_X86) && !defined(ASMJIT_NO_COMPILER)

#include <asmjit/core/compiler_test_p.h>
#include <asmjit/x86/x86_compiler.h>

#include <asmjit-testing/tests/asmjit_test_misc.h>

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if ASMJIT_TARGET_ARCH_X86
  // Required for function tests that pass / return XMM registers.
  #include <emmintrin.h>
#endif

#ifdef _MSC_VER
// Interaction between '_setjmp' and C++ object destruction is non-portable.
#pragma warning(disable: 4611)
#endif

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

TEST_CASE(x86_compiler_func_no_code) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, [[maybe_unused]] String& result, [[maybe_unused]] String& expect) {
    using Func = void (*)(void);
    ptr_as_func<Func>(func_ptr)();
    return true;
  });
}

TEST_CASE(x86_compiler_func_no_align) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());
    cc.align(AlignMode::kCode, 0);
    cc.align(AlignMode::kCode, 1);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, [[maybe_unused]] String& result, [[maybe_unused]] String& expect) {
    using Func = void (*)(void);
    ptr_as_func<Func>(func_ptr)();
    return true;
  });
}

TEST_CASE(x86_compiler_const_pool) {
  for (ConstPoolScope scope : {ConstPoolScope::kLocal, ConstPoolScope::kGlobal}) {
    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    float f_val = 1.234f;
    double d_val = 1.234;

    tester.compile([&](x86::Compiler& cc) {
      FuncNode* fn = cc.add_func(FuncSignature::build<void, void*>());

      x86::Mem m_u8 = cc.new_byte_const(scope, 0x81u);
      x86::Mem m_i16 = cc.new_int16_const(scope, static_cast<int16_t>(0xF123u));
      x86::Mem m_u16 = cc.new_uint16_const(scope, 0x8123u);
      x86::Mem m_i32 = cc.new_int32_const(scope, static_cast<int32_t>(0xF1234567u));
      x86::Mem m_u32 = cc.new_uint32_const(scope, 0x81234567u);
      x86::Mem m_f32 = cc.new_float_const(scope, f_val);
      x86::Mem m_i64 = cc.new_int64_const(scope, static_cast<int64_t>(0xF123456789ABCDEF));
      x86::Mem m_u64 = cc.new_uint64_const(scope, 0x8123456789ABCDEFu);
      x86::Mem m_f64 = cc.new_double_const(scope, d_val);

      x86::Gp out_ptr = cc.new_gp_ptr("out_ptr");
      fn->set_arg(0, out_ptr);

      x86::Gp tmp_gp = cc.new_gp32("tmp_gp");
      x86::Vec tmp_vec = cc.new_vec128("tmp_vec");

      cc.movzx(tmp_gp, m_u8.clone_resized(1));
      cc.mov(dword_ptr(out_ptr), tmp_gp.r32());

      cc.movsx(tmp_gp, m_i16.clone_resized(2));
      cc.mov(dword_ptr(out_ptr, 4), tmp_gp.r32());

      cc.movzx(tmp_gp, m_u16.clone_resized(2));
      cc.mov(dword_ptr(out_ptr, 8), tmp_gp.r32());

      cc.mov(tmp_gp, m_i32.clone_resized(4));
      cc.mov(dword_ptr(out_ptr, 12), tmp_gp.r32());

      cc.mov(tmp_gp, m_u32.clone_resized(4));
      cc.mov(dword_ptr(out_ptr, 16), tmp_gp.r32());

      cc.mov(tmp_gp, m_f32.clone_resized(4));
      cc.mov(dword_ptr(out_ptr, 20), tmp_gp.r32());

      cc.movq(tmp_vec, m_i64);
      cc.movq(dword_ptr(out_ptr, 24), tmp_vec);

      cc.movq(tmp_vec, m_u64);
      cc.movq(dword_ptr(out_ptr, 32), tmp_vec);

      cc.movq(tmp_vec, m_f64);
      cc.movq(dword_ptr(out_ptr, 40), tmp_vec);

      cc.end_func();
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = void (*)(void*);

      constexpr size_t kElementCount = 12;
      uint32_t out[kElementCount] {};
      uint32_t exp[kElementCount] = {
        0x00000081u,
        0xFFFFF123u,
        0x00008123u,
        0xF1234567u,
        0x81234567u,
        std::bit_cast<uint32_t>(f_val),
        0x89ABCDEFu,
        0xF1234567u,
        0x89ABCDEFu,
        0x81234567u,
        static_cast<uint32_t>(std::bit_cast<uint64_t>(d_val) & 0xFFFFFFFFu),
        static_cast<uint32_t>(std::bit_cast<uint64_t>(d_val) >> 32u),
      };

      ptr_as_func<Func>(func_ptr)(out);
      stringify_array(result, "0x%08X", out, kElementCount);
      stringify_array(expect, "0x%08X", exp, kElementCount);
      return result == expect;
    });
  }
}

TEST_CASE(x86_compiler_func_alignment) {
  for (uint32_t arg_count = 0; arg_count <= 16; arg_count++) {
    for (uint32_t alignment = 16; alignment <= 32; alignment += 16) {
      for (bool preserve_fp : {true, false}) {
        CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

        tester.compile([&](x86::Compiler& cc) {
          FuncSignature signature(CallConvId::kCDecl);
          signature.set_ret_t<int>();
          for (uint32_t i = 0; i < arg_count; i++) {
            signature.add_arg_t<int>();
          }

          FuncNode* func_node = cc.add_func(signature);
          if (preserve_fp)
            func_node->frame().set_preserved_fp();

          x86::Gp gp_var = cc.new_gp_ptr("gp_var");
          x86::Gp gp_sum;
          x86::Mem stack = cc.new_stack(alignment, alignment);

          // Do a sum of arguments to verify a possible relocation when misaligned.
          if (arg_count) {
            for (uint32_t i = 0; i < arg_count; i++) {
              x86::Gp gp_arg = cc.new_gp32("gp_arg%u", i);
              func_node->set_arg(i, gp_arg);

              if (i == 0)
                gp_sum = gp_arg;
              else
                cc.add(gp_sum, gp_arg);
            }
          }

          // Check alignment of the stack (has to be 16).
          cc.lea(gp_var, stack);
          cc.and_(gp_var, alignment - 1);

          // Add a sum of all arguments to check if they are correct.
          if (arg_count)
            cc.or_(gp_var.r32(), gp_sum);

          cc.ret(gp_var);
          cc.end_func();
        });

        tester.execute([&](void* func_ptr, String& result, String& expect) {
          using U = uint32_t;

          using Func0 = U (*)();
          using Func1 = U (*)(U);
          using Func2 = U (*)(U, U);
          using Func3 = U (*)(U, U, U);
          using Func4 = U (*)(U, U, U, U);
          using Func5 = U (*)(U, U, U, U, U);
          using Func6 = U (*)(U, U, U, U, U, U);
          using Func7 = U (*)(U, U, U, U, U, U, U);
          using Func8 = U (*)(U, U, U, U, U, U, U, U);
          using Func9 = U (*)(U, U, U, U, U, U, U, U, U);
          using Func10 = U (*)(U, U, U, U, U, U, U, U, U, U);
          using Func11 = U (*)(U, U, U, U, U, U, U, U, U, U, U);
          using Func12 = U (*)(U, U, U, U, U, U, U, U, U, U, U, U);
          using Func13 = U (*)(U, U, U, U, U, U, U, U, U, U, U, U, U);
          using Func14 = U (*)(U, U, U, U, U, U, U, U, U, U, U, U, U, U);
          using Func15 = U (*)(U, U, U, U, U, U, U, U, U, U, U, U, U, U, U);
          using Func16 = U (*)(U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U);

          uint32_t result_ret = 0;
          uint32_t expect_ret = arg_count * (arg_count + 1u) / 2u;

          switch (arg_count) {
            case 0: result_ret = ptr_as_func<Func0>(func_ptr)(); break;
            case 1: result_ret = ptr_as_func<Func1>(func_ptr)(1); break;
            case 2: result_ret = ptr_as_func<Func2>(func_ptr)(1, 2); break;
            case 3: result_ret = ptr_as_func<Func3>(func_ptr)(1, 2, 3); break;
            case 4: result_ret = ptr_as_func<Func4>(func_ptr)(1, 2, 3, 4); break;
            case 5: result_ret = ptr_as_func<Func5>(func_ptr)(1, 2, 3, 4, 5); break;
            case 6: result_ret = ptr_as_func<Func6>(func_ptr)(1, 2, 3, 4, 5, 6); break;
            case 7: result_ret = ptr_as_func<Func7>(func_ptr)(1, 2, 3, 4, 5, 6, 7); break;
            case 8: result_ret = ptr_as_func<Func8>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8); break;
            case 9: result_ret = ptr_as_func<Func9>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9); break;
            case 10: result_ret = ptr_as_func<Func10>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10); break;
            case 11: result_ret = ptr_as_func<Func11>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11); break;
            case 12: result_ret = ptr_as_func<Func12>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12); break;
            case 13: result_ret = ptr_as_func<Func13>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13); break;
            case 14: result_ret = ptr_as_func<Func14>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14); break;
            case 15: result_ret = ptr_as_func<Func15>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15); break;
            case 16: result_ret = ptr_as_func<Func16>(func_ptr)(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16); break;
          }

          result.assign_format("{%u, %u}", result_ret >> 28, result_ret & 0x0FFFFFFFu);
          expect.assign_format("{%u, %u}", expect_ret >> 28, expect_ret & 0x0FFFFFFFu);

          return result == expect;
        });
      }
    }
  }
}

TEST_CASE(x86_compiler_indirect_branch_protection) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func = cc.add_func(FuncSignature::build<void>());
    func->add_attributes(FuncAttributes::kIndirectBranchProtection);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    axl::maybe_unused(result, expect);

    using Func = void (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    func();
    return true;
  });
}


TEST_CASE(x86_compiler_jump_merge) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    Label L0 = cc.new_label();
    Label L1 = cc.new_label();
    Label L2 = cc.new_label();
    Label LEnd = cc.new_label();

    x86::Gp dst = cc.new_gp_ptr("dst");
    x86::Gp val = cc.new_gp32("val");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int*, int>());
    func_node->set_arg(0, dst);
    func_node->set_arg(1, val);

    cc.cmp(val, 0);
    cc.je(L2);

    cc.cmp(val, 1);
    cc.je(L1);

    cc.cmp(val, 2);
    cc.je(L0);

    cc.mov(x86::dword_ptr(dst), val);
    cc.jmp(LEnd);

    // On purpose. This tests whether the CFG constructs a single basic-block
    // from multiple labels next to each other.
    cc.bind(L0);
    cc.bind(L1);
    cc.bind(L2);
    cc.mov(x86::dword_ptr(dst), 0);

    cc.bind(LEnd);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int*, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int arr[5] = { -1, -1, -1, -1, -1 };
    int exp[5] = {  0,  0,  0,  3,  4 };

    for (int i = 0; i < 5; i++)
      func(&arr[i], i);

    result.assign_format("{%d, %d, %d, %d, %d}", arr[0], arr[1], arr[2], arr[3], arr[4]);
    expect.assign_format("{%d, %d, %d, %d, %d}", exp[0], exp[1], exp[2], exp[3], exp[4]);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_jump_cross) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());

    Label L1 = cc.new_label();
    Label L2 = cc.new_label();
    Label L3 = cc.new_label();

    cc.jmp(L2);

    cc.bind(L1);
    cc.jmp(L3);

    cc.bind(L2);
    cc.jmp(L1);

    cc.bind(L3);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    axl::maybe_unused(result, expect);

    using Func = void (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    func();
    return true;
  });
}

TEST_CASE(x86_compiler_jump_many) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());
    for (uint32_t i = 0; i < 1000; i++) {
      Label L = cc.new_label();
      cc.jmp(L);
      cc.bind(L);
    }

    x86::Gp ret = cc.new_gp32("ret");
    cc.xor_(ret, ret);
    cc.ret(ret);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);

    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 0;

    result.assign_format("{%d}", result_ret);
    expect.assign_format("{%d}", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_jump_unreachable_1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());

    Label L_1 = cc.new_label();
    Label L_2 = cc.new_label();
    Label L_3 = cc.new_label();
    Label L_4 = cc.new_label();
    Label L_5 = cc.new_label();
    Label L_6 = cc.new_label();
    Label L_7 = cc.new_label();

    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");

    cc.bind(L_2);
    cc.bind(L_3);

    cc.jmp(L_1);

    cc.bind(L_5);
    cc.mov(v0, 0);

    cc.bind(L_6);
    cc.jmp(L_3);
    cc.mov(v1, 1);
    cc.jmp(L_1);

    cc.bind(L_4);
    cc.jmp(L_2);
    cc.bind(L_7);
    cc.add(v0, v1);

    cc.align(AlignMode::kCode, 16);
    cc.bind(L_1);
    cc.ret();
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    func();

    result.append("{}");
    expect.append("{}");

    return true;
  });
}

TEST_CASE(x86_compiler_jump_unreachable_2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());

    Label L_1 = cc.new_label();
    Label L_2 = cc.new_label();

    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");

    cc.jmp(L_1);
    cc.bind(L_2);
    cc.mov(v0, 1);
    cc.mov(v1, 2);
    cc.cmp(v0, v1);
    cc.jz(L_2);
    cc.jmp(L_1);

    cc.bind(L_1);
    cc.ret();
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    func();

    result.append("{}");
    expect.append("{}");

    return true;
  });
}

TEST_CASE(x86_compiler_jump_table_1) {
  enum Operator {
    kOperatorAdd = 0,
    kOperatorSub = 1,
    kOperatorMul = 2,
    kOperatorDiv = 3
  };

  for (bool annotated : {false, true}) {
    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    tester.compile([&](x86::Compiler& cc) {
      x86::Vec a = cc.new_xmm_ss("a");
      x86::Vec b = cc.new_xmm_ss("b");
      x86::Gp op = cc.new_gp32("op");
      x86::Gp target = cc.new_gp_ptr("target");
      x86::Gp offset = cc.new_gp_ptr("offset");

      Label L_Table = cc.new_label();
      Label L_Add = cc.new_label();
      Label L_Sub = cc.new_label();
      Label L_Mul = cc.new_label();
      Label L_Div = cc.new_label();
      Label L_End = cc.new_label();

      FuncNode* func_node = cc.add_func(FuncSignature::build<float, float, float, uint32_t>());
      func_node->set_arg(0, a);
      func_node->set_arg(1, b);
      func_node->set_arg(2, op);

      cc.lea(offset, x86::ptr(L_Table));
      if (cc.is_64bit())
        cc.movsxd(target, x86::dword_ptr(offset, op.clone_as(offset), 2));
      else
        cc.mov(target, x86::dword_ptr(offset, op.clone_as(offset), 2));
      cc.add(target, offset);

      // JumpAnnotation allows to annotate all possible jump targets of
      // instructions where it cannot be deduced from operands.
      if (annotated) {
        JumpAnnotation* annotation = cc.new_jump_annotation();
        annotation->add_label(L_Add);
        annotation->add_label(L_Sub);
        annotation->add_label(L_Mul);
        annotation->add_label(L_Div);
        cc.jmp(target, annotation);
      }
      else {
        cc.jmp(target);
      }

      cc.bind(L_Add);
      cc.addss(a, b);
      cc.jmp(L_End);

      cc.bind(L_Sub);
      cc.subss(a, b);
      cc.jmp(L_End);

      cc.bind(L_Mul);
      cc.mulss(a, b);
      cc.jmp(L_End);

      cc.bind(L_Div);
      cc.divss(a, b);

      cc.bind(L_End);
      cc.ret(a);

      cc.end_func();

      cc.bind(L_Table);
      cc.embed_label_delta(L_Add, L_Table, 4);
      cc.embed_label_delta(L_Sub, L_Table, 4);
      cc.embed_label_delta(L_Mul, L_Table, 4);
      cc.embed_label_delta(L_Div, L_Table, 4);
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = float (*)(float, float, uint32_t);
      Func func = ptr_as_func<Func>(func_ptr);

      float results[4];
      float expected[4];

      results[0] = func(33.0f, 14.0f, kOperatorAdd);
      results[1] = func(33.0f, 14.0f, kOperatorSub);
      results[2] = func(10.0f, 6.0f, kOperatorMul);
      results[3] = func(80.0f, 8.0f, kOperatorDiv);

      expected[0] = 47.0f;
      expected[1] = 19.0f;
      expected[2] = 60.0f;
      expected[3] = 10.0f;

      result.assign_format("{%f, %f, %f, %f}", double(results[0]), double(results[1]), double(results[2]), double(results[3]));
      expect.assign_format("{%f, %f, %f, %f}", double(expected[0]), double(expected[1]), double(expected[2]), double(expected[3]));

      return result == expect;
    });
  }
}

TEST_CASE(x86_compiler_jump_table_2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp result = cc.new_gp32("result");
    x86::Gp value = cc.new_gp32("value");
    x86::Gp target = cc.new_gp_ptr("target");
    x86::Gp offset = cc.new_gp_ptr("offset");

    Label L_Table = cc.new_label();
    Label L_Begin = cc.new_label();
    Label L_Case0 = cc.new_label();
    Label L_Case1 = cc.new_label();
    Label L_End = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int>());
    func_node->set_arg(0, value);

    cc.bind(L_Begin);
    cc.lea(offset, x86::ptr(L_Table));
    if (cc.is_64bit())
      cc.movsxd(target, x86::dword_ptr(offset, value.clone_as(offset), 2));
    else
      cc.mov(target, x86::dword_ptr(offset, value.clone_as(offset), 2));
    cc.add(target, offset);

    {
      JumpAnnotation* annotation = cc.new_jump_annotation();
      annotation->add_label(L_Case0);
      annotation->add_label(L_Case1);
      annotation->add_label(L_Begin); // Never used, just for the purpose of the test.
      cc.jmp(target, annotation);

      cc.bind(L_Case0);
      cc.mov(result, 0);
      cc.jmp(L_End);

      cc.bind(L_Case1);
      cc.mov(result, 1);
      cc.jmp(L_End);
    }

    cc.bind(L_End);
    cc.ret(result);

    cc.end_func();

    cc.bind(L_Table);
    cc.embed_label_delta(L_Case0, L_Table, 4);
    cc.embed_label_delta(L_Case1, L_Table, 4);
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int);
    Func func = ptr_as_func<Func>(func_ptr);

    int results[2];
    int expected[2];

    results[0] = func(0);
    results[1] = func(1);

    expected[0] = 0;
    expected[1] = 1;

    result.assign_format("{%d, %d}", results[0], results[1]);
    expect.assign_format("{%d, %d}", expected[0], expected[1]);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_jump_table_3) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    Label L_Target = cc.new_label();
    x86::Gp target = cc.new_gp_ptr("target");
    x86::Gp result = cc.new_gp32("result");

    JumpAnnotation* annotation = cc.new_jump_annotation();
    annotation->add_label(L_Target);

    cc.lea(target, x86::ptr(L_Target));
    cc.jmp(target, annotation);

    cc.bind(L_Target);
    cc.mov(result, 1234);
    cc.ret(result);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int out = func();
    int expected = 1234;

    result.assign_format("%d", out);
    expect.assign_format("%d", expected);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_jump_table_4) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp result = cc.new_gp32("result");
    x86::Gp condition = cc.new_gp32("condition");

    FuncNode* func = cc.add_func(FuncSignature::build<int, int>());
    func->set_arg(0, condition);

    Label L_NonZero = cc.new_label();
    cc.test(condition, condition);
    cc.jnz(L_NonZero);

    {
      JumpAnnotation* annotation = cc.new_jump_annotation();
      Label L_Target = cc.new_label();
      annotation->add_label(L_Target);

      x86::Gp target = cc.new_gp_ptr("target");
      cc.lea(target, x86::ptr(L_Target));
      cc.jmp(target, annotation);
      cc.bind(L_Target);
      cc.mov(result, 1234);
      cc.ret(result);
    }

    {
      JumpAnnotation* annotation = cc.new_jump_annotation();
      Label L_Target1 = cc.new_label();
      Label L_Target2 = cc.new_label();
      annotation->add_label(L_Target1);
      annotation->add_label(L_Target2);

      cc.bind(L_NonZero);
      x86::Gp target = cc.new_gp_ptr("target");
      cc.lea(target, x86::ptr(L_Target1));
      cc.jmp(target, annotation);

      cc.bind(L_Target1);
      cc.mov(result, 4321);
      cc.ret(result);

      // Never executed.
      cc.bind(L_Target2);
      cc.mov(result, 0);
      cc.ret(result);
    }

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int);
    Func func = ptr_as_func<Func>(func_ptr);

    int results[2] = { func(0), func(1) };
    int expected[2] = { 1234, 4321 };

    result.assign_format("{%d, %d}", results[0], results[1]);
    expect.assign_format("{%d, %d}", expected[0], expected[1]);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_basics) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");
    x86::Gp v3 = cc.new_gp32("v3");
    x86::Gp v4 = cc.new_gp32("v4");

    cc.xor_(v0, v0);

    cc.mov(v1, 1);
    cc.mov(v2, 2);
    cc.mov(v3, 3);
    cc.mov(v4, 4);

    cc.add(v0, v1);
    cc.add(v0, v2);
    cc.add(v0, v3);
    cc.add(v0, v4);

    cc.ret(v0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 1 + 2 + 3 + 4;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_many_1) {
  static constexpr uint32_t kCount = 8;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp a0 = cc.new_gp_ptr("a0");
    x86::Gp a1 = cc.new_gp_ptr("a1");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int*, int*>());
    func_node->set_arg(0, a0);
    func_node->set_arg(1, a1);

    // Create some variables.
    x86::Gp t = cc.new_gp32("t");
    x86::Gp x[kCount];

    uint32_t i;

    // Setup variables (use mov with reg/imm to se if register allocator works).
    for (i = 0; i < kCount; i++) x[i] = cc.new_gp32("x%u", i);
    for (i = 0; i < kCount; i++) cc.mov(x[i], int(i + 1));

    // Make sum (addition).
    cc.xor_(t, t);
    for (i = 0; i < kCount; i++) cc.add(t, x[i]);

    // Store result to a given pointer in first argument.
    cc.mov(x86::dword_ptr(a0), t);

    // Clear t.
    cc.xor_(t, t);

    // Make sum (subtraction).
    for (i = 0; i < kCount; i++) cc.sub(t, x[i]);

    // Store result to a given pointer in second argument.
    cc.mov(x86::dword_ptr(a1), t);

    // End of function.
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int*, int*);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_x = 0;
    int result_y = 0;

    int expect_x =  36;
    int expect_y = -36;

    func(&result_x, &result_y);

    result.assign_format("{x=%d, y=%d}", result_x, result_y);
    expect.assign_format("{x=%d, y=%d}", expect_x, expect_y);

    return result_x == expect_x && result_y == expect_y;
  });
}

TEST_CASE(x86_compiler_ra_many_2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp a = cc.new_gp_ptr("a");
    x86::Gp v[32];

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, uint32_t*>());
    func_node->set_arg(0, a);

    for (uint32_t i = 0; i < ASMJIT_ARRAY_SIZE(v); i++) v[i] = cc.new_gp32("v%d", i);
    for (uint32_t i = 0; i < ASMJIT_ARRAY_SIZE(v); i++) cc.xor_(v[i], v[i]);

    x86::Gp x = cc.new_gp32("x");
    Label L = cc.new_label();

    cc.mov(x, 32);
    cc.bind(L);
    for (uint32_t i = 0; i < ASMJIT_ARRAY_SIZE(v); i++) cc.add(v[i], i);

    cc.dec(x);
    cc.jnz(L);
    for (uint32_t i = 0; i < ASMJIT_ARRAY_SIZE(v); i++) cc.mov(x86::dword_ptr(a, int(i * 4)), v[i]);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(uint32_t*);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t i;
    uint32_t result_buf[32] {};
    uint32_t expect_buf[32] {};

    for (i = 0; i < ASMJIT_ARRAY_SIZE(result_buf); i++)
      expect_buf[i] = i * 32;
    func(result_buf);

    for (i = 0; i < ASMJIT_ARRAY_SIZE(result_buf); i++) {
      if (i != 0) {
        result.append(',');
        expect.append(',');
      }

      result.append_format("%u", result_buf[i]);
      expect.append_format("%u", expect_buf[i]);
    }

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_int8) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp x = cc.new_gp8("x");
    x86::Gp y = cc.new_gp32("y");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int8_t>());
    func_node->set_arg(0, x);

    cc.movsx(y, x);

    cc.ret(y);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int8_t);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(int8_t(-13));
    int expect_ret = -13;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_unhandled_arg) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp x = cc.new_gp32("x");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());
    func_node->set_arg(2, x);

    cc.ret(x);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(42, 155, 199);
    int expect_ret = 199;

    result.assign_format("{%d}", result_ret);
    expect.assign_format("{%d}", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_args_int_ptr) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, void*, void*, void*, void*, void*, void*, void*>());
    x86::Gp var[8];

    for (uint32_t i = 0; i < 8; i++) {
      var[i] = cc.new_gp_ptr("var%u", i);
      func_node->set_arg(i, var[i]);
    }

    for (uint32_t i = 0; i < 8; i++) {
      cc.add(var[i], int(i + 1));
    }

    // Move some data into buffer provided by arguments so we can verify if it
    // really works without looking into assembler output.
    for (uint32_t i = 0; i < 8; i++) {
      cc.add(x86::byte_ptr(var[i]), int(i + 1));
    }

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void*, void*, void*, void*, void*, void*, void*, void*);
    Func func = ptr_as_func<Func>(func_ptr);

    uint8_t result_buf[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    uint8_t expect_buf[9] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

    func(result_buf, result_buf, result_buf, result_buf,
         result_buf, result_buf, result_buf, result_buf);

    result.assign_format("buf={%d, %d, %d, %d, %d, %d, %d, %d, %d}",
      result_buf[0], result_buf[1], result_buf[2], result_buf[3],
      result_buf[4], result_buf[5], result_buf[6], result_buf[7],
      result_buf[8]);

    expect.assign_format("buf={%d, %d, %d, %d, %d, %d, %d, %d, %d}",
      expect_buf[0], expect_buf[1], expect_buf[2], expect_buf[3],
      expect_buf[4], expect_buf[5], expect_buf[6], expect_buf[7],
      expect_buf[8]);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_args_float) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, float, float, float, float, float, float, float, void*>());

    x86::Gp p = cc.new_gp_ptr("p");
    x86::Vec xv[7];

    for (uint32_t i = 0; i < 7; i++) {
      xv[i] = cc.new_xmm_ss("xv%u", i);
      func_node->set_arg(i, xv[i]);
    }

    func_node->set_arg(7, p);

    cc.addss(xv[0], xv[1]);
    cc.addss(xv[0], xv[2]);
    cc.addss(xv[0], xv[3]);
    cc.addss(xv[0], xv[4]);
    cc.addss(xv[0], xv[5]);
    cc.addss(xv[0], xv[6]);

    cc.movss(x86::ptr(p), xv[0]);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(float, float, float, float, float, float, float, float*);
    Func func = ptr_as_func<Func>(func_ptr);

    float result_ret = 0;
    float expect_ret = 1.0f + 2.0f + 3.0f + 4.0f + 5.0f + 6.0f + 7.0f;

    func(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, &result_ret);

    result.assign_format("{%g}", double(result_ret));
    expect.assign_format("{%g}", double(expect_ret));

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_args_double) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, double, double, double, double, double, double, double, void*>());

    x86::Gp p = cc.new_gp_ptr("p");
    x86::Vec xv[7];

    for (uint32_t i = 0; i < 7; i++) {
      xv[i] = cc.new_xmm_sd("xv%u", i);
      func_node->set_arg(i, xv[i]);
    }

    func_node->set_arg(7, p);

    cc.addsd(xv[0], xv[1]);
    cc.addsd(xv[0], xv[2]);
    cc.addsd(xv[0], xv[3]);
    cc.addsd(xv[0], xv[4]);
    cc.addsd(xv[0], xv[5]);
    cc.addsd(xv[0], xv[6]);

    cc.movsd(x86::ptr(p), xv[0]);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(double, double, double, double, double, double, double, double*);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = 0;
    double expect_ret = 1.0 + 2.0 + 3.0 + 4.0 + 5.0 + 6.0 + 7.0;

    func(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, &result_ret);

    result.assign_format("{%g}", result_ret);
    expect.assign_format("{%g}", expect_ret);

    return result == expect;
  });
}

#if ASMJIT_TARGET_ARCH_X86
// Not supported on Windows.
#ifndef _WIN32
TEST_CASE(x86_compiler_ra_args_vec) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Vec a = cc.new_xmm("xmm_a");
    x86::Vec b = cc.new_xmm("xmm_b");

    FuncNode* func_node = cc.add_func(FuncSignature::build<Type::Vec128, Type::Vec128, Type::Vec128>());
    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    cc.paddb(a, b);
    cc.ret(a);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = __m128i (*)(__m128i, __m128i);
    Func func = ptr_as_func<Func>(func_ptr);

    uint8_t a_data[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    uint8_t b_data[16] = { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 };

    uint8_t r_data[16] {};
    uint8_t e_data[16] = { 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15 };

    __m128i a_vec = _mm_loadu_si128(reinterpret_cast<const __m128i*>(a_data));
    __m128i b_vec = _mm_loadu_si128(reinterpret_cast<const __m128i*>(b_data));

    __m128i r_vec = func(a_vec, b_vec);
    _mm_storeu_si128(reinterpret_cast<__m128i*>(r_data), r_vec);

    result.append_hex(r_data, 16);
    expect.append_hex(e_data, 16);

    return result == expect;
  });
}
#endif // !_WIN32
#endif // ASMJIT_TARGET_ARCH_X86

TEST_CASE(x86_compiler_ra_ret_float1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Vec x = cc.new_xmm_ss("x");

    FuncNode* func_node = cc.add_func(FuncSignature::build<float, float>());
    func_node->set_arg(0, x);

    cc.ret(x);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = float (*)(float);
    Func func = ptr_as_func<Func>(func_ptr);

    float result_ret = func(42.0f);
    float expect_ret = 42.0f;

    result.assign_format("{%g}", double(result_ret));
    expect.assign_format("{%g}", double(expect_ret));

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_ret_float2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Vec x = cc.new_xmm_ss("x");
    x86::Vec y = cc.new_xmm_ss("y");

    FuncNode* func_node = cc.add_func(FuncSignature::build<float, float, float>());
    func_node->set_arg(0, x);
    func_node->set_arg(1, y);

    cc.addss(x, y);
    cc.ret(x);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = float (*)(float, float);
    Func func = ptr_as_func<Func>(func_ptr);

    float result_ret = func(1.0f, 2.0f);
    float expect_ret = 1.0f + 2.0f;

    result.assign_format("{%g}", double(result_ret));
    expect.assign_format("{%g}", double(expect_ret));

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_ret_double1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Vec x = cc.new_xmm_sd("x");

    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double>());
    func_node->set_arg(0, x);

    cc.ret(x);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = func(42.0);
    double expect_ret = 42.0;

    result.assign_format("{%g}", result_ret);
    expect.assign_format("{%g}", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_ret_double2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Vec x = cc.new_xmm_sd("x");
    x86::Vec y = cc.new_xmm_sd("y");

    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double, double>());
    func_node->set_arg(0, x);
    func_node->set_arg(1, y);

    cc.addsd(x, y);
    cc.ret(x);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double, double);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = func(1.0, 2.0);
    double expect_ret = 1.0 + 2.0;

    result.assign_format("{%g}", result_ret);
    expect.assign_format("{%g}", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_ra_stack) {
  constexpr uint32_t kSize = 256u;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    x86::Mem stack = cc.new_stack(kSize, 1);
    stack.set_size(1);

    x86::Gp i = cc.new_gp_ptr("i");
    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp32("b");

    Label L_1 = cc.new_label();
    Label L_2 = cc.new_label();

    // Fill stack by sequence [0, 1, 2, 3 ... 255].
    cc.xor_(i, i);

    x86::Mem stack_with_index = stack.clone();
    stack_with_index.set_index(i, 0);

    cc.bind(L_1);
    cc.mov(stack_with_index, i.r8());
    cc.inc(i);
    cc.cmp(i, 255);
    cc.jle(L_1);

    // Sum sequence in stack.
    cc.xor_(i, i);
    cc.xor_(a, a);

    cc.bind(L_2);
    cc.movzx(b, stack_with_index);
    cc.add(a, b);
    cc.inc(i);
    cc.cmp(i, 255);
    cc.jle(L_2);

    cc.ret(a);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 32640;

    result.assign_int(result_ret);
    expect.assign_int(expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_imul1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp dst_hi = cc.new_gp_ptr("dst_hi");
    x86::Gp dst_lo = cc.new_gp_ptr("dst_lo");

    x86::Gp v_hi = cc.new_gp32("v_hi");
    x86::Gp v_lo = cc.new_gp32("v_lo");
    x86::Gp src = cc.new_gp32("src");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int*, int*, int, int>());
    func_node->set_arg(0, dst_hi);
    func_node->set_arg(1, dst_lo);
    func_node->set_arg(2, v_lo);
    func_node->set_arg(3, src);

    cc.imul(v_hi, v_lo, src);
    cc.mov(x86::dword_ptr(dst_hi), v_hi);
    cc.mov(x86::dword_ptr(dst_lo), v_lo);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int*, int*, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int v0 = 4;
    int v1 = 4;

    int result_hi = 0;
    int result_lo = 0;

    int expect_hi = 0;
    int expect_lo = v0 * v1;

    func(&result_hi, &result_lo, v0, v1);

    result.assign_format("hi=%d, lo=%d", result_hi, result_lo);
    expect.assign_format("hi=%d, lo=%d", expect_hi, expect_lo);

    return result_hi == expect_hi && result_lo == expect_lo;
  });
}

TEST_CASE(x86_compiler_imul2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp dst = cc.new_gp_ptr("dst");
    x86::Gp src = cc.new_gp_ptr("src");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int*, const int*>());
    func_node->set_arg(0, dst);
    func_node->set_arg(1, src);

    for (unsigned int i = 0; i < 4; i++) {
      x86::Gp x  = cc.new_gp32("x");
      x86::Gp y  = cc.new_gp32("y");
      x86::Gp hi = cc.new_gp32("hi");

      cc.mov(x, x86::dword_ptr(src, 0));
      cc.mov(y, x86::dword_ptr(src, 4));

      cc.imul(hi, x, y);
      cc.add(x86::dword_ptr(dst, 0), hi);
      cc.add(x86::dword_ptr(dst, 4), x);
    }

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int*, const int*);
    Func func = ptr_as_func<Func>(func_ptr);

    int src[2] = { 4, 9 };
    int result_ret[2] = { 0, 0 };
    int expect_ret[2] = { 0, (4 * 9) * 4 };

    func(result_ret, src);

    result.assign_format("{%d, %d}", result_ret[0], result_ret[1]);
    expect.assign_format("{%d, %d}", expect_ret[0], expect_ret[1]);

    return result_ret[0] == expect_ret[0] && result_ret[1] == expect_ret[1];
  });
}

TEST_CASE(x86_compiler_idiv1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp32("b");
    x86::Gp dummy = cc.new_gp32("dummy");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());
    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    cc.xor_(dummy, dummy);
    cc.idiv(dummy, a, b);

    cc.ret(a);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int v0 = 2999;
    int v1 = 245;

    int result_ret = func(v0, v1);
    int expect_ret = 2999 / 245;

    result.assign_format("result=%d", result_ret);
    expect.assign_format("result=%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_setz) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp src0 = cc.new_gp32("src0");
    x86::Gp src1 = cc.new_gp32("src1");
    x86::Gp dst0 = cc.new_gp_ptr("dst0");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int, int, char*>());
    func_node->set_arg(0, src0);
    func_node->set_arg(1, src1);
    func_node->set_arg(2, dst0);

    cc.cmp(src0, src1);
    cc.setz(x86::byte_ptr(dst0));

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int, int, char*);
    Func func = ptr_as_func<Func>(func_ptr);

    char result_buf[4] {};
    char expect_buf[4] = { 1, 0, 0, 1 };

    func(0, 0, &result_buf[0]); // We are expecting 1 (0 == 0).
    func(0, 1, &result_buf[1]); // We are expecting 0 (0 != 1).
    func(1, 0, &result_buf[2]); // We are expecting 0 (1 != 0).
    func(1, 1, &result_buf[3]); // We are expecting 1 (1 == 1).

    result.assign_format("out={%d, %d, %d, %d}", result_buf[0], result_buf[1], result_buf[2], result_buf[3]);
    expect.assign_format("out={%d, %d, %d, %d}", expect_buf[0], expect_buf[1], expect_buf[2], expect_buf[3]);

    return result_buf[0] == expect_buf[0] &&
           result_buf[1] == expect_buf[1] &&
           result_buf[2] == expect_buf[2] &&
           result_buf[3] == expect_buf[3] ;
  });
}

TEST_CASE(x86_compiler_shl_ror) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp dst = cc.new_gp_ptr("dst");
    x86::Gp var = cc.new_gp32("var");
    x86::Gp v_shl_param = cc.new_gp32("v_shl_param");
    x86::Gp v_ror_param = cc.new_gp32("v_ror_param");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, int*, int, int, int>());
    func_node->set_arg(0, dst);
    func_node->set_arg(1, var);
    func_node->set_arg(2, v_shl_param);
    func_node->set_arg(3, v_ror_param);

    cc.shl(var, v_shl_param);
    cc.ror(var, v_ror_param);
    cc.mov(x86::dword_ptr(dst), var);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(int*, int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int v0 = 0x000000FF;

    int result_ret = 0;
    int expect_ret = 0x0000FF00;

    func(&result_ret, v0, 16, 8);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_gpb_lo1) {
  constexpr uint32_t kCount = 32u;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp reg_Ptr = cc.new_gp_ptr("reg_Ptr");
    x86::Gp sum = cc.new_gp32("sum");
    x86::Gp x[kCount];

    FuncNode* func_node = cc.add_func(FuncSignature::build<uint32_t, uint32_t*>());
    func_node->set_arg(0, reg_Ptr);

    for (uint32_t i = 0; i < kCount; i++) {
      x[i] = cc.new_gp32("x%u", i);
    }

    // Init pseudo-regs with values from our array.
    for (uint32_t i = 0; i < kCount; i++) {
      cc.mov(x[i], x86::dword_ptr(reg_Ptr, int(i * 4)));
    }

    for (uint32_t i = 2; i < kCount; i++) {
      // Add and truncate to 8 bit; no purpose, just mess with jit.
      cc.add  (x[i  ], x[i-1]);
      cc.movzx(x[i  ], x[i  ].r8());
      cc.movzx(x[i-2], x[i-1].r8());
      cc.movzx(x[i-1], x[i-2].r8());
    }

    // Sum up all computed values.
    cc.mov(sum, 0);
    for (uint32_t i = 0; i < kCount; i++) {
      cc.add(sum, x[i]);
    }

    // Return the sum.
    cc.ret(sum);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t*);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t i;
    uint32_t buf[kCount];
    uint32_t result_ret = 0;
    uint32_t expect_ret = 0;

    for (i = 0; i < kCount; i++) {
      buf[i] = 1;
    }

    for (i = 2; i < kCount; i++) {
      buf[i  ]+= buf[i-1];
      buf[i  ] = buf[i  ] & 0xFF;
      buf[i-2] = buf[i-1] & 0xFF;
      buf[i-1] = buf[i-2] & 0xFF;
    }

    for (i = 0; i < kCount; i++) {
      expect_ret += buf[i];
    }

    for (i = 0; i < kCount; i++) {
      buf[i] = 1;
    }
    result_ret = func(buf);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_gpb_lo2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v = cc.new_gp32("v");

    FuncNode* func_node = cc.add_func(FuncSignature::build<uint32_t, uint32_t>());
    func_node->set_arg(0, v);

    cc.mov(v.r8(), 0xFF);
    cc.ret(v);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t result_ret = func(0x12345678u);
    uint32_t expect_ret = 0x123456FFu;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_rep_movsb) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp dst = cc.new_gp_ptr("dst");
    x86::Gp src = cc.new_gp_ptr("src");
    x86::Gp cnt = cc.new_gp_ptr("cnt");

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, void*, size_t>());
    func_node->set_arg(0, dst);
    func_node->set_arg(1, src);
    func_node->set_arg(2, cnt);

    cc.rep(cnt).movs(x86::byte_ptr(dst), x86::byte_ptr(src));
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void*, void*, size_t);
    Func func = ptr_as_func<Func>(func_ptr);

    char dst[20] = { 0 };
    char src[20] = "Hello AsmJit!";
    func(dst, src, strlen(src) + 1);

    result.assign_format("\"%s\"", dst);
    expect.assign_format("\"%s\"", src);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_if_else1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");

    Label L_1 = cc.new_label();
    Label L_2 = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());
    func_node->set_arg(0, v1);
    func_node->set_arg(1, v2);

    cc.cmp(v1, v2);
    cc.jg(L_1);

    cc.mov(v1, 1);
    cc.jmp(L_2);

    cc.bind(L_1);
    cc.mov(v1, 2);

    cc.bind(L_2);
    cc.ret(v1);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int a = func(0, 1);
    int b = func(1, 0);

    result.append_format("{%d, %d}", a, b);
    expect.append_format("{%d, %d}", 1, 2);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_if_else2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");

    Label L_1 = cc.new_label();
    Label L_2 = cc.new_label();
    Label L_3 = cc.new_label();
    Label L_4 = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());
    func_node->set_arg(0, v1);
    func_node->set_arg(1, v2);

    cc.jmp(L_1);
    cc.bind(L_2);
    cc.jmp(L_4);
    cc.bind(L_1);

    cc.cmp(v1, v2);
    cc.jg(L_3);

    cc.mov(v1, 1);
    cc.jmp(L_2);

    cc.bind(L_3);
    cc.mov(v1, 2);
    cc.jmp(L_2);

    cc.bind(L_4);

    cc.ret(v1);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int a = func(0, 1);
    int b = func(1, 0);

    result.append_format("{%d, %d}", a, b);
    expect.append_format("{%d, %d}", 1, 2);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_if_else3) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");
    x86::Gp counter = cc.new_gp32("counter");

    Label L_1 = cc.new_label();
    Label L_Loop = cc.new_label();
    Label L_Exit = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());
    func_node->set_arg(0, v1);
    func_node->set_arg(1, v2);

    cc.cmp(v1, v2);
    cc.jg(L_1);

    cc.mov(counter, 0);

    cc.bind(L_Loop);
    cc.mov(v1, counter);

    cc.inc(counter);
    cc.cmp(counter, 1);
    cc.jle(L_Loop);
    cc.jmp(L_Exit);

    cc.bind(L_1);
    cc.mov(v1, 2);

    cc.bind(L_Exit);
    cc.ret(v1);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int a = func(0, 1);
    int b = func(1, 0);

    result.append_format("{%d, %d}", a, b);
    expect.append_format("{%d, %d}", 1, 2);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_if_else4) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");
    x86::Gp counter = cc.new_gp32("counter");

    Label L_1 = cc.new_label();
    Label L_Loop1 = cc.new_label();
    Label L_Loop2 = cc.new_label();
    Label L_Exit = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());
    func_node->set_arg(0, v1);
    func_node->set_arg(1, v2);

    cc.mov(counter, 0);
    cc.cmp(v1, v2);
    cc.jg(L_1);

    cc.bind(L_Loop1);
    cc.mov(v1, counter);

    cc.inc(counter);
    cc.cmp(counter, 1);
    cc.jle(L_Loop1);
    cc.jmp(L_Exit);

    cc.bind(L_1);
    cc.bind(L_Loop2);
    cc.mov(v1, counter);
    cc.inc(counter);
    cc.cmp(counter, 2);
    cc.jle(L_Loop2);

    cc.bind(L_Exit);
    cc.ret(v1);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int a = func(0, 1);
    int b = func(1, 0);

    result.append_format("{%d, %d}", a, b);
    expect.append_format("{%d, %d}", 1, 2);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_memcpy) {
  constexpr uint32_t kCount = 32u;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp dst = cc.new_gp_ptr("dst");
    x86::Gp src = cc.new_gp_ptr("src");
    x86::Gp cnt = cc.new_gp_ptr("cnt");

    Label L_Loop = cc.new_label();                   // Create base labels we use
    Label L_Exit = cc.new_label();                   // in our function.

    FuncNode* func_node = cc.add_func(FuncSignature::build<void, uint32_t*, const uint32_t*, size_t>());
    func_node->set_arg(0, dst);
    func_node->set_arg(1, src);
    func_node->set_arg(2, cnt);

    cc.test(cnt, cnt);                              // Exit if the size is zero.
    cc.jz(L_Exit);

    cc.bind(L_Loop);                                // Bind the loop label here.

    x86::Gp tmp = cc.new_gp32("tmp");               // Copy a single dword (4 bytes).
    cc.mov(tmp, x86::dword_ptr(src));
    cc.mov(x86::dword_ptr(dst), tmp);

    cc.add(src, 4);                                 // Increment dst/src pointers.
    cc.add(dst, 4);

    cc.dec(cnt);                                    // Loop until cnt isn't zero.
    cc.jnz(L_Loop);

    cc.bind(L_Exit);                                // Bind the exit label here.
    cc.end_func();                                   // End of function.
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(uint32_t*, const uint32_t*, size_t);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t i;

    uint32_t dst_buffer[kCount];
    uint32_t src_buffer[kCount];

    for (i = 0; i < kCount; i++) {
      dst_buffer[i] = 0;
      src_buffer[i] = i;
    }

    func(dst_buffer, src_buffer, kCount);

    result.assign("buf={");
    expect.assign("buf={");

    for (i = 0; i < kCount; i++) {
      if (i != 0) {
        result.append(", ");
        expect.append(", ");
      }

      result.append_format("%u", unsigned(dst_buffer[i]));
      expect.append_format("%u", unsigned(src_buffer[i]));
    }

    result.append("}");
    expect.append("}");

    return result == expect;
  });
}

TEST_CASE(x86_compiler_extra_block) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp cond = cc.new_gp32("cond");
    x86::Gp ret = cc.new_gp32("ret");
    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp32("b");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());
    func_node->set_arg(0, cond);
    func_node->set_arg(1, a);
    func_node->set_arg(2, b);

    Label L_Ret = cc.new_label();
    Label L_Extra = cc.new_label();

    cc.test(cond, cond);
    cc.jnz(L_Extra);

    cc.mov(ret, a);
    cc.add(ret, b);

    cc.bind(L_Ret);
    cc.ret(ret);

    // Emit code sequence at the end of the function.
    BaseNode* prev_cursor = cc.set_cursor(func_node->end_node()->prev());
    cc.bind(L_Extra);
    cc.mov(ret, a);
    cc.sub(ret, b);
    cc.jmp(L_Ret);
    cc.set_cursor(prev_cursor);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int ret1 = func(0, 4, 5);
    int ret2 = func(1, 4, 5);

    int exp1 = 4 + 5;
    int exp2 = 4 - 5;

    result.assign_format("{%d, %d}", ret1, ret2);
    expect.assign_format("{%d, %d}", exp1, exp2);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_alpha_blend) {
  constexpr uint32_t kCount = 17u;

  using BlendSrcOver = uint32_t (*)(uint32_t, uint32_t);
  BlendSrcOver blend_src_over = [](uint32_t d, uint32_t s) {
    uint32_t sa_inv = ~s >> 24;

    uint32_t d_20 = (d     ) & 0x00FF00FF;
    uint32_t d_31 = (d >> 8) & 0x00FF00FF;

    d_20 *= sa_inv;
    d_31 *= sa_inv;

    d_20 = ((d_20 + ((d_20 >> 8) & 0x00FF00FFu) + 0x00800080u) & 0xFF00FF00u) >> 8;
    d_31 = ((d_31 + ((d_31 >> 8) & 0x00FF00FFu) + 0x00800080u) & 0xFF00FF00u);

    return d_20 + d_31 + s;
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    asmtest::generate_sse_alpha_blend(cc, true);
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void*, const void*, size_t);
    Func func = ptr_as_func<Func>(func_ptr);

    static const uint32_t dst_const_data[] = { 0x00000000, 0x10101010, 0x20100804, 0x30200003, 0x40204040, 0x5000004D, 0x60302E2C, 0x706F6E6D, 0x807F4F2F, 0x90349001, 0xA0010203, 0xB03204AB, 0xC023AFBD, 0xD0D0D0C0, 0xE0AABBCC, 0xFFFFFFFF, 0xF8F4F2F1 };
    static const uint32_t src_const_data[] = { 0xE0E0E0E0, 0xA0008080, 0x341F1E1A, 0xFEFEFEFE, 0x80302010, 0x49490A0B, 0x998F7798, 0x00000000, 0x01010101, 0xA0264733, 0xBAB0B1B9, 0xFF000000, 0xDAB0A0C1, 0xE0BACFDA, 0x99887766, 0xFFFFFF80, 0xEE0A5FEC };

    uint32_t dst_buffer_storage[kCount + 3];
    uint32_t src_buffer_storage[kCount + 3];

    // Has to be aligned.
    uint32_t* dst_buffer = (uint32_t*)axl::align_up<intptr_t>((intptr_t)dst_buffer_storage, 16);
    uint32_t* src_buffer = (uint32_t*)axl::align_up<intptr_t>((intptr_t)src_buffer_storage, 16);

    memcpy(dst_buffer, dst_const_data, sizeof(dst_const_data));
    memcpy(src_buffer, src_const_data, sizeof(src_const_data));

    uint32_t i;
    uint32_t exp_buffer[kCount];

    for (i = 0; i < kCount; i++) {
      exp_buffer[i] = blend_src_over(dst_buffer[i], src_buffer[i]);
    }

    func(dst_buffer, src_buffer, kCount);

    result.assign("buf={");
    expect.assign("buf={");

    for (i = 0; i < kCount; i++) {
      if (i != 0) {
        result.append(", ");
        expect.append(", ");
      }

      result.append_format("%08X", unsigned(dst_buffer[i]));
      expect.append_format("%08X", unsigned(exp_buffer[i]));
    }

    result.append("}");
    expect.append("}");

    return result == expect;
  });
}

TEST_CASE(x86_compiler_avx512_kk) {
  const CpuInfo& cpu_info = CpuInfo::host();

  if (cpu_info.features().x86().has_avx512_f()) {
    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    tester.compile([&](x86::Compiler& cc) {
      FuncNode* func_node = cc.add_func(FuncSignature::build<uint32_t, const void*, const void*, uint32_t>());

      x86::Gp a = cc.new_gp_ptr("a");
      x86::Gp b = cc.new_gp_ptr("b");
      x86::Gp pred = cc.new_gp32("pred");
      x86::Gp result = cc.new_gp32("result");

      x86::Vec va = cc.new_zmm("va");
      x86::Vec vb = cc.new_zmm("vb");
      x86::KReg k_in = cc.new_kd("k_in");
      x86::KReg k_out = cc.new_kd("k_out");

      func_node->set_arg(0, a);
      func_node->set_arg(1, b);
      func_node->set_arg(2, pred);

      cc.vmovdqu32(va, x86::ptr(a));
      cc.vmovdqu32(vb, x86::ptr(b));
      cc.kmovd(k_in, pred);
      cc.k(k_in).vpcmpeqd(k_out, va, vb);
      cc.kmovd(result, k_out);
      cc.ret(result);
      cc.end_func();
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = uint32_t (*)(const void*, const void*, uint32_t pred_k);
      Func func = ptr_as_func<Func>(func_ptr);

      static const uint32_t src_a[16] = { 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1 };
      static const uint32_t src_b[16] = { 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 };

      uint32_t ret = func(src_a, src_b, 0xF0F0);

      result.assign_format("0x%08X", ret);
      expect.assign_format("0x%08X", 0xA040u);

      return result == expect;
    });
  }
}

TEST_CASE(x86_compiler_avx512_tern_log) {
  const CpuInfo& cpu_info = CpuInfo::host();

  if (cpu_info.features().x86().has_avx512_f()) {
    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    tester.compile([&](x86::Compiler& cc) {
      FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*>());

      x86::Gp out_ptr = cc.new_gp_ptr("out_ptr");
      x86::Vec vec = cc.new_zmm("vec");

      func_node->set_arg(0, out_ptr);

      cc.vpternlogd(vec, vec, vec, 0xFFu);
      cc.vmovdqu8(x86::ptr(out_ptr), vec);
      cc.end_func();
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = void (*)(void*);
      Func func = ptr_as_func<Func>(func_ptr);

      uint32_t out[16] {};
      uint32_t exp[16] = {
        0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
        0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
        0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
        0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
      };

      func(out);

      stringify_array(result, "0x%08X", out, 16);
      stringify_array(expect, "0x%08X", exp, 16);

      return result == expect;
    });
  }
}

TEST_CASE(x86_compiler_func_arg_int8) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");

    FuncNode* func_node = cc.add_func(FuncSignature::build<unsigned, uint8_t, uint8_t, uint32_t>());
    func_node->set_arg(0, v0);
    func_node->set_arg(1, v1);

    cc.add(v0, v1);

    cc.ret(v0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint8_t, uint8_t, uint32_t);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t arg = uint32_t(uintptr_t(func_ptr) & 0xFFFFFFFF);

    unsigned result_ret = func(uint8_t(arg & 0xFF), uint8_t(arg & 0xFF), arg);
    unsigned expect_ret = (arg & 0xFF) * 2;

    result.assign_format("%u", result_ret);
    expect.assign_format("%u", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_base1) {
  using CalledFn = int (*)(int, int, int);
  CalledFn called_fn = [](int a, int b, int c) { return (a + b) * c; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");
    x86::Gp v2 = cc.new_gp32("v2");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());
    func_node->set_arg(0, v0);
    func_node->set_arg(1, v1);
    func_node->set_arg(2, v2);

    // Just do something.
    cc.shl(v0, 1);
    cc.shl(v1, 1);
    cc.shl(v2, 1);

    // Call a function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), imm((void*)called_fn), FuncSignature::build<int, int, int, int>());
    invoke_node->set_arg(0, v2);
    invoke_node->set_arg(1, v1);
    invoke_node->set_arg(2, v0);
    invoke_node->set_ret(0, v0);

    cc.ret(v0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(3, 2, 1);
    int expect_ret = 36;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_base2) {
  [[maybe_unused]] static constexpr uint32_t kSize = 256u;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    const int kTokenSize = 32;

    x86::Mem s1 = cc.new_stack(kTokenSize, 32);
    x86::Mem s2 = cc.new_stack(kTokenSize, 32);

    x86::Gp p1 = cc.new_gp_ptr("p1");
    x86::Gp p2 = cc.new_gp_ptr("p2");

    x86::Gp ret = cc.new_gp32("ret");
    Label L_Exit = cc.new_label();

    static const char token[kTokenSize] = "-+:|abcdefghijklmnopqrstuvwxyz|";
    InvokeNode* invoke_node;

    cc.lea(p1, s1);
    cc.lea(p2, s2);

    // Try to corrupt the stack if wrongly allocated.
    cc.invoke(Out(invoke_node), imm((void*)memcpy), FuncSignature::build<void*, void*, void*, size_t>());
    invoke_node->set_arg(0, p1);
    invoke_node->set_arg(1, imm(token));
    invoke_node->set_arg(2, imm(kTokenSize));
    invoke_node->set_ret(0, p1);

    cc.invoke(Out(invoke_node), imm((void*)memcpy), FuncSignature::build<void*, void*, void*, size_t>());
    invoke_node->set_arg(0, p2);
    invoke_node->set_arg(1, imm(token));
    invoke_node->set_arg(2, imm(kTokenSize));
    invoke_node->set_ret(0, p2);

    cc.invoke(Out(invoke_node), imm((void*)memcmp), FuncSignature::build<int, void*, void*, size_t>());
    invoke_node->set_arg(0, p1);
    invoke_node->set_arg(1, p2);
    invoke_node->set_arg(2, imm(kTokenSize));
    invoke_node->set_ret(0, ret);

    // This should be 0 on success, however, if both `p1` and `p2` were
    // allocated in the same address this check will still pass.
    cc.cmp(ret, 0);
    cc.jnz(L_Exit);

    // Checks whether `p1` and `p2` are different (must be).
    cc.xor_(ret, ret);
    cc.cmp(p1, p2);
    cc.setz(ret.r8());

    cc.bind(L_Exit);
    cc.ret(ret);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 0; // Must be zero, stack addresses must be different.

    result.assign_int(result_ret);
    expect.assign_int(expect_ret);

    return result == expect;
  });
}

// STDCALL function that is called inside the generated one.
static int ASMJIT_STDCALL x86_compiler_func_call_std_called_fn(int a, int b, int c) noexcept {
  return (a + b) * c;
}

TEST_CASE(x86_compiler_func_call_std) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp x = cc.new_gp32("x");
    x86::Gp y = cc.new_gp32("y");
    x86::Gp z = cc.new_gp32("z");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());
    func_node->set_arg(0, x);
    func_node->set_arg(1, y);
    func_node->set_arg(2, z);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)x86_compiler_func_call_std_called_fn),
      FuncSignature::build<int, int, int, int>(CallConvId::kStdCall));
    invoke_node->set_arg(0, x);
    invoke_node->set_arg(1, y);
    invoke_node->set_arg(2, z);
    invoke_node->set_ret(0, x);

    cc.ret(x);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(1, 42, 3);
    int expect_ret = x86_compiler_func_call_std_called_fn(1, 42, 3);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

// FASTCALL function that is called inside the generated one.
static int ASMJIT_FASTCALL x86_compiler_func_call_fast_called_fn(int a) noexcept {
  return a * a;
}

TEST_CASE(x86_compiler_func_call_fast) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp var = cc.new_gp32("var");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int>());
    func_node->set_arg(0, var);

    InvokeNode* invoke_node;

    cc.invoke(Out(invoke_node), imm((void*)x86_compiler_func_call_fast_called_fn), FuncSignature::build<int, int>(CallConvId::kFastCall));
    invoke_node->set_arg(0, var);
    invoke_node->set_ret(0, var);

    cc.invoke(Out(invoke_node), imm((void*)x86_compiler_func_call_fast_called_fn), FuncSignature::build<int, int>(CallConvId::kFastCall));
    invoke_node->set_arg(0, var);
    invoke_node->set_ret(0, var);

    cc.ret(var);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(9);
    int expect_ret = (9 * 9) * (9 * 9);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

#if ASMJIT_TARGET_ARCH_X86
static __m128i x86_compiler_func_call_simd_called_fn_cdecl(__m128i a, __m128i b) {
  return _mm_add_epi8(a, b);
}

#ifdef _MSC_VER
static __m128i __vectorcall x86_compiler_func_call_simd_called_fn_vectorcall(__m128i a, __m128i b) {
  return _mm_add_epi8(a, b);
}
#endif

TEST_CASE(x86_compiler_func_call_simd) {
  for (bool use_vector_call : {false, true}) {
#ifndef _MSC_VER
    if (use_vector_call) {
      continue;
    }
#endif

    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    tester.compile([&](x86::Compiler& cc) {
      FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, const void*, const void*>());

      x86::Gp result_ptr = cc.new_gp_ptr("result_ptr");
      x86::Gp a_ptr = cc.new_gp_ptr("a_ptr");
      x86::Gp b_ptr = cc.new_gp_ptr("b_ptr");
      x86::Gp p_fn = cc.new_gp_ptr("p_fn");

      x86::Vec xmm_a = cc.new_xmm("xmm_a");
      x86::Vec xmm_b = cc.new_xmm("xmm_b");

      func_node->set_arg(0, result_ptr);
      func_node->set_arg(1, a_ptr);
      func_node->set_arg(2, b_ptr);

      CallConvId call_conv_id = CallConvId::kCDecl;
      Imm p_fn_imm = imm((void*)x86_compiler_func_call_simd_called_fn_cdecl);

#ifdef _MSC_VER
      if (use_vector_call) {
        call_conv_id = CallConvId::kVectorCall;
        p_fn_imm = imm((void*)x86_compiler_func_call_simd_called_fn_vectorcall);
      }
#endif

      cc.mov(p_fn, p_fn_imm);
      cc.movdqu(xmm_a, x86::ptr(a_ptr));
      cc.movdqu(xmm_b, x86::ptr(b_ptr));

      InvokeNode* invoke_node;
      cc.invoke(Out(invoke_node), p_fn, FuncSignature::build<Type::Vec128, Type::Vec128, Type::Vec128>(call_conv_id));

      invoke_node->set_arg(0, xmm_a);
      invoke_node->set_arg(1, xmm_b);
      invoke_node->set_ret(0, xmm_a);

      cc.movdqu(x86::ptr(result_ptr), xmm_a);

      cc.end_func();
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = void (*)(void*, const void*, const void*);
      Func func = ptr_as_func<Func>(func_ptr);

      uint8_t a_data[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
      uint8_t b_data[16] = { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 };

      uint8_t r_data[16] {};
      uint8_t e_data[16] = { 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15 };

      func(r_data, a_data, b_data);

      result.append_hex(r_data, 16);
      expect.append_hex(e_data, 16);

      return result == expect;
    });
  }
}
#endif // ASMJIT_TARGET_ARCH_X86

TEST_CASE(x86_compiler_func_call_light) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncSignature f1_signature = FuncSignature::build<void, const void*, const void*, const void*, const void*, void*>();
    FuncSignature f2_signature = FuncSignature::build<Type::Vec128, Type::Vec128, Type::Vec128>(CallConvId::kLightCall2);

    FuncNode* f1_node = cc.new_func(f1_signature);
    FuncNode* f2_node = cc.new_func(f2_signature);

    {
      x86::Gp a_ptr = cc.new_gp_ptr("a_ptr");
      x86::Gp b_ptr = cc.new_gp_ptr("b_ptr");
      x86::Gp c_ptr = cc.new_gp_ptr("c_ptr");
      x86::Gp d_ptr = cc.new_gp_ptr("d_ptr");
      x86::Gp p_out = cc.new_gp_ptr("p_out");

      x86::Vec xmm_a = cc.new_xmm("xmm_a");
      x86::Vec xmm_b = cc.new_xmm("xmm_b");
      x86::Vec xmm_c = cc.new_xmm("xmm_c");
      x86::Vec xmm_d = cc.new_xmm("xmm_d");

      cc.add_func(f1_node);
      f1_node->set_arg(0, a_ptr);
      f1_node->set_arg(1, b_ptr);
      f1_node->set_arg(2, c_ptr);
      f1_node->set_arg(3, d_ptr);
      f1_node->set_arg(4, p_out);

      cc.movups(xmm_a, x86::ptr(a_ptr));
      cc.movups(xmm_b, x86::ptr(b_ptr));
      cc.movups(xmm_c, x86::ptr(c_ptr));
      cc.movups(xmm_d, x86::ptr(d_ptr));

      x86::Vec xmm_x = cc.new_xmm("xmm_x");
      x86::Vec xmm_y = cc.new_xmm("xmm_y");

      InvokeNode* invoke_node;

      cc.invoke(Out(invoke_node), f2_node->label(), f2_signature);
      invoke_node->set_arg(0, xmm_a);
      invoke_node->set_arg(1, xmm_b);
      invoke_node->set_ret(0, xmm_x);

      cc.invoke(Out(invoke_node), f2_node->label(), f2_signature);
      invoke_node->set_arg(0, xmm_c);
      invoke_node->set_arg(1, xmm_d);
      invoke_node->set_ret(0, xmm_y);

      cc.pmullw(xmm_x, xmm_y);
      cc.movups(x86::ptr(p_out), xmm_x);

      cc.end_func();
    }

    {
      x86::Vec xmm_a = cc.new_xmm("xmm_a");
      x86::Vec xmm_b = cc.new_xmm("xmm_b");

      cc.add_func(f2_node);
      f2_node->set_arg(0, xmm_a);
      f2_node->set_arg(1, xmm_b);
      cc.paddw(xmm_a, xmm_b);
      cc.ret(xmm_a);
      cc.end_func();
    }
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(const void*, const void*, const void*, const void*, void*);

    Func func = ptr_as_func<Func>(func_ptr);

    int16_t a[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
    int16_t b[8] = { 7, 6, 5, 4, 3, 2, 1, 0 };
    int16_t c[8] = { 1, 3, 9, 7, 5, 4, 2, 1 };
    int16_t d[8] = { 2, 0,-6,-4,-2,-1, 1, 2 };

    int16_t o[8] {};
    int oexp = 7 * 3;

    func(a, b, c, d, o);

    result.assign_format("{%02X %02X %02X %02X %02X %02X %02X %02X}", o[0], o[1], o[2], o[3], o[4], o[5], o[6], o[7]);
    expect.assign_format("{%02X %02X %02X %02X %02X %02X %02X %02X}", oexp, oexp, oexp, oexp, oexp, oexp, oexp, oexp);

    return result == expect;
  });
}

static int x86_compiler_func_call_many_args_called_fn(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
  return (a * b * c * d * e) + (f * g * h * i * j);
}

TEST_CASE(x86_compiler_func_call_many_args) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    // Prepare.
    x86::Gp va = cc.new_gp32("va");
    x86::Gp vb = cc.new_gp32("vb");
    x86::Gp vc = cc.new_gp32("vc");
    x86::Gp vd = cc.new_gp32("vd");
    x86::Gp ve = cc.new_gp32("ve");
    x86::Gp vf = cc.new_gp32("vf");
    x86::Gp vg = cc.new_gp32("vg");
    x86::Gp vh = cc.new_gp32("vh");
    x86::Gp vi = cc.new_gp32("vi");
    x86::Gp vj = cc.new_gp32("vj");

    cc.mov(va, 0x03);
    cc.mov(vb, 0x12);
    cc.mov(vc, 0xA0);
    cc.mov(vd, 0x0B);
    cc.mov(ve, 0x2F);
    cc.mov(vf, 0x02);
    cc.mov(vg, 0x0C);
    cc.mov(vh, 0x12);
    cc.mov(vi, 0x18);
    cc.mov(vj, 0x1E);

    // Function call.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)x86_compiler_func_call_many_args_called_fn),
      FuncSignature::build<int, int, int, int, int, int, int, int, int, int, int>());
    invoke_node->set_arg(0, va);
    invoke_node->set_arg(1, vb);
    invoke_node->set_arg(2, vc);
    invoke_node->set_arg(3, vd);
    invoke_node->set_arg(4, ve);
    invoke_node->set_arg(5, vf);
    invoke_node->set_arg(6, vg);
    invoke_node->set_arg(7, vh);
    invoke_node->set_arg(8, vi);
    invoke_node->set_arg(9, vj);
    invoke_node->set_ret(0, va);

    cc.ret(va);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = x86_compiler_func_call_many_args_called_fn(0x03, 0x12, 0xA0, 0x0B, 0x2F, 0x02, 0x0C, 0x12, 0x18, 0x1E);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_duplicate_args) {
  using CalledFn = int (*)(int, int, int, int, int, int, int, int, int, int);
  CalledFn called_fn = [](int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) -> int {
    return (a * b * c * d * e) + (f * g * h * i * j);
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    // Prepare.
    x86::Gp a = cc.new_gp32("a");
    cc.mov(a, 3);

    // Call function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)called_fn),
      FuncSignature::build<int, int, int, int, int, int, int, int, int, int, int>());
    invoke_node->set_arg(0, a);
    invoke_node->set_arg(1, a);
    invoke_node->set_arg(2, a);
    invoke_node->set_arg(3, a);
    invoke_node->set_arg(4, a);
    invoke_node->set_arg(5, a);
    invoke_node->set_arg(6, a);
    invoke_node->set_arg(7, a);
    invoke_node->set_arg(8, a);
    invoke_node->set_arg(9, a);
    invoke_node->set_ret(0, a);

    cc.ret(a);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = called_fn(3, 3, 3, 3, 3, 3, 3, 3, 3, 3);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_imm_args) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    // Prepare.
    x86::Gp rv = cc.new_gp32("rv");

    // Call function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)x86_compiler_func_call_many_args_called_fn),
      FuncSignature::build<int, int, int, int, int, int, int, int, int, int, int>());

    invoke_node->set_arg(0, imm(0x03));
    invoke_node->set_arg(1, imm(0x12));
    invoke_node->set_arg(2, imm(0xA0));
    invoke_node->set_arg(3, imm(0x0B));
    invoke_node->set_arg(4, imm(0x2F));
    invoke_node->set_arg(5, imm(0x02));
    invoke_node->set_arg(6, imm(0x0C));
    invoke_node->set_arg(7, imm(0x12));
    invoke_node->set_arg(8, imm(0x18));
    invoke_node->set_arg(9, imm(0x1E));
    invoke_node->set_ret(0, rv);

    cc.ret(rv);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = x86_compiler_func_call_many_args_called_fn(0x03, 0x12, 0xA0, 0x0B, 0x2F, 0x02, 0x0C, 0x12, 0x18, 0x1E);

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_ptr_args) {
  using CalledFn = int (*)(void*, void*, void*, void*, void*, void*, void*, void*, void*, void*);
  CalledFn called_fn = [](void* a, void* b, void* c, void* d, void* e, void* f, void* g, void* h, void* i, void* j) -> int {
    return int((intptr_t)a) +
           int((intptr_t)b) +
           int((intptr_t)c) +
           int((intptr_t)d) +
           int((intptr_t)e) +
           int((intptr_t)f) +
           int((intptr_t)g) +
           int((intptr_t)h) +
           int((intptr_t)i) +
           int((intptr_t)j) ;
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    // Prepare.
    x86::Gp rv = cc.new_gp32("rv");

    // Call function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)called_fn),
      FuncSignature::build<int, void*, void*, void*, void*, void*, void*, void*, void*, void*, void*>());

    invoke_node->set_arg(0, imm(0x01));
    invoke_node->set_arg(1, imm(0x02));
    invoke_node->set_arg(2, imm(0x03));
    invoke_node->set_arg(3, imm(0x04));
    invoke_node->set_arg(4, imm(0x05));
    invoke_node->set_arg(5, imm(0x06));
    invoke_node->set_arg(6, imm(0x07));
    invoke_node->set_arg(7, imm(0x08));
    invoke_node->set_arg(8, imm(0x09));
    invoke_node->set_arg(9, imm(0x0A));
    invoke_node->set_ret(0, rv);

    cc.ret(rv);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 55;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_ref_args) {
  using CalledFn = int (*)(int&, int&, int&, int&);
  CalledFn called_fn = [](int& a, int& b, int& c, int& d) -> int {
    a += a;
    b += b;
    c += c;
    d += d;
    return a + b + c + d;
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int&, int&, int&, int&>());

    // Prepare.
    x86::Gp arg1 = cc.new_gp32();
    x86::Gp arg2 = cc.new_gp32();
    x86::Gp arg3 = cc.new_gp32();
    x86::Gp arg4 = cc.new_gp32();
    x86::Gp rv = cc.new_gp32("rv");

    func_node->set_arg(0, arg1);
    func_node->set_arg(1, arg2);
    func_node->set_arg(2, arg3);
    func_node->set_arg(3, arg4);

    // Call function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)called_fn),
      FuncSignature::build<int, int&, int&, int&, int&>());

    invoke_node->set_arg(0, arg1);
    invoke_node->set_arg(1, arg2);
    invoke_node->set_arg(2, arg3);
    invoke_node->set_arg(3, arg4);
    invoke_node->set_ret(0, rv);

    cc.ret(rv);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int&, int&, int&, int&);
    Func func = ptr_as_func<Func>(func_ptr);

    int inputs[4] = { 1, 2, 3, 4 };
    int outputs[4] = { 2, 4, 6, 8 };
    int result_ret = func(inputs[0], inputs[1], inputs[2], inputs[3]);
    int expect_ret = 20;

    result.assign_format("{%08X %08X %08X %08X %08X}", result_ret, inputs[0], inputs[1], inputs[2], inputs[3]);
    expect.assign_format("{%08X %08X %08X %08X %08X}", expect_ret, outputs[0], outputs[1], outputs[2], outputs[3]);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_float_as_xmm_ret) {
  using CalledFn = float (*)(float, float);
  CalledFn called_fn = [](float a, float b) -> float {
    return a * b;
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<float, float, float>());

    x86::Vec a = cc.new_xmm_ss("a");
    x86::Vec b = cc.new_xmm_ss("b");
    x86::Vec ret = cc.new_xmm_ss("ret");

    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    // Call function.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), imm((void*)called_fn), FuncSignature::build<float, float, float>());
    invoke_node->set_arg(0, a);
    invoke_node->set_arg(1, b);
    invoke_node->set_ret(0, ret);

    cc.ret(ret);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = float (*)(float, float);
    Func func = ptr_as_func<Func>(func_ptr);

    float result_ret = func(15.5f, 2.0f);
    float expect_ret = called_fn(15.5f, 2.0f);

    result.assign_format("%g", double(result_ret));
    expect.assign_format("%g", double(expect_ret));

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_double_as_xmm_ret) {
  using CalledFn = double (*)(double, double);
  CalledFn called_fn = [](double a, double b) -> double {
    return a * b;
  };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double, double>());

    x86::Vec a = cc.new_xmm_sd("a");
    x86::Vec b = cc.new_xmm_sd("b");
    x86::Vec ret = cc.new_xmm_sd("ret");

    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), imm((void*)called_fn), FuncSignature::build<double, double, double>());
    invoke_node->set_arg(0, a);
    invoke_node->set_arg(1, b);
    invoke_node->set_ret(0, ret);

    cc.ret(ret);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double, double);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = func(15.5, 2.0);
    double expect_ret = called_fn(15.5, 2.0);

    result.assign_format("%g", result_ret);
    expect.assign_format("%g", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_conditional) {
  using CalledFnAdd = int (*)(int, int);
  CalledFnAdd called_fn_add = [](int x, int y) -> int { return x + y; };

  using CalledFnMul = int (*)(int, int);
  CalledFnMul called_fn_mul = [](int x, int y) -> int { return x * y; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp x = cc.new_gp32("x");
    x86::Gp y = cc.new_gp32("y");
    x86::Gp op = cc.new_gp32("op");

    InvokeNode* invoke_node;
    x86::Gp result;

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());
    func_node->set_arg(0, x);
    func_node->set_arg(1, y);
    func_node->set_arg(2, op);

    Label op_add = cc.new_label();
    Label op_mul = cc.new_label();

    cc.cmp(op, 0);
    cc.jz(op_add);
    cc.cmp(op, 1);
    cc.jz(op_mul);

    result = cc.new_gp32("result_0");
    cc.mov(result, 0);
    cc.ret(result);

    cc.bind(op_add);
    result = cc.new_gp32("result_1");

    cc.invoke(Out(invoke_node), (uint64_t)called_fn_add, FuncSignature::build<int, int, int>());
    invoke_node->set_arg(0, x);
    invoke_node->set_arg(1, y);
    invoke_node->set_ret(0, result);
    cc.ret(result);

    cc.bind(op_mul);
    result = cc.new_gp32("result_2");

    cc.invoke(Out(invoke_node), (uint64_t)called_fn_mul, FuncSignature::build<int, int, int>());
    invoke_node->set_arg(0, x);
    invoke_node->set_arg(1, y);
    invoke_node->set_ret(0, result);

    cc.ret(result);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int arg1 = 4;
    int arg2 = 8;

    int result_add = func(arg1, arg2, 0);
    int expect_add = called_fn_add(arg1, arg2);

    int result_mul = func(arg1, arg2, 1);
    int expect_mul = called_fn_mul(arg1, arg2);

    result.assign_format("{add=%d, mul=%d}", result_add, result_mul);
    expect.assign_format("{add=%d, mul=%d}", expect_add, expect_mul);

    return (result_add == expect_add) && (result_mul == expect_mul);
  });
}

static int ASMJIT_FASTCALL x86_compiler_func_call_multiple_called_fn(int* p_int, int index) {
  return p_int[index];
}

TEST_CASE(x86_compiler_func_call_multiple) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    unsigned int i;

    x86::Gp buf = cc.new_gp_ptr("buf");
    x86::Gp acc0 = cc.new_gp32("acc0");
    x86::Gp acc1 = cc.new_gp32("acc1");

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int*>());
    func_node->set_arg(0, buf);

    cc.mov(acc0, 0);
    cc.mov(acc1, 0);

    for (i = 0; i < 4; i++) {
      x86::Gp ret = cc.new_gp32("ret");
      x86::Gp ptr = cc.new_gp_ptr("ptr");
      x86::Gp idx = cc.new_gp32("idx");
      InvokeNode* invoke_node;

      cc.mov(ptr, buf);
      cc.mov(idx, int(i));

      cc.invoke(Out(invoke_node), (uint64_t)x86_compiler_func_call_multiple_called_fn, FuncSignature::build<int, int*, int>(CallConvId::kFastCall));
      invoke_node->set_arg(0, ptr);
      invoke_node->set_arg(1, idx);
      invoke_node->set_ret(0, ret);

      cc.add(acc0, ret);

      cc.mov(ptr, buf);
      cc.mov(idx, int(i));

      cc.invoke(Out(invoke_node), (uint64_t)x86_compiler_func_call_multiple_called_fn, FuncSignature::build<int, int*, int>(CallConvId::kFastCall));
      invoke_node->set_arg(0, ptr);
      invoke_node->set_arg(1, idx);
      invoke_node->set_ret(0, ret);

      cc.sub(acc1, ret);
    }

    cc.add(acc0, acc1);
    cc.ret(acc0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int*);
    Func func = ptr_as_func<Func>(func_ptr);

    int buffer[4] = { 127, 87, 23, 17 };

    int result_ret = func(buffer);
    int expect_ret = 0;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_recursive) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    x86::Gp val = cc.new_gp32("val");
    Label skip = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int>());
    func_node->set_arg(0, val);

    cc.cmp(val, 1);
    cc.jle(skip);

    x86::Gp tmp = cc.new_gp32("tmp");
    cc.mov(tmp, val);
    cc.dec(tmp);

    InvokeNode* invoke_node;

    cc.invoke(Out(invoke_node), func_node->label(), FuncSignature::build<int, int>());
    invoke_node->set_arg(0, tmp);
    invoke_node->set_ret(0, tmp);
    cc.mul(cc.new_gp32(), val, tmp);

    cc.bind(skip);
    cc.ret(val);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(5);
    int expect_ret = 1 * 2 * 3 * 4 * 5;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

static int x86_compiler_func_call_var_arg1_called_fn(size_t n, ...) {
  int sum = 0;
  va_list ap;
  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    int arg = va_arg(ap, int);
    sum += arg;
  }
  va_end(ap);
  return sum;
}

TEST_CASE(x86_compiler_func_call_var_arg1) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int, int>());

    x86::Gp a0 = cc.new_gp32("a0");
    x86::Gp a1 = cc.new_gp32("a1");
    x86::Gp a2 = cc.new_gp32("a2");
    x86::Gp a3 = cc.new_gp32("a3");

    func_node->set_arg(0, a0);
    func_node->set_arg(1, a1);
    func_node->set_arg(2, a2);
    func_node->set_arg(3, a3);

    // We call `int func(size_t, ...)`
    //   - The `va_index` must be 1 (first argument after size_t).
    //   - The full signature of varargs (int, int, int, int) must follow.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)x86_compiler_func_call_var_arg1_called_fn),
      FuncSignature::build<int, size_t, int, int, int, int>(CallConvId::kCDecl, 1));
    invoke_node->set_arg(0, imm(4));
    invoke_node->set_arg(1, a0);
    invoke_node->set_arg(2, a1);
    invoke_node->set_arg(3, a2);
    invoke_node->set_arg(4, a3);
    invoke_node->set_ret(0, a0);

    cc.ret(a0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(1, 2, 3, 4);
    int expect_ret = 1 + 2 + 3 + 4;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

static double x86_compiler_func_call_var_arg2_called_fn(size_t n, ...) {
  double sum = 0;
  va_list ap;
  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    double arg = va_arg(ap, double);
    sum += arg;
  }
  va_end(ap);
  return sum;
}

TEST_CASE(x86_compiler_func_call_var_arg2) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double, double, double, double>());

    x86::Vec a0 = cc.new_xmm_sd("a0");
    x86::Vec a1 = cc.new_xmm_sd("a1");
    x86::Vec a2 = cc.new_xmm_sd("a2");
    x86::Vec a3 = cc.new_xmm_sd("a3");

    func_node->set_arg(0, a0);
    func_node->set_arg(1, a1);
    func_node->set_arg(2, a2);
    func_node->set_arg(3, a3);

    // We call `double func(size_t, ...)`
    //   - The `va_index` must be 1 (first argument after size_t).
    //   - The full signature of varargs (double, double, double, double) must follow.
    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)x86_compiler_func_call_var_arg2_called_fn),
      FuncSignature::build<double, size_t, double, double, double, double>(CallConvId::kCDecl, 1));
    invoke_node->set_arg(0, imm(4));
    invoke_node->set_arg(1, a0);
    invoke_node->set_arg(2, a1);
    invoke_node->set_arg(3, a2);
    invoke_node->set_arg(4, a3);
    invoke_node->set_ret(0, a0);

    cc.ret(a0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double, double, double, double);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = func(1.0, 2.0, 3.0, 4.0);
    double expect_ret = 1.0 + 2.0 + 3.0 + 4.0;

    result.assign_format("%f", result_ret);
    expect.assign_format("%f", expect_ret);

    return result == expect;
  });
}

[[maybe_unused]] static double x86_compiler_func_call_int64_arg_called_fn(size_t n, ...) {
  double sum = 0;
  va_list ap;
  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    double arg = va_arg(ap, double);
    sum += arg;
  }
  va_end(ap);
  return sum;
}

TEST_CASE(x86_compiler_func_call_int64_arg) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<uint64_t, uint64_t>());

    if (cc.is_64bit()) {
      x86::Gp reg = cc.new_gp64();
      func_node->set_arg(0, reg);
      cc.add(reg, 1);
      cc.ret(reg);
    }
    else {
      x86::Gp hi = cc.new_gp32("hi");
      x86::Gp lo = cc.new_gp32("lo");

      func_node->set_arg(0, 0, lo);
      func_node->set_arg(0, 1, hi);

      cc.add(lo, 1);
      cc.adc(hi, 0);
      cc.ret(lo, hi);
    }

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint64_t (*)(uint64_t);
    Func func = ptr_as_func<Func>(func_ptr);

    uint64_t result_ret = func(uint64_t(0xFFFFFFFF));
    uint64_t expect_ret = 0x100000000;

    result.assign_format("%llu", (unsigned long long)result_ret);
    expect.assign_format("%llu", (unsigned long long)expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_misc1) {
  using Dummy = void (*)(int, int);
  Dummy dummy = [](int, int) -> void {};

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int>());

    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp32("b");
    x86::Gp r = cc.new_gp32("r");

    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)dummy),
      FuncSignature::build<void, int, int>());
    invoke_node->set_arg(0, a);
    invoke_node->set_arg(1, b);

    cc.lea(r, x86::ptr(a, b));
    cc.ret(r);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(44, 199);
    int expect_ret = 243;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_misc2) {
  using Op = double (*)(double);
  Op op = [](double a) -> double { return a * a; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, const double*>());

    x86::Gp p = cc.new_gp_ptr("p");
    x86::Vec arg = cc.new_xmm_sd("arg");
    x86::Vec ret = cc.new_xmm_sd("ret");

    func_node->set_arg(0, p);
    cc.movsd(arg, x86::ptr(p));

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)op),
      FuncSignature::build<double, double>());
    invoke_node->set_arg(0, arg);
    invoke_node->set_ret(0, ret);

    cc.ret(ret);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(const double*);
    Func func = ptr_as_func<Func>(func_ptr);

    double arg = 2;

    double result_ret = func(&arg);
    double expect_ret = op(arg);

    result.assign_format("%g", result_ret);
    expect.assign_format("%g", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_misc3) {
  using Op = double (*)(double);
  Op op = [](double a) -> double { return a * a; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, const double*>());

    x86::Gp p = cc.new_gp_ptr("p");
    x86::Vec arg = cc.new_xmm_sd("arg");
    x86::Vec ret = cc.new_xmm_sd("ret");

    func_node->set_arg(0, p);
    cc.movsd(arg, x86::ptr(p));

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node),
      imm((void*)op),
      FuncSignature::build<double, double>());
    invoke_node->set_arg(0, arg);
    invoke_node->set_ret(0, ret);

    cc.xorps(arg, arg);
    cc.subsd(arg, ret);

    cc.ret(arg);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(const double*);
    Func func = ptr_as_func<Func>(func_ptr);

    double arg = 2;

    double result_ret = func(&arg);
    double expect_ret = -op(arg);

    result.assign_format("%g", result_ret);
    expect.assign_format("%g", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_misc4) {
  using CalledFn = double (*)();
  CalledFn called_fn = []() -> double { return 3.14; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    InvokeNode* invoke_node;

    FuncSignature func_signature;
    func_signature.set_call_conv_id(CallConvId::kCDecl);
    func_signature.set_ret(TypeId::kFloat64);
    cc.add_func(func_signature);

    FuncSignature invoke_signature;
    invoke_signature.set_call_conv_id(CallConvId::kCDecl);
    invoke_signature.set_ret(TypeId::kFloat64);

    cc.invoke(Out(invoke_node), imm((void*)called_fn), invoke_signature);
    x86::Vec ret = cc.new_xmm_sd("ret");
    invoke_node->set_ret(0, ret);
    cc.ret(ret);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    double result_ret = func();
    double expect_ret = 3.14;

    result.assign_format("%g", result_ret);
    expect.assign_format("%g", expect_ret);

    return result == expect;
  });
}

// The register allocator should clobber the register used by the `call` itself.
TEST_CASE(x86_compiler_func_call_misc5) {
  using CalledFn = void (*)();
  CalledFn called_fn = []() -> void {};

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    x86::Gp p_fn = cc.new_gp_ptr("p_fn");
    x86::Gp vars[16];

    uint32_t i, reg_count = cc.arch() == Arch::kX86 ? 8 : 16;
    ASMJIT_ASSERT(reg_count <= ASMJIT_ARRAY_SIZE(vars));

    cc.mov(p_fn, imm((void*)called_fn));

    for (i = 0; i < reg_count; i++) {
      if (i == x86::Gp::kIdBp || i == x86::Gp::kIdSp)
        continue;

      vars[i] = cc.new_gp32("%%%u", unsigned(i));
      cc.mov(vars[i], 1);
    }

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), p_fn, FuncSignature::build<void>());

    for (i = 1; i < reg_count; i++)
      if (vars[i].is_valid())
        cc.add(vars[0], vars[i]);
    cc.ret(vars[0]);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = sizeof(void*) == 4 ? 6 : 14;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_misc6) {
  using CalledFn = uint32_t (*)(uint32_t);
  CalledFn called_fn = [](uint32_t x) -> uint32_t { return x + 1; };

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<uint32_t, uint32_t>());

    constexpr uint32_t kCount = 16;

    x86::Gp v[kCount];
    x86::Gp arg_val = cc.new_gp32("arg_val");
    x86::Gp ret_val = cc.new_gp32("ret_val");

    func_node->set_arg(0, arg_val);
    cc.add(arg_val, 1);

    for (uint32_t i = 0; i < kCount; i++) {
      v[i] = cc.new_gp32("v%u", i);
    }

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), imm((void*)called_fn), FuncSignature::build<uint32_t, uint32_t>());
    invoke_node->set_arg(0, arg_val);
    invoke_node->set_ret(0, ret_val);

    for (uint32_t i = 0; i < kCount; i++) {
      cc.mov(v[i], i + 1);
    }

    for (uint32_t i = 0; i < kCount; i++) {
      cc.add(arg_val, v[i]);
    }

    cc.add(ret_val, arg_val);
    cc.ret(ret_val);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t x);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t result_ret = func(111);
    uint32_t expect_ret = 111 + 112 + 2 + (1 + 16) * 8;

    result.assign_format("%u", result_ret);
    expect.assign_format("%u", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_func_call_avx_clobber) {
  const CpuInfo& cpu_info = CpuInfo::host();

  if (cpu_info.features().x86().has_avx2() && sizeof(void*) == 8) {
    CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

    tester.compile([&](x86::Compiler& cc) {
      FuncNode* main_func = cc.add_func(FuncSignature::build<void, void*, const void*, const void*>());
      main_func->frame().set_avx_enabled();
      main_func->frame().set_avx_cleanup();

      // We need a Windows calling convention to test this properly also on a non-Windows machine.
      FuncNode* helper_func = cc.new_func(FuncSignature::build<void, void*, const void*>(CallConvId::kX64Windows));
      helper_func->frame().set_avx_enabled();
      helper_func->frame().set_avx_cleanup();

      {
        size_t i;

        x86::Gp d_ptr = cc.new_gp_ptr("d_ptr");
        x86::Gp a_ptr = cc.new_gp_ptr("a_ptr");
        x86::Gp b_ptr = cc.new_gp_ptr("b_ptr");
        x86::Gp t_ptr = cc.new_gp_ptr("t_ptr");
        x86::Vec acc[8];
        x86::Mem stack = cc.new_stack(32, 1, "stack");

        main_func->set_arg(0, d_ptr);
        main_func->set_arg(1, a_ptr);
        main_func->set_arg(2, b_ptr);

        cc.lea(t_ptr, stack);
        for (i = 0; i < 8; i++) {
          acc[i] = cc.new_ymm("acc%zu", i);
          cc.vmovdqu(acc[i], x86::ptr(a_ptr));
        }

        InvokeNode* invoke_node;
        cc.invoke(Out(invoke_node),
          helper_func->label(),
          FuncSignature::build<void, void*, const void*>(CallConvId::kX64Windows));
        invoke_node->set_arg(0, t_ptr);
        invoke_node->set_arg(1, b_ptr);

        for (i = 1; i < 8; i++) {
          cc.vpaddd(acc[0], acc[0], acc[i]);
        }

        cc.vpaddd(acc[0], acc[0], x86::ptr(t_ptr));
        cc.vmovdqu(x86::ptr(d_ptr), acc[0]);

        cc.end_func();
      }

      {
        cc.add_func(helper_func);

        x86::Gp d_ptr = cc.new_gp_ptr("d_ptr");
        x86::Gp a_ptr = cc.new_gp_ptr("a_ptr");

        helper_func->set_arg(0, d_ptr);
        helper_func->set_arg(1, a_ptr);

        x86::Gp tmp = cc.new_gp_ptr("tmp");
        x86::Vec acc = cc.new_ymm("acc");

        cc.mov(tmp, 1);
        cc.vmovd(acc.xmm(), tmp);
        cc.vpbroadcastd(acc, acc.xmm());
        cc.vpaddd(acc, acc, x86::ptr(a_ptr));
        cc.vmovdqu(x86::ptr(d_ptr), acc);

        cc.end_func();
      }
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = void (*)(void*, const void*, const void*);
      Func func = ptr_as_func<Func>(func_ptr);

      size_t i;

      static const uint32_t a_data[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
      static const uint32_t b_data[8] = { 6, 3, 5, 9, 1, 8, 7, 2 };

      uint32_t result_data[8] {};
      uint32_t expect_data[8] {};

      for (i = 0; i < 8; i++)
        expect_data[i] = a_data[i] * 8 + b_data[i] + 1;

      func(result_data, a_data, b_data);

      stringify_array(result, "%u", result_data, 8);
      stringify_array(expect, "%u", expect_data, 8);

      return result == expect;
    });
  }
}

TEST_CASE(x86_compiler_vec_to_scalar) {
  constexpr uint32_t kVecCount = 64;

  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func = cc.add_func(FuncSignature::build<uint32_t, uint32_t>());

    x86::Gp x = cc.new_gp32("x");
    x86::Gp t = cc.new_gp32("t");
    x86::Vec v[kVecCount];

    func->set_arg(0, x);

    for (size_t i = 0; i < kVecCount; i++) {
      v[i] = cc.new_xmm("v%d", i);
      if (i != 0)
        cc.add(x, 1);
      cc.movd(v[i], x);
    }

    cc.xor_(x, x);

    for (size_t i = 0; i < kVecCount; i++) {
      cc.movd(t, v[i]);
      cc.add(x, t);
    }

    cc.ret(x);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t);
    Func func = ptr_as_func<Func>(func_ptr);

    uint32_t result_ret = func(1);
    uint32_t expect_ret = 2080; // 1 + 2 + 3 + ... + 64

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_misc_local_const_pool) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");

    x86::Mem c0 = cc.new_int32_const(ConstPoolScope::kLocal, 200);
    x86::Mem c1 = cc.new_int32_const(ConstPoolScope::kLocal, 33);

    cc.mov(v0, c0);
    cc.mov(v1, c1);
    cc.add(v0, v1);

    cc.ret(v0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 233;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_misc_global_const_pool) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    x86::Gp v0 = cc.new_gp32("v0");
    x86::Gp v1 = cc.new_gp32("v1");

    x86::Mem c0 = cc.new_int32_const(ConstPoolScope::kGlobal, 200);
    x86::Mem c1 = cc.new_int32_const(ConstPoolScope::kGlobal, 33);

    cc.mov(v0, c0);
    cc.mov(v1, c1);
    cc.add(v0, v1);

    cc.ret(v0);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func();
    int expect_ret = 233;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_misc_multi_ret) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, int, int>());

    x86::Gp op = cc.new_gp32("op");
    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp32("b");

    Label L_Zero = cc.new_label();
    Label L_Add = cc.new_label();
    Label L_Sub = cc.new_label();
    Label L_Mul = cc.new_label();
    Label L_Div = cc.new_label();

    func_node->set_arg(0, op);
    func_node->set_arg(1, a);
    func_node->set_arg(2, b);

    cc.cmp(op, 0);
    cc.jz(L_Add);

    cc.cmp(op, 1);
    cc.jz(L_Sub);

    cc.cmp(op, 2);
    cc.jz(L_Mul);

    cc.cmp(op, 3);
    cc.jz(L_Div);

    cc.bind(L_Zero);
    cc.xor_(a, a);
    cc.ret(a);

    cc.bind(L_Add);
    cc.add(a, b);
    cc.ret(a);

    cc.bind(L_Sub);
    cc.sub(a, b);
    cc.ret(a);

    cc.bind(L_Mul);
    cc.imul(a, b);
    cc.ret(a);

    cc.bind(L_Div);
    cc.cmp(b, 0);
    cc.jz(L_Zero);

    x86::Gp zero = cc.new_gp32("zero");
    cc.xor_(zero, zero);
    cc.idiv(zero, a, b);
    cc.ret(a);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int, int);

    Func func = ptr_as_func<Func>(func_ptr);

    int a = 44;
    int b = 3;

    int r0 = func(0, a, b);
    int r1 = func(1, a, b);
    int r2 = func(2, a, b);
    int r3 = func(3, a, b);
    int e0 = a + b;
    int e1 = a - b;
    int e2 = a * b;
    int e3 = a / b;

    result.assign_format("{%d %d %d %d}", r0, r1, r2, r3);
    expect.assign_format("{%d %d %d %d}", e0, e1, e2, e3);

    return result == expect;
  });
}

TEST_CASE(x86_compiler_misc_multi_func) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    FuncNode* f1_node = cc.new_func(FuncSignature::build<int, int, int>());
    FuncNode* f2_node = cc.new_func(FuncSignature::build<int, int, int>());

    {
      x86::Gp a = cc.new_gp32("a");
      x86::Gp b = cc.new_gp32("b");

      cc.add_func(f1_node);
      f1_node->set_arg(0, a);
      f1_node->set_arg(1, b);

      InvokeNode* invoke_node;
      cc.invoke(Out(invoke_node), f2_node->label(), FuncSignature::build<int, int, int>());
      invoke_node->set_arg(0, a);
      invoke_node->set_arg(1, b);
      invoke_node->set_ret(0, a);

      cc.ret(a);
      cc.end_func();
    }

    {
      x86::Gp a = cc.new_gp32("a");
      x86::Gp b = cc.new_gp32("b");

      cc.add_func(f2_node);
      f2_node->set_arg(0, a);
      f2_node->set_arg(1, b);

      cc.add(a, b);
      cc.ret(a);
      cc.end_func();
    }
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(int, int);

    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = func(56, 22);
    int expect_ret = 56 + 22;

    result.assign_format("%d", result_ret);
    expect.assign_format("%d", expect_ret);

    return result == expect;
  });
}

// Global (I didn't find a better way to test this).
static jmp_buf global_jmp_buf;

static void ASMJIT_FASTCALL x86_compiler_misc_unfollow_handler() { longjmp(global_jmp_buf, 1); }

TEST_CASE(x86_compiler_misc_unfollow) {
  CompilerTester<x86::Compiler> tester(Arch::kHost == Arch::kX86 ? Arch::kX86 : Arch::kX64, test_runner);

  tester.compile([&](x86::Compiler& cc) {
    // NOTE: Fastcall calling convention is the most appropriate here as all arguments are passed via registers and
    // there won't be any stack misalignment in the `handler()`. This was failing on MacOS when targeting 32-bit mode.
    x86::Gp a = cc.new_gp32("a");
    x86::Gp b = cc.new_gp_ptr("b");
    Label tramp = cc.new_label();

    FuncNode* func_node = cc.add_func(FuncSignature::build<int, int, void*>(CallConvId::kFastCall));
    func_node->set_arg(0, a);
    func_node->set_arg(1, b);

    cc.cmp(a, 0);
    cc.jz(tramp);
    cc.ret(a);
    cc.bind(tramp);
    cc.unfollow().jmp(b);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (ASMJIT_FASTCALL*)(int, void*);
    Func func = ptr_as_func<Func>(func_ptr);

    int result_ret = 0;
    int expect_ret = 1;

    if (!setjmp(global_jmp_buf))
      result_ret = func(0, (void*)x86_compiler_misc_unfollow_handler);
    else
      result_ret = 1;

    result.assign_format("{%d}", result_ret);
    expect.assign_format("{%d}", expect_ret);

    return result == expect;
  });
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_X86 && !ASMJIT_NO_COMPILER
