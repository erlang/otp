// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_COMPILER)

#include <asmjit/core/compiler_test_p.h>
#include <asmjit/arm/a64_compiler.h>

#include <initializer_list>

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

TEST_CASE(a64_compiler_func_no_code) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    cc.add_func(FuncSignature::build<void>());
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, [[maybe_unused]] String& result, [[maybe_unused]] String& expect) {
    using Func = void (*)(void);
    ptr_as_func<Func>(func_ptr)();
    return true;
  });
}

TEST_CASE(a64_compiler_func_no_align) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
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

TEST_CASE(a64_compiler_const_pool) {
  for (ConstPoolScope scope : {ConstPoolScope::kLocal, ConstPoolScope::kGlobal}) {
    CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

    float f_val = 1.234f;
    double d_val = 1.234;

    tester.compile([&](a64::Compiler& cc) {
      FuncNode* fn = cc.add_func(FuncSignature::build<void, void*>());

      a64::Mem m_u8 = cc.new_byte_const(scope, 0x81u);
      a64::Mem m_i16 = cc.new_int16_const(scope, static_cast<int16_t>(0xF123u));
      a64::Mem m_u16 = cc.new_uint16_const(scope, 0x8123u);
      a64::Mem m_i32 = cc.new_int32_const(scope, static_cast<int32_t>(0xF1234567u));
      a64::Mem m_u32 = cc.new_uint32_const(scope, 0x81234567u);
      a64::Mem m_f32 = cc.new_float_const(scope, f_val);
      a64::Mem m_i64 = cc.new_int64_const(scope, static_cast<int64_t>(0xF123456789ABCDEF));
      a64::Mem m_u64 = cc.new_uint64_const(scope, 0x8123456789ABCDEFu);
      a64::Mem m_f64 = cc.new_double_const(scope, d_val);

      a64::Gp out_ptr = cc.new_gp_ptr("out_ptr");
      fn->set_arg(0, out_ptr);

      a64::Gp tmp_gp = cc.new_gp32("tmp_gp");
      a64::Gp tmp_addr = cc.new_gp64("tmp_addr");
      a64::Vec tmp_vec = cc.new_vec128("tmp_vec");

      cc.load_address_of(tmp_addr, m_u8);
      cc.ldrb(tmp_gp, ptr(tmp_addr));
      cc.str(tmp_gp, ptr(out_ptr));

      cc.load_address_of(tmp_addr, m_i16);
      cc.ldrsh(tmp_gp, ptr(tmp_addr));
      cc.str(tmp_gp, ptr(out_ptr, 4));

      cc.load_address_of(tmp_addr, m_u16);
      cc.ldrh(tmp_gp, ptr(tmp_addr));
      cc.str(tmp_gp, ptr(out_ptr, 8));

      cc.ldr(tmp_gp, m_i32);
      cc.str(tmp_gp, ptr(out_ptr, 12));

      cc.ldr(tmp_gp, m_u32);
      cc.str(tmp_gp, ptr(out_ptr, 16));

      cc.ldr(tmp_gp, m_f32);
      cc.str(tmp_gp, ptr(out_ptr, 20));

      cc.ldr(tmp_vec.d(), m_i64);
      cc.str(tmp_vec.d(), ptr(out_ptr, 24));

      cc.ldr(tmp_vec.d(), m_u64);
      cc.str(tmp_vec.d(), ptr(out_ptr, 32));

      cc.ldr(tmp_vec.d(), m_f64);
      cc.str(tmp_vec.d(), ptr(out_ptr, 40));

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

TEST_CASE(a64_compiler_func_alignment) {
  for (uint32_t arg_count = 0; arg_count <= 16; arg_count++) {
    for (bool preserve_fp : {false, true}) {
      CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

      tester.compile([&](a64::Compiler& cc) {
        FuncSignature signature;
        signature.set_ret_t<uint32_t>();
        for (uint32_t i = 0; i < arg_count; i++) {
          signature.add_arg_t<uint32_t>();
        }

        FuncNode* func_node = cc.add_func(signature);
        if (preserve_fp) {
          func_node->frame().set_preserved_fp();
        }

        a64::Gp sum;
        if (arg_count) {
          for (uint32_t i = 0; i < arg_count; i++) {
            a64::Gp i_reg = cc.new_gp32("i%u", i);
            func_node->set_arg(i, i_reg);
            if (i == 0)
              sum = i_reg;
            else
              cc.add(sum, sum, i_reg);
          }
        }
        else {
          sum = cc.new_gp32("i");
          cc.mov(sum, 0);
        }

        cc.ret(sum);
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

        result.assign_format("ret={%u, %u}", result_ret >> 28, result_ret & 0x0FFFFFFFu);
        expect.assign_format("ret={%u, %u}", expect_ret >> 28, expect_ret & 0x0FFFFFFFu);

        return result == expect;
      });
    }
  }
}

TEST_CASE(a64_compiler_simd_1) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, const void*, const void*>());

    a64::Gp dst = cc.new_gp_ptr("dst");
    a64::Gp src1 = cc.new_gp_ptr("src1");
    a64::Gp src2 = cc.new_gp_ptr("src2");

    func_node->set_arg(0, dst);
    func_node->set_arg(1, src1);
    func_node->set_arg(2, src2);

    a64::Vec v1 = cc.new_vec_q("vec1");
    a64::Vec v2 = cc.new_vec_q("vec2");
    a64::Vec v3 = cc.new_vec_q("vec3");

    cc.ldr(v2, a64::ptr(src1));
    cc.ldr(v3, a64::ptr(src2));
    cc.add(v1.b16(), v2.b16(), v3.b16());
    cc.str(v1, a64::ptr(dst));

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void*, const void*, const void*);

    uint32_t dst[4];
    uint32_t a_src[4] = { 0 , 1 , 2 , 255 };
    uint32_t b_src[4] = { 99, 17, 33, 1   };

    // NOTE: It's a byte-add, so uint8_t(255+1) == 0.
    uint32_t ref[4] = { 99, 18, 35, 0 };

    ptr_as_func<Func>(func_ptr)(dst, a_src, b_src);

    result.assign_format("ret={%u, %u, %u, %u}", dst[0], dst[1], dst[2], dst[3]);
    expect.assign_format("ret={%u, %u, %u, %u}", ref[0], ref[1], ref[2], ref[3]);

    return result == expect;
  });
}

// Regression coverage for https://github.com/asmjit/asmjit/issues/533.
TEST_CASE(a64_compiler_simd_preserve_dst) {
  enum Op : uint32_t {
    kSri,
    kSriScalar,
    kAddhn2,
    kBfcvtn2,
    kTbx,
    kBicImm,
    kOrrImm,
    kBicShift,
    kOrrShift,
    kBicReg,
    kOrrReg,
    kOpCount
  };

  for (uint32_t op = 0; op < kOpCount; op++) {
    if (op == kBfcvtn2 && Arch::kHost == Arch::kAArch64 && !CpuInfo::host().features().arm().has_bf16()) {
      continue;
    }

    CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

    tester.compile([&](a64::Compiler& cc) {
      FuncSignature called_signature = FuncSignature::build<Type::Vec128, Type::Vec128, Type::Vec128, Type::Vec128>();

      FuncNode* main_fn = cc.add_func(FuncSignature::build<void, void*, const void*>());
      FuncNode* called_fn = cc.new_func(called_signature);

      {
        a64::Gp dst = cc.new_gp_ptr("dst");
        a64::Gp src = cc.new_gp_ptr("src");
        a64::Vec v[3] = {cc.new_vec_q("v0"), cc.new_vec_q("v1"), cc.new_vec_q("v2")};

        main_fn->set_arg(0, dst);
        main_fn->set_arg(1, src);

        cc.ldr(v[0], a64::ptr(src, 0 * 16));
        cc.ldr(v[1], a64::ptr(src, 1 * 16));
        cc.ldr(v[2], a64::ptr(src, 2 * 16));

        InvokeNode* invoke_node;
        cc.invoke(Out(invoke_node), called_fn->label(), called_signature);

        invoke_node->set_arg(0, v[0]);
        invoke_node->set_arg(1, v[1]);
        invoke_node->set_arg(2, v[2]);
        invoke_node->set_ret(0, v[0]);

        cc.str(v[0], a64::ptr(dst));
        cc.end_func();
      }

      {
        cc.add_func(called_fn);

        a64::Vec a = cc.new_vec_q("a");
        a64::Vec b = cc.new_vec_q("b");
        a64::Vec dst = cc.new_vec_q("dst");

        called_fn->set_arg(0, a);
        called_fn->set_arg(1, b);
        called_fn->set_arg(2, dst);

        // The incoming destination is v2, whereas the result must be returned in v0.
        // Write-only metadata would allow the allocator to discard the initial v2.
        switch (op) {
          case kSri      : cc.sri(dst.d2(), a.d2(), 61); break;
          case kSriScalar: cc.sri(dst.d(), a.d(), 61); break;
          case kAddhn2   : cc.addhn2(dst.b16(), a.h8(), b.h8()); break;
          case kBfcvtn2  : cc.bfcvtn2(dst.h8(), a.s4()); break;
          case kTbx      : cc.tbx(dst.b16(), a.b16(), b.b16()); break;
          case kBicImm   : cc.bic(dst.s4(), 0x12); break;
          case kOrrImm   : cc.orr(dst.s4(), 0x12); break;
          case kBicShift : cc.bic(dst.s4(), 0x12, a64::lsl(8)); break;
          case kOrrShift : cc.orr(dst.s4(), 0x12, a64::lsl(8)); break;
          case kBicReg   : cc.bic(dst.b16(), a.b16(), b.b16()); break;
          case kOrrReg   : cc.orr(dst.b16(), a.b16(), b.b16()); break;
          default: break;
        }

        cc.ret(dst);
        cc.end_func();
      }
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = void (*)(void*, const void*);

      // The first vector also contains exactly representable BF16 values (1, 2, 3, 4).
      const uint32_t src[12] = {
        0x3F800000u, 0x40000000u, 0x40400000u, 0x40800000u,
        0xFF100F00u, 0x80110E01u, 0xFE1F0D02u, 0x7F200C03u,
        0xA5B6C7D8u, 0x12345678u, 0x9ABCDEF0u, 0xFEDCBA98u
      };

      const uint8_t* bytes = reinterpret_cast<const uint8_t*>(src);
      uint8_t dst[16] = {};
      uint8_t ref[16];

      memcpy(ref, src + 8, 16);

      switch (op) {
        case kSri:
        case kSriScalar: {
          for (uint32_t i = 0; i < 2; i++) {
            uint64_t a;
            uint64_t d;
            memcpy(&a, bytes + i * 8, 8);
            memcpy(&d, ref + i * 8, 8);
            d = (d & ~uint64_t(7)) | (a >> 61);
            if (op == kSriScalar && i == 1) {
              d = 0;
            }
            memcpy(ref + i * 8, &d, 8);
          }
          break;
        }

        case kAddhn2: {
          for (uint32_t i = 0; i < 8; i++) {
            uint16_t a;
            uint16_t b;
            memcpy(&a, bytes + i * 2, 2);
            memcpy(&b, bytes + 16 + i * 2, 2);
            ref[8 + i] = uint8_t((uint32_t(a) + b) >> 8);
          }
          break;
        }

        case kBfcvtn2: {
          for (uint32_t i = 0; i < 4; i++) {
            uint16_t value = uint16_t(src[i] >> 16);
            memcpy(ref + 8 + i * 2, &value, 2);
          }
          break;
        }

        case kTbx: {
          for (uint32_t i = 0; i < 16; i++) {
            uint32_t index = bytes[16 + i];
            if (index < 16) {
              ref[i] = bytes[index];
            }
          }
          break;
        }

        case kBicImm:
        case kOrrImm:
        case kBicShift:
        case kOrrShift: {
          for (uint32_t i = 0; i < 4; i++) {
            uint32_t mask = (op == kBicShift || op == kOrrShift) ? 0x1200u : 0x12u;
            uint32_t value = src[8 + i];
            value = (op == kBicImm || op == kBicShift) ? value & ~mask : value | mask;
            memcpy(ref + i * 4, &value, 4);
          }
          break;
        }

        case kBicReg:
        case kOrrReg: {
          for (uint32_t i = 0; i < 16; i++) {
            ref[i] = op == kBicReg ? uint8_t(bytes[i] & ~bytes[16 + i]) : uint8_t(bytes[i] | bytes[16 + i]);
          }
          break;
        }

        default: {
          break;
        }
      }

      ptr_as_func<Func>(func_ptr)(dst, src);
      result.assign_hex(dst, 16);
      expect.assign_hex(ref, 16);

      return result == expect;
    });
  }
}

TEST_CASE(a64_compiler_many_regs) {
  static constexpr uint32_t kMaxRegCount = 64;
  auto calc_sum = [](uint32_t n) { return (n | 1u) * ((n + 1u) / 2u); };

  for (uint32_t reg_count = 2; reg_count < kMaxRegCount; reg_count++) {
    CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

    tester.compile([&](a64::Compiler& cc) {
      cc.add_func(FuncSignature::build<int>());

      a64::Gp regs[kMaxRegCount];
      for (uint32_t i = 0; i < reg_count; i++) {
        regs[i] = cc.new_gp32("reg%u", i);
        cc.mov(regs[i], i + 1);
      }

      a64::Gp sum = cc.new_gp32("sum");
      cc.mov(sum, 0);

      for (uint32_t i = 0; i < reg_count; i++) {
        cc.add(sum, sum, regs[i]);
      }

      cc.ret(sum);
      cc.end_func();
    });

    tester.execute([&](void* func_ptr, String& result, String& expect) {
      using Func = int (*)(void);

      result.assign_format("ret={%d}", ptr_as_func<Func>(func_ptr)());
      expect.assign_format("ret={%d}", calc_sum(reg_count));

      return result == expect;
    });
  }
}

TEST_CASE(a64_compiler_adr) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    cc.add_func(FuncSignature::build<int>());

    a64::Gp addr = cc.new_gp_ptr("addr");
    a64::Gp val = cc.new_gp_ptr("val");

    Label L_Table = cc.new_label();

    cc.adr(addr, L_Table);
    cc.ldrsw(val, a64::ptr(addr, 8));
    cc.ret(val);
    cc.end_func();

    cc.bind(L_Table);
    cc.embed_int32(1);
    cc.embed_int32(2);
    cc.embed_int32(3);
    cc.embed_int32(4);
    cc.embed_int32(5);
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = int (*)(void);

    result.assign_format("ret={%d}", ptr_as_func<Func>(func_ptr)());
    expect.assign_format("ret={%d}", 3);

    return result == expect;
  });
}

TEST_CASE(a64_compiler_load_address_of_label_offset) {
  const int32_t offsets[] = {
    0,
    1, -1,
    0xFFF, -0xFFF,
    0x1000, -0x1000,
    0x1001, -0x1001,
    0x7FFFF, -0x7FFFF
  };

  for (int32_t offset : offsets) {
    for (bool forward : {false, true}) {
      CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

      tester.compile([&](a64::Compiler& cc) {
        cc.add_func(FuncSignature::build<int64_t>());

        Label label = cc.new_label();
        if (!forward) {
          cc.bind(label);
        }

        a64::Gp addr = cc.new_gp_ptr("addr");
        a64::Gp base = cc.new_gp_ptr("base");
        cc.load_address_of(addr, a64::ptr(label, offset));
        cc.adr(base, label);
        cc.sub(addr, addr, base);
        cc.ret(addr);

        if (forward) {
          cc.bind(label);
        }
        cc.end_func();
      });

      tester.execute([&](void* func_ptr, String& result, String& expect) {
        using Func = int64_t (*)(void);
        int64_t value = ptr_as_func<Func>(func_ptr)();

        result.assign_format("forward={%u} offset={%d} ret={%lld}", unsigned(forward), offset, static_cast<long long>(value));
        expect.assign_format("forward={%u} offset={%d} ret={%lld}", unsigned(forward), offset, static_cast<long long>(offset));
        return result == expect;
      });
    }
  }
}

TEST_CASE(a64_compiler_branch1) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, size_t>());

    a64::Gp p = cc.new_gp_ptr("p");
    a64::Gp count = cc.new_gp_ptr("count");
    a64::Gp i = cc.new_gp_ptr("i");
    Label L = cc.new_label();

    func_node->set_arg(0, p);
    func_node->set_arg(1, count);

    cc.mov(i, 0);

    cc.bind(L);
    cc.strb(i.w(), a64::ptr(p, i));
    cc.add(i, i, 1);
    cc.cmp(i, count);
    cc.b_ne(L);

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void* p, size_t n);

    uint8_t array[16];
    ptr_as_func<Func>(func_ptr)(array, 16);

    expect.assign("ret={0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}");
    result.assign("ret={");
    for (size_t i = 0; i < 16; i++) {
      if (i)
        result.append(", ");
      result.append_format("%d", int(array[i]));
    }
    result.append("}");

    return result == expect;
  });
}

TEST_CASE(a64_compiler_invoke1) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  using CalledFn = uint32_t (*)(uint32_t, uint32_t);
  CalledFn called_fn = [](uint32_t x, uint32_t y) {
    return x - y;
  };

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<uint32_t, uint32_t, uint32_t>());

    a64::Gp x = cc.new_gp32("x");
    a64::Gp y = cc.new_gp32("y");
    a64::Gp r = cc.new_gp32("r");
    a64::Gp fn = cc.new_gp_ptr("fn");

    func_node->set_arg(0, x);
    func_node->set_arg(1, y);

    cc.mov(fn, (uint64_t)called_fn);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), fn, FuncSignature::build<uint32_t, uint32_t, uint32_t>(), pauth_c_func());
    invoke_node->set_arg(0, x);
    invoke_node->set_arg(1, y);
    invoke_node->set_ret(0, r);

    cc.ret(r);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t, uint32_t);

    uint32_t x = 49;
    uint32_t y = 7;

    result.assign_format("ret={%u}", ptr_as_func<Func>(func_ptr)(x, y));
    expect.assign_format("ret={%u}", x - y);

    return result == expect;
  });
}

TEST_CASE(a64_compiler_invoke2) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  using CalledFn = double (*)(double, double);
  CalledFn called_fn = [](double x, double y) {
    return x - y;
  };

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double, double>());

    a64::Vec x = cc.new_vec_d("x");
    a64::Vec y = cc.new_vec_d("y");
    a64::Vec r = cc.new_vec_d("r");
    a64::Gp fn = cc.new_gp_ptr("fn");

    func_node->set_arg(0, x);
    func_node->set_arg(1, y);
    cc.mov(fn, (uint64_t)called_fn);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), fn, FuncSignature::build<double, double, double>(), pauth_c_func());
    invoke_node->set_arg(0, x);
    invoke_node->set_arg(1, y);
    invoke_node->set_ret(0, r);

    cc.ret(r);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double, double);

    double x = 49;
    double y = 7;

    result.assign_format("ret={%f}", ptr_as_func<Func>(func_ptr)(x, y));
    expect.assign_format("ret={%f}", called_fn(x, y));

    return result == expect;
  });
}

TEST_CASE(a64_compiler_invoke3) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  using CalledFn = double (*)(double, double);
  CalledFn called_fn = [](double x, double y) {
    return x - y;
  };

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<double, double, double>());

    a64::Vec x = cc.new_vec_d("x");
    a64::Vec y = cc.new_vec_d("y");
    a64::Vec r = cc.new_vec_d("r");
    a64::Gp fn = cc.new_gp_ptr("fn");

    func_node->set_arg(0, x);
    func_node->set_arg(1, y);
    cc.mov(fn, (uint64_t)called_fn);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), fn, FuncSignature::build<double, double, double>(), pauth_c_func());
    invoke_node->set_arg(0, y);
    invoke_node->set_arg(1, x);
    invoke_node->set_ret(0, r);

    cc.ret(r);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = double (*)(double, double);

    double x = 49;
    double y = 7;

    result.assign_format("ret={%f}", ptr_as_func<Func>(func_ptr)(x, y));
    expect.assign_format("ret={%f}", called_fn(y, x));

    return result == expect;
  });
}

TEST_CASE(a64_compiler_invoke4) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* called_fn = cc.new_func(FuncSignature::build<uint32_t, uint32_t, uint32_t>());
    FuncNode* main_fn = cc.add_func(FuncSignature::build<uint32_t, uint32_t, uint32_t>());

    {
      a64::Gp v1 = cc.new_gp32("v1");
      a64::Gp v2 = cc.new_gp32("v2");

      InvokeNode* invoke_node;
      cc.invoke(Out(invoke_node), called_fn->label(), FuncSignature::build<uint32_t, uint32_t, uint32_t>());

      main_fn->set_arg(0, v1);
      main_fn->set_arg(1, v2);

      invoke_node->set_ret(0, v1);
      invoke_node->set_arg(0, v1);
      invoke_node->set_arg(1, v2);

      cc.ret(v1);
      cc.end_func();
    }

    {
      a64::Gp v1 = cc.new_gp32("v1");
      a64::Gp v2 = cc.new_gp32("v2");

      cc.add_func(called_fn);

      called_fn->set_arg(0, v1);
      called_fn->set_arg(1, v2);

      cc.add(v1, v1, v2);
      cc.ret(v1);
      cc.end_func();
    }
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(uint32_t, uint32_t);

    result.assign_format("ret={%u}", ptr_as_func<Func>(func_ptr)(10, 32));
    expect.assign_format("ret={%u}", 42);

    return result == expect;
  });
}

TEST_CASE(a64_compiler_invoke5) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  using CalledFn = uint32_t (*)();
  CalledFn called_fn = []() -> uint32_t {
    return 42;
  };

  tester.compile([&](a64::Compiler& cc) {
    static constexpr uint32_t kRegCount = 2;

    cc.add_func(FuncSignature::build<uint32_t>());

    a64::Gp fn = cc.new_gp_ptr("fn");
    a64::Gp out = cc.new_gp32("out");
    a64::Gp gpr[kRegCount];

    Label L_Done = cc.new_label();
    Label L_Failed = cc.new_label();

    cc.mov(fn, (uint64_t)called_fn);

    for (uint32_t i = 0; i < kRegCount; i++) {
      gpr[i] = cc.new_gp32("reg%u", i);
      cc.mov(gpr[i], i + 1000u);
    }

    cc.virt_reg_by_reg(gpr[kRegCount - 1])->set_home_id_hint(a64::Gp::kIdLr);

    InvokeNode* invoke_node;
    cc.invoke(Out(invoke_node), fn, FuncSignature::build<uint32_t>(), pauth_c_func());
    invoke_node->set_ret(0, out);

    for (uint32_t i = 0; i < kRegCount; i++) {
      cc.cmp(gpr[i], i + 1000u);
      cc.b_ne(L_Failed);
    }

    cc.b(L_Done);

    cc.bind(L_Failed);
    cc.mov(out, 0xFFFFFFFFu);

    cc.bind(L_Done);
    cc.ret(out);
    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = uint32_t (*)(void);

    result.assign_format("ret={%u}", ptr_as_func<Func>(func_ptr)());
    expect.assign_format("ret={%u}", called_fn());

    return result == expect;
  });
}

TEST_CASE(a64_compiler_jump_table) {
  enum Operator {
    kOperatorAdd = 0,
    kOperatorSub = 1,
    kOperatorMul = 2,
    kOperatorDiv = 3
  };

  for (bool annotated : {false, true}) {
    CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

    tester.compile([&](a64::Compiler& cc) {
      FuncNode* func_node = cc.add_func(FuncSignature::build<float, float, float, uint32_t>());

      a64::Vec a = cc.new_vec_s("a");
      a64::Vec b = cc.new_vec_s("b");
      a64::Gp op = cc.new_gp32("op");

      a64::Gp target = cc.new_gp_ptr("target");
      a64::Gp offset = cc.new_gp_ptr("offset");

      Label L_End = cc.new_label();

      Label L_Table = cc.new_label();
      Label L_Add = cc.new_label();
      Label L_Sub = cc.new_label();
      Label L_Mul = cc.new_label();
      Label L_Div = cc.new_label();

      func_node->set_arg(0, a);
      func_node->set_arg(1, b);
      func_node->set_arg(2, op);

      cc.adr(target, L_Table);
      cc.ldrsw(offset, a64::ptr(target, op, a64::sxtw(2)));
      cc.add(target, target, offset);

      // JumpAnnotation allows to annotate all possible jump targets of
      // instructions where it cannot be deduced from operands.
      if (annotated) {
        JumpAnnotation* annotation = cc.new_jump_annotation();
        annotation->add_label(L_Add);
        annotation->add_label(L_Sub);
        annotation->add_label(L_Mul);
        annotation->add_label(L_Div);
        cc.br(target, annotation);
      }
      else {
        cc.br(target);
      }

      cc.bind(L_Add);
      cc.fadd(a, a, b);
      cc.b(L_End);

      cc.bind(L_Sub);
      cc.fsub(a, a, b);
      cc.b(L_End);

      cc.bind(L_Mul);
      cc.fmul(a, a, b);
      cc.b(L_End);

      cc.bind(L_Div);
      cc.fdiv(a, a, b);

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

      float dst[4];
      float ref[4];

      dst[0] = func(33.0f, 14.0f, kOperatorAdd);
      dst[1] = func(33.0f, 14.0f, kOperatorSub);
      dst[2] = func(10.0f, 6.0f, kOperatorMul);
      dst[3] = func(80.0f, 8.0f, kOperatorDiv);

      ref[0] = 47.0f;
      ref[1] = 19.0f;
      ref[2] = 60.0f;
      ref[3] = 10.0f;

      result.assign_format("ret={%f, %f, %f, %f}", double(dst[0]), double(dst[1]), double(dst[2]), double(dst[3]));
      expect.assign_format("ret={%f, %f, %f, %f}", double(ref[0]), double(ref[1]), double(ref[2]), double(ref[3]));

      return result == expect;
    });
  }
}

TEST_CASE(a64_compiler_consecutive_out_allocation) {
  CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

  tester.compile([&](a64::Compiler& cc) {
    FuncNode* func_node = cc.add_func(FuncSignature::build<void, void*, const void*>());

    a64::Gp src = cc.new_gp_ptr();
    a64::Gp dst = cc.new_gp_ptr();

    func_node->set_arg(0, dst);
    func_node->set_arg(1, src);

    a64::Vec v[] = {cc.new_vec_q(), cc.new_vec_q(), cc.new_vec_q(), cc.new_vec_q()};
    cc.ld2(v[0].b16(), v[1].b16(), a64::ptr(src));
    cc.add(src, src, 32);
    cc.ld2(v[2].b16(), v[3].b16(), a64::ptr(src));

    for (int i = 0; i < 4; ++i) {
      cc.str(v[i], a64::ptr(dst, i * 16));
    }

    cc.end_func();
  });

  tester.execute([&](void* func_ptr, String& result, String& expect) {
    using Func = void (*)(void*, const void*);

    uint8_t src_buffer[64];
    uint8_t dst_buffer[64];

    for (uint32_t i = 0; i < 64u; i++) {
      src_buffer[i] = static_cast<uint8_t>(i);
    }

    ptr_as_func<Func>(func_ptr)(dst_buffer, src_buffer);

    result.assign("ret={");
    expect.assign("ret={");

    for (uint32_t i = 0; i < 64u; i++) {
      if (i != 0) {
        result.append(", ");
        expect.append(", ");
      }
      result.append_format("%u", dst_buffer[i]);
      // Each LD2 deinterleaves 32 bytes into an even-byte vector followed by an odd-byte vector.
      uint32_t src_index = (i / 32u) * 32u + (i % 16u) * 2u + (i / 16u) % 2u;
      expect.append_format("%u", src_buffer[src_index]);
    }

    return result == expect;
  });
}

// Regression coverage for https://github.com/asmjit/asmjit/issues/533.
TEST_CASE(a64_compiler_load_lane) {
  for (uint32_t reg_count = 1; reg_count <= 4; reg_count++) {
    for (uint32_t element_size = 1; element_size <= 8; element_size *= 2) {
      for (uint32_t lane_index : {0u, 16 / element_size - 1}) {
        CompilerTester<a64::Compiler> tester(Arch::kAArch64, test_runner);

        tester.compile([&](a64::Compiler& cc) {
          FuncSignature signature = FuncSignature::build<void, void*, const void*>();
          for (uint32_t i = 0; i <= reg_count; i++) {
            signature.add_arg(TypeId::kUInt8x16);
          }

          FuncNode* called_fn = cc.new_func(signature);
          FuncNode* main_fn = cc.add_func(FuncSignature::build<void, void*, const void*, const void*>());

          {
            a64::Gp dst = cc.new_gp_ptr("dst");
            a64::Gp initial = cc.new_gp_ptr("initial");
            a64::Gp src = cc.new_gp_ptr("src");

            main_fn->set_arg(0, dst);
            main_fn->set_arg(1, initial);
            main_fn->set_arg(2, src);

            a64::Vec v[5];
            for (uint32_t i = 0; i <= reg_count; i++) {
              v[i] = cc.new_vec_q("initial%u", i);
              cc.ldr(v[i], a64::ptr(initial, int32_t(i * 16)));
            }

            InvokeNode* invoke_node;
            cc.invoke(Out(invoke_node), called_fn->label(), signature);

            invoke_node->set_arg(0, dst);
            invoke_node->set_arg(1, src);

            for (uint32_t i = 0; i <= reg_count; i++) {
              invoke_node->set_arg(i + 2, v[i]);
            }

            cc.end_func();
          }

          {
            cc.add_func(called_fn);

            a64::Gp dst = cc.new_gp_ptr("dst");
            a64::Gp src = cc.new_gp_ptr("src");

            called_fn->set_arg(0, dst);
            called_fn->set_arg(1, src);

            // The unused first SIMD argument occupies v0. If lane destinations are treated
            // as write-only, the allocator loses their incoming values in v1 and higher.
            a64::Vec v[4];
            a64::Vec lane[4];

            for (uint32_t i = 0; i < reg_count; i++) {
              v[i] = cc.new_vec_q("v%u", i);
              called_fn->set_arg(i + 3, v[i]);

              switch (element_size) {
                case 1: lane[i] = v[i].b(lane_index); break;
                case 2: lane[i] = v[i].h(lane_index); break;
                case 4: lane[i] = v[i].s(lane_index); break;
                case 8: lane[i] = v[i].d(lane_index); break;
              }
            }

            switch (reg_count) {
              case 1: cc.ld1(lane[0], a64::ptr(src)); break;
              case 2: cc.ld2(lane[0], lane[1], a64::ptr(src)); break;
              case 3: cc.ld3(lane[0], lane[1], lane[2], a64::ptr(src)); break;
              case 4: cc.ld4(lane[0], lane[1], lane[2], lane[3], a64::ptr(src)); break;
            }

            for (uint32_t i = 0; i < reg_count; i++) {
              cc.str(v[i], a64::ptr(dst, int32_t(i * 16)));
            }

            cc.end_func();
          }
        });

        tester.execute([&](void* func_ptr, String& result, String& expect) {
          using Func = void (*)(void*, const void*, const void*);

          uint8_t initial[80];
          uint8_t src[32];
          uint8_t dst[64] = {};
          uint8_t ref[64];

          for (uint32_t i = 0; i < 80; i++) {
            initial[i] = uint8_t(17 + i * 3);
          }

          for (uint32_t i = 0; i < 32; i++) {
            src[i] = uint8_t(255 - i * 5);
          }

          memcpy(ref, initial + 16, reg_count * 16);
          for (uint32_t i = 0; i < reg_count; i++) {
            memcpy(ref + i * 16 + lane_index * element_size, src + i * element_size, element_size);
          }

          ptr_as_func<Func>(func_ptr)(dst, initial, src);

          result.assign_hex(dst, reg_count * 16);
          expect.assign_hex(ref, reg_count * 16);
          return result == expect;
        });
      }
    }
  }
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64
