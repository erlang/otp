// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if !defined(ASMJIT_NO_AARCH64) && !defined(ASMJIT_NO_BUILDER)

#include <asmjit/arm/a64_builder.h>

ASMJIT_BEGIN_SUB_NAMESPACE(Tests)

TEST_CASE(a64_builder_basics) {
  using namespace a64;
  Environment env(Arch::kAArch64);

  auto verify_builder = [](CodeHolder& code, Builder& b) {
    EXPECT_EQ(b.arch(), Arch::kAArch64);
    EXPECT_TRUE(b.is_64bit());
    EXPECT_FALSE(b.is_32bit());

    Section* section;
    code.new_section(Out(section), ".data");

    b.mov(w0, 42);
    b.ret(x30);
    b.section(section);
    b.embed_uint32(0x12345678u);

    NodeList nodes = b.node_list();
    BaseNode* node_0 = nodes.first();

    EXPECT_NOT_NULL(node_0);
    EXPECT_TRUE(node_0->is_section());
    EXPECT_EQ(node_0->as<SectionNode>()->section_id(), 0u);

    BaseNode* node_1 = node_0->next();
    EXPECT_NOT_NULL(node_1);
    EXPECT_TRUE(node_1->is_inst());
    EXPECT_EQ(node_1->as<InstNode>()->inst_id(), Inst::kIdMov);
    EXPECT_EQ(node_1->as<InstNode>()->operands().size(), 2u);
    EXPECT_EQ(node_1->as<InstNode>()->operands()[0], w0);
    EXPECT_EQ(node_1->as<InstNode>()->operands()[1], imm(42));

    BaseNode* node_2 = node_1->next();
    EXPECT_NOT_NULL(node_2);
    EXPECT_TRUE(node_2->is_inst());
    EXPECT_EQ(node_2->as<InstNode>()->inst_id(), Inst::kIdRet);
    EXPECT_EQ(node_2->as<InstNode>()->operands().size(), 1u);
    EXPECT_EQ(node_2->as<InstNode>()->operands()[0], x30);

    BaseNode* node_3 = node_2->next();
    EXPECT_NOT_NULL(node_3);
    EXPECT_TRUE(node_3->is_section());
    EXPECT_EQ(node_3->as<SectionNode>()->section_id(), section->section_id());

    BaseNode* node_4 = node_3->next();
    EXPECT_NOT_NULL(node_4);
    EXPECT_TRUE(node_4->is_embed_data());
    EXPECT_EQ(node_4->as<EmbedDataNode>()->data_size(), 4u);

    BaseNode* node_5 = node_4->next();
    EXPECT_NULL(node_5);

    EXPECT_OK(b.finalize());
  };

  CodeHolder code;
  EXPECT_OK(code.init(env));

  // Default-constructed then attached, reinitialized, then detached.
  {
    Builder b;

    EXPECT_TRUE(b.is_builder());
    EXPECT_FALSE(b.is_initialized());
    EXPECT_FALSE(b.is_compiler());
    EXPECT_FALSE(b.is_destroyed());

    EXPECT_OK(code.attach(&b));
    EXPECT_TRUE(b.is_initialized());

    verify_builder(code, b);
    EXPECT_OK(code.reinit());
    EXPECT_TRUE(b.is_initialized());

    verify_builder(code, b);
    EXPECT_OK(code.detach(&b));

    EXPECT_FALSE(b.is_initialized());
  }

  // Constructed with code (usual case) and detached at destruction time.
  {
    Builder b(&code);

    EXPECT_TRUE(b.is_builder());
    EXPECT_TRUE(b.is_initialized());
    EXPECT_FALSE(b.is_compiler());
    EXPECT_FALSE(b.is_destroyed());

    verify_builder(code, b);
    EXPECT_OK(code.reinit());
  }
}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH64 && !ASMJIT_NO_BUILDER
