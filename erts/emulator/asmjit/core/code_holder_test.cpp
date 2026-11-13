// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/core/code_holder.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(core_code_holder_constructors) {
  Environment env;
  env.init(Arch::kX86);

  CodeHolder code;
  EXPECT_FALSE(code.is_initialized());

  EXPECT_OK(code.init(env));
  EXPECT_TRUE(code.is_initialized());
  EXPECT_EQ(code.arch(), Arch::kX86);
}

TEST_CASE(core_code_holder_labels) {
  Environment env;
  env.init(Arch::kX86);

  CodeHolder code;
  EXPECT_OK(code.init(env));

  uint32_t dummy_id;

  // Global labels must be unique - there cannot be two global labels with the same name.
  uint32_t label_global_a;
  uint32_t label_global_b;

  EXPECT_OK(code.new_named_label_id(Out(label_global_a), "label_global_a", SIZE_MAX, LabelType::kGlobal));
  EXPECT_OK(code.new_named_label_id(Out(label_global_b), "label_global_b", SIZE_MAX, LabelType::kGlobal));

  // Global labels must have a name - not providing one is an error, but error is also trying to create a duplicate.
  EXPECT_EQ(code.new_named_label_id(Out(dummy_id), "", SIZE_MAX, LabelType::kGlobal), Error::kInvalidLabelName);
  EXPECT_EQ(code.new_named_label_id(Out(dummy_id), "label_global_a", SIZE_MAX, LabelType::kGlobal), Error::kLabelAlreadyDefined);
  EXPECT_EQ(code.new_named_label_id(Out(dummy_id), "label_global_b", SIZE_MAX, LabelType::kGlobal), Error::kLabelAlreadyDefined);

  EXPECT_TRUE(code.is_label_valid(label_global_a));
  EXPECT_TRUE(code.is_label_valid(label_global_b));

  EXPECT_FALSE(code.is_label_bound(label_global_a));
  EXPECT_FALSE(code.is_label_bound(label_global_b));

  EXPECT_EQ(code.label_entry_of(label_global_a).label_type(), LabelType::kGlobal);
  EXPECT_EQ(code.label_entry_of(label_global_b).label_type(), LabelType::kGlobal);

  EXPECT_EQ(code.label_entry_of(label_global_a).name_size(), 14u);
  EXPECT_EQ(code.label_entry_of(label_global_b).name_size(), 14u);

  EXPECT_EQ(strcmp(code.label_entry_of(label_global_a).name(), "label_global_a"), 0);
  EXPECT_EQ(strcmp(code.label_entry_of(label_global_b).name(), "label_global_b"), 0);

  EXPECT_EQ(code.label_id_by_name("label_global_a"), label_global_a);
  EXPECT_EQ(code.label_id_by_name("label_global_b"), label_global_b);

  // Local labels must have a parent - and a parent must be used during lookup too.
  uint32_t label_local_a;
  uint32_t label_local_b;

  EXPECT_OK(code.new_named_label_id(Out(label_local_a), "label_local_a", SIZE_MAX, LabelType::kLocal, label_global_a));
  EXPECT_OK(code.new_named_label_id(Out(label_local_b), "label_local_b", SIZE_MAX, LabelType::kLocal, label_global_b));

  EXPECT_EQ(code.new_named_label_id(Out(dummy_id), "label_local_a", SIZE_MAX, LabelType::kLocal, label_global_a), Error::kLabelAlreadyDefined);
  EXPECT_EQ(code.new_named_label_id(Out(dummy_id), "label_local_b", SIZE_MAX, LabelType::kLocal, label_global_b), Error::kLabelAlreadyDefined);

  EXPECT_TRUE(code.is_label_valid(label_local_a));
  EXPECT_TRUE(code.is_label_valid(label_local_b));

  EXPECT_FALSE(code.is_label_bound(label_local_a));
  EXPECT_FALSE(code.is_label_bound(label_local_b));

  EXPECT_EQ(code.label_entry_of(label_local_a).label_type(), LabelType::kLocal);
  EXPECT_EQ(code.label_entry_of(label_local_b).label_type(), LabelType::kLocal);

  EXPECT_EQ(code.label_entry_of(label_local_a).name_size(), 13u);
  EXPECT_EQ(code.label_entry_of(label_local_b).name_size(), 13u);

  EXPECT_EQ(strcmp(code.label_entry_of(label_local_a).name(), "label_local_a"), 0);
  EXPECT_EQ(strcmp(code.label_entry_of(label_local_b).name(), "label_local_b"), 0);

  EXPECT_EQ(code.label_id_by_name("label_local_a", SIZE_MAX, label_global_a), label_local_a);
  EXPECT_EQ(code.label_id_by_name("label_local_b", SIZE_MAX, label_global_b), label_local_b);

  EXPECT_EQ(code.label_id_by_name("label_local_a"), Globals::kInvalidId);
  EXPECT_EQ(code.label_id_by_name("label_local_b"), Globals::kInvalidId);

  // Anonymous labels are nameless by default.
  uint32_t label_anon_a;
  uint32_t label_anon_b;

  // Anonymous labels are simply created by `code.new_label_id()`
  EXPECT_OK(code.new_label_id(Out(label_anon_a)));
  // Anonymous labels can have no-name (this is basically like calling `code.new_label_id()`).
  EXPECT_OK(code.new_named_label_id(Out(label_anon_b), "", SIZE_MAX, LabelType::kAnonymous));

  EXPECT_TRUE(code.is_label_valid(label_anon_a));
  EXPECT_TRUE(code.is_label_valid(label_anon_b));

  EXPECT_FALSE(code.is_label_bound(label_anon_a));
  EXPECT_FALSE(code.is_label_bound(label_anon_b));

  EXPECT_EQ(code.label_entry_of(label_anon_a).label_type(), LabelType::kAnonymous);
  EXPECT_EQ(code.label_entry_of(label_anon_b).label_type(), LabelType::kAnonymous);

  EXPECT_FALSE(code.label_entry_of(label_anon_a).has_name());
  EXPECT_FALSE(code.label_entry_of(label_anon_b).has_name());

  EXPECT_EQ(code.label_entry_of(label_anon_a).name_size(), 0u);
  EXPECT_EQ(code.label_entry_of(label_anon_b).name_size(), 0u);

  // Anonymous labels can have names - but these names cannot be used to query them.
  uint32_t label_named_anon_a;
  uint32_t label_named_anon_b;

  EXPECT_OK(code.new_named_label_id(Out(label_named_anon_a), "label_named_anon", SIZE_MAX, LabelType::kAnonymous));
  EXPECT_OK(code.new_named_label_id(Out(label_named_anon_b), "label_named_anon", SIZE_MAX, LabelType::kAnonymous));

  EXPECT_TRUE(code.is_label_valid(label_named_anon_a));
  EXPECT_TRUE(code.is_label_valid(label_named_anon_b));

  EXPECT_FALSE(code.is_label_bound(label_named_anon_a));
  EXPECT_FALSE(code.is_label_bound(label_named_anon_b));

  EXPECT_EQ(code.label_entry_of(label_named_anon_a).label_type(), LabelType::kAnonymous);
  EXPECT_EQ(code.label_entry_of(label_named_anon_b).label_type(), LabelType::kAnonymous);

  EXPECT_TRUE(code.label_entry_of(label_named_anon_a).has_name());
  EXPECT_TRUE(code.label_entry_of(label_named_anon_b).has_name());

  EXPECT_EQ(code.label_entry_of(label_named_anon_a).name_size(), 16u);
  EXPECT_EQ(code.label_entry_of(label_named_anon_b).name_size(), 16u);

  EXPECT_EQ(strcmp(code.label_entry_of(label_named_anon_a).name(), "label_named_anon"), 0);
  EXPECT_EQ(strcmp(code.label_entry_of(label_named_anon_b).name(), "label_named_anon"), 0);

  EXPECT_EQ(code.label_id_by_name("label_named_anon"), Globals::kInvalidId);

  // Invalid / unknown names should not return a valid label-id.
  EXPECT_EQ(code.label_id_by_name(""), Globals::kInvalidId);
  EXPECT_EQ(code.label_id_by_name("aaa"), Globals::kInvalidId);
  EXPECT_EQ(code.label_id_by_name("unknown_label"), Globals::kInvalidId);
}

TEST_CASE(core_code_holder_sections) {
  Environment env;
  env.init(Arch::kX86);

  CodeHolder code;
  EXPECT_OK(code.init(env));

  // ".text" section properties.
  Section* section_text = code.text_section();
  EXPECT_TRUE(section_text->has_offset());
  EXPECT_TRUE(section_text->has_flag(SectionFlags::kExecutable));
  EXPECT_TRUE(section_text->has_flag(SectionFlags::kReadOnly));
  EXPECT_FALSE(section_text->has_flag(SectionFlags::kZeroInitialized));
  EXPECT_FALSE(section_text->has_flag(SectionFlags::kComment));
  EXPECT_TRUE(section_text->has_flag(SectionFlags::kBuiltIn));
  EXPECT_FALSE(section_text->has_flag(SectionFlags::kImplicit));

  // Section ordering.
  Section* section_a;
  EXPECT_OK(code.new_section(Out(section_a), "high-priority", SIZE_MAX, SectionFlags::kNone, 1, -1));
  EXPECT_EQ(code.sections()[0], section_text);
  EXPECT_EQ(code.sections()[1], section_a);
  EXPECT_EQ(code.sections_by_order()[0], section_text);
  EXPECT_EQ(code.sections_by_order()[1], section_a);
  EXPECT_FALSE(section_a->has_offset());

  Section* section_b;
  EXPECT_OK(code.new_section(Out(section_b), "higher-priority", SIZE_MAX, SectionFlags::kNone, 1, -2));
  EXPECT_EQ(code.sections()[0], section_text);
  EXPECT_EQ(code.sections()[1], section_a);
  EXPECT_EQ(code.sections()[2], section_b);
  EXPECT_EQ(code.sections_by_order()[0], section_text);
  EXPECT_EQ(code.sections_by_order()[1], section_b);
  EXPECT_EQ(code.sections_by_order()[2], section_a);
  EXPECT_FALSE(section_b->has_offset());

  Section* section_c;
  EXPECT_OK(code.new_section(Out(section_c), "low-priority", SIZE_MAX, SectionFlags::kNone, 1, 2));
  EXPECT_EQ(code.sections()[0], section_text);
  EXPECT_EQ(code.sections()[1], section_a);
  EXPECT_EQ(code.sections()[2], section_b);
  EXPECT_EQ(code.sections()[3], section_c);
  EXPECT_EQ(code.sections_by_order()[0], section_text);
  EXPECT_EQ(code.sections_by_order()[1], section_b);
  EXPECT_EQ(code.sections_by_order()[2], section_a);
  EXPECT_EQ(code.sections_by_order()[3], section_c);
  EXPECT_FALSE(section_c->has_offset());
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
