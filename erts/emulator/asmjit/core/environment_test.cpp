// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/core/environment.h>

ASMJIT_BEGIN_NAMESPACE

TEST_CASE(core_environment_arch) {
  Environment env;

  env.init(Arch::kX86);
  EXPECT_EQ(env.arch(), Arch::kX86);
  EXPECT_TRUE(env.is_32bit());
  EXPECT_FALSE(env.is_64bit());

  EXPECT_TRUE(env.is_arch_x86());
  EXPECT_FALSE(env.is_arch_x64());
  EXPECT_FALSE(env.is_arch_arm());
  EXPECT_FALSE(env.is_arch_aarch64());

  EXPECT_TRUE(env.is_family_x86());
  EXPECT_FALSE(env.is_family_arm());
  EXPECT_FALSE(env.is_family_riscv());

  env.init(Arch::kX64);
  EXPECT_EQ(env.arch(), Arch::kX64);
  EXPECT_FALSE(env.is_32bit());
  EXPECT_TRUE(env.is_64bit());

  EXPECT_FALSE(env.is_arch_x86());
  EXPECT_TRUE(env.is_arch_x64());
  EXPECT_FALSE(env.is_arch_arm());
  EXPECT_FALSE(env.is_arch_aarch64());

  EXPECT_TRUE(env.is_family_x86());
  EXPECT_FALSE(env.is_family_arm());
  EXPECT_FALSE(env.is_family_riscv());

  env.init(Arch::kARM);
  EXPECT_EQ(env.arch(), Arch::kARM);
  EXPECT_TRUE(env.is_32bit());
  EXPECT_FALSE(env.is_64bit());

  EXPECT_FALSE(env.is_arch_x86());
  EXPECT_FALSE(env.is_arch_x64());
  EXPECT_TRUE(env.is_arch_arm());
  EXPECT_FALSE(env.is_arch_aarch64());

  EXPECT_FALSE(env.is_family_x86());
  EXPECT_TRUE(env.is_family_arm());
  EXPECT_FALSE(env.is_family_riscv());

  env.init(Arch::kAArch64);
  EXPECT_EQ(env.arch(), Arch::kAArch64);
  EXPECT_FALSE(env.is_32bit());
  EXPECT_TRUE(env.is_64bit());

  EXPECT_FALSE(env.is_arch_x86());
  EXPECT_FALSE(env.is_arch_x64());
  EXPECT_FALSE(env.is_arch_arm());
  EXPECT_TRUE(env.is_arch_aarch64());

  EXPECT_FALSE(env.is_family_x86());
  EXPECT_TRUE(env.is_family_arm());
  EXPECT_FALSE(env.is_family_riscv());
}

TEST_CASE(core_environment_platform) {
  Environment env;

  env.init(Arch::kX86, SubArch::kUnknown, Vendor::kUnknown, Platform::kWindows, PlatformABI::kMSVC);
  EXPECT_EQ(env.arch(), Arch::kX86);
  EXPECT_EQ(env.platform(), Platform::kWindows);
  EXPECT_TRUE(env.is_platform_windows());
  EXPECT_FALSE(env.is_platform_linux());
  EXPECT_FALSE(env.is_platform_bsd());
  EXPECT_FALSE(env.is_platform_apple());
  EXPECT_TRUE(env.is_msvc_abi());
  EXPECT_FALSE(env.is_darwin_abi());

  env.init(Arch::kX64, SubArch::kUnknown, Vendor::kUnknown, Platform::kLinux, PlatformABI::kGNU);
  EXPECT_EQ(env.arch(), Arch::kX64);
  EXPECT_EQ(env.platform(), Platform::kLinux);
  EXPECT_FALSE(env.is_platform_windows());
  EXPECT_TRUE(env.is_platform_linux());
  EXPECT_FALSE(env.is_platform_bsd());
  EXPECT_FALSE(env.is_platform_apple());
  EXPECT_FALSE(env.is_msvc_abi());
  EXPECT_FALSE(env.is_darwin_abi());

  env.init(Arch::kAArch64, SubArch::kUnknown, Vendor::kUnknown, Platform::kOSX, PlatformABI::kDarwin);
  EXPECT_EQ(env.arch(), Arch::kAArch64);
  EXPECT_EQ(env.platform(), Platform::kOSX);
  EXPECT_FALSE(env.is_platform_windows());
  EXPECT_FALSE(env.is_platform_linux());
  EXPECT_FALSE(env.is_platform_bsd());
  EXPECT_TRUE(env.is_platform_apple());
  EXPECT_FALSE(env.is_msvc_abi());
  EXPECT_TRUE(env.is_darwin_abi());
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
