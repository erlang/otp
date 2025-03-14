SPDX-License-Identifier: Apache-2.0
SPDX-FileCopyrightText: Copyright Ericsson AB 2025. All Rights Reserved.

Unicode 16.0.0 was updated from:
- https://www.unicode.org/Public/16.0.0/ucd/
- https://www.unicode.org/Public/16.0.0/ucd/auxiliary/
- https://www.unicode.org/Public/16.0.0/ucd/emoji/

When updating the Unicode version please follow these steps:

The latest vesrion of the Unicode Character Database can be found at
https://www.unicode.org/Public/UCD/latest/ucd/

1. Copy the following files to lib/stdlib/uc_spec/ replacing existing ones.
No subfolder should be created.
  - CaseFolding.txt
  - CompositionExclusions.txt
  - PropList.txt
  - SpecialCasing.txt
  - UnicodeData.txt
  - auxiliary/GraphemeBreakProperty.txt
  - emoji/emoji-data.txt
  - EastAsianWidth.txt
  - IndicSyllabicCategory.txt

2. Copy the following test files to lib/stdlib/test/unicode_util_SUITE_data/
replacing existing ones. No subfolder should be created.
  - NormalizationTest.txt
  - auxiliary/GraphemeBreakTest.txt
  - auxiliary/LineBreakTest.txt

3. Update the "spec_version()" function in the generator by replacing the Unicode
version in lib/stdlib/uc_spec/gen_unicode_mod.escript

4. Read the release notes by visiting https://www.unicode.org/versions/latest/
and assess if additional changes are necessary in the Erlang code.

5. Replace all occurrences of the previous version of Unicode with the new one in
this very same file (lib/stdlib/uc_spec/README-UPDATE.txt).
Remember to update these instructions if a new file is added or any other change
is required for future version updates.

6. Check if the test file needs to be updated:
   (cd $ERL_TOP/lib/stdlib/uc_spec; escript gen_unicode_mod.escript update_tests)
   If ../test/unicode_util_SUITE_data/unicode_table.bin is updated include it in
   the commit.

7. Run the test for the Unicode suite from the OTP repository root dir.
   $ export ERL_TOP=$PWD
   $ export PATH=$ERL_TOP/bin:$PATH
   $ ./otp_build all -a && ./otp_build tests
   $ cd release/tests/test_server
   $ erl
   erl> ts:install().
   erl> ts:run(stdlib, unicode_SUITE, [batch]).
