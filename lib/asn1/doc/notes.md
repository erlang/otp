<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# asn1 Release Notes

This document describes the changes made to the asn1 application.

## Asn1 5.2.1

### Fixed Bugs and Malfunctions

- Fix benign warning from gcc 11 about mismatching call to free().

  Own Id: OTP-18844

## Asn1 5.2

### Fixed Bugs and Malfunctions

- The ASN.1 compiler would ignore a constraint such as `(SIZE (1..4), ...)`,
  causing incorrect behavior of the encoding and decoding function for the PER
  and UPER backends. Corrected to handle the constraint in the same way as
  `(SIZE (1..4, ...))`.

  Own Id: OTP-18729 Aux Id: PR-7575

### Improvements and New Features

- The JER backend has been internally refactored in a way that is compatible for
  applications that use the documented API. However, for a group of ASN.1
  modules that depend on each other (for example, `S1AP-PDU-Descriptions`,
  S1AP-Contents, and so on), all modules in the group must be recompiled if on
  of the group members is recompiled.

  Own Id: OTP-18748 Aux Id: ERIERL-957, PR-7637

## Asn1 5.1

### Fixed Bugs and Malfunctions

- The ASN.1 compiler used to reject correctly specified RELATIVE-OID values
  containing other RELATIVE-OID values. This is now corrected.

  Own Id: OTP-18534 Aux Id: ERIERL-737, PR-7039

### Improvements and New Features

- Minor code improvements.

  Own Id: OTP-18441

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

## Asn1 5.0.21.1

### Fixed Bugs and Malfunctions

- Fix benign warning from gcc 11 about mismatching call to free().

  Own Id: OTP-18844

## Asn1 5.0.21

### Fixed Bugs and Malfunctions

- For the `per` and `uper` ASN.1 encoding rules, encoding and decoding the
  `SEQUENCE OF` and `SET OF` constructs with 16384 items or more is now
  supported.

  Own Id: OTP-18245 Aux Id: ERIERL-859

## Asn1 5.0.20

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## Asn1 5.0.19

### Fixed Bugs and Malfunctions

- The atom `maybe` has been quoted in the source code.

  Own Id: OTP-17980

## Asn1 5.0.18.2

### Fixed Bugs and Malfunctions

- Fix benign warning from gcc 11 about mismatching call to free().

  Own Id: OTP-18844

## Asn1 5.0.18.1

### Fixed Bugs and Malfunctions

- For the `per` and `uper` ASN.1 encoding rules, encoding and decoding the
  `SEQUENCE OF` and `SET OF` constructs with 16384 items or more is now
  supported.

  Own Id: OTP-18245 Aux Id: ERIERL-859

## Asn1 5.0.18

### Fixed Bugs and Malfunctions

- Add support for the `maps` option in combination with the `jer` backend.

  Own Id: OTP-17959 Aux Id: GH-5757

## Asn1 5.0.17

### Fixed Bugs and Malfunctions

- A parameterized type with a SEQUENCE with extension ("...") made the compiler
  backend to crash. The previous fix for this in GH-4514 was not complete.

  Own Id: OTP-17522 Aux Id: GH-4902

## Asn1 5.0.16

### Fixed Bugs and Malfunctions

- Fixed a bug in the `asn1` compiler that potentially could cause it to fail to
  open a file.

  Own Id: OTP-17387 Aux Id: OTP-17123

## Asn1 5.0.15.1

### Fixed Bugs and Malfunctions

- A parameterized type with a SEQUENCE with extension ("...") made the compiler
  backend to crash. The previous fix for this in GH-4514 was not complete.

  Own Id: OTP-17522 Aux Id: GH-4902

## Asn1 5.0.15

### Fixed Bugs and Malfunctions

- A parameterized type with a SEQUENCE with extension ("...") made the compiler
  backend to crash.

  Own Id: OTP-17227 Aux Id: GH-4514

- For JER encoding rules an INTEGER value outside the declared range is now
  reported as error during decode.

  Own Id: OTP-17306 Aux Id: ERIERL-506

### Improvements and New Features

- For the JER encoding rules, the declared order of the fields in a SEQUENCE is
  now maintained in the resulting JSON object. Previously a map was used which
  caused an undefined order of the fields which was not friendly for debugging.

  Own Id: OTP-17297 Aux Id: ERIERL-607

## Asn1 5.0.14

### Improvements and New Features

- Changes in order to build on the Haiku operating system.

  Thanks to Calvin Buckley

  Own Id: OTP-16707 Aux Id: PR-2638

## Asn1 5.0.13

### Fixed Bugs and Malfunctions

- Adhere to the ASN.1 specification for hstring & bstring lexical items. That is
  they may include white space.

  Own Id: OTP-16490

### Improvements and New Features

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- Improve handling of ellipsis in a CHOICE

  Own Id: OTP-16554 Aux Id: ERL-1189

## Asn1 5.0.12

### Improvements and New Features

- Dialyzer warnings of type `no_match` are now suppressed in the generated
  files.

  Own Id: OTP-16636 Aux Id: ERIERL-145

## Asn1 5.0.11

### Improvements and New Features

- The compiler now has limited support for the JSON encoding rules (ITU-T X.697
  ASN.1 encoding rules: Specification of JavaScript Object Notation Encoding
  Rules).

  Own Id: OTP-16030

## Asn1 5.0.10

### Improvements and New Features

- Fix 'DEFAULT' with 'OCTET STRING' and 'SEQUENCE OF CHOICE' with extensions.

  Own Id: OTP-16542 Aux Id: PR-2159

## Asn1 5.0.9

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

- Corrected problems with the following value definitions:

  - value of SEQUENCE OF CHOICE with extensions
  - value of CHOICE with extensions
  - DEFAULT used with OCTET STRING

  Own Id: OTP-15697 Aux Id: PR-2159

## Asn1 5.0.8

### Fixed Bugs and Malfunctions

- Handle erroneous length during decode (BER only) without crashing.

  Own Id: OTP-15470 Aux Id: ERIERL-278

## Asn1 5.0.7

### Fixed Bugs and Malfunctions

- A bug in ASN.1 BER decoding has been fixed. When decoding a recursively
  enclosed term the length was not propagated to that term decoding, so if the
  length of the enclosed term was longer than the enclosing that error was not
  detected.

  A hard coded C stack limitation for decoding recursive ASN.1 terms has been
  introduced. This is currently set to 8 kWords giving a nesting depth of about
  1000 levels. Deeper terms can not be decoded, which should not be much of a
  real world limitation.

  Own Id: OTP-14440 Aux Id: ERIERL-220

## Asn1 5.0.6

### Improvements and New Features

- Update to use the new string api instead of the old.

  Own Id: OTP-15036

## Asn1 5.0.5.2

### Fixed Bugs and Malfunctions

- Handle erroneous length during decode (BER only) without crashing.

  Own Id: OTP-15470 Aux Id: ERIERL-278

## Asn1 5.0.5.1

### Known Bugs and Problems

- A bug in ASN.1 BER decoding has been fixed. When decoding a recursively
  enclosed term the length was not propagated to that term decoding, so if the
  length of the enclosed term was longer than the enclosing that error was not
  detected

  A hard coded C stack limitation for decoding recursive ASN.1 terms has been
  introduced. This is currently set to 8 kWords giving a nesting depth of about
  1000 levels. Deeper terms can not be decoded, which should not be much of a
  real world limitation.

  Own Id: OTP-14440 Aux Id: ERIERL-220

## Asn1 5.0.5

### Fixed Bugs and Malfunctions

- Dialyzer suppression has been added for the generated ASN.1 helper function
  to_bitstring/1 that previously created irrelevant warnings.

  Own Id: OTP-13882 Aux Id: ERIERL-144

## Asn1 5.0.4

### Fixed Bugs and Malfunctions

- There was a issue with BER encoding and the `undec_rest` option in generated
  decoders. An exception could be thrown instead of returning an error tuple.

  Own Id: OTP-14786 Aux Id: ERL-518

- The asn1ct:test functions crashed on decoders generated with options
  `no_ok_wrapper`, `undec_rest`.

  Own Id: OTP-14787 Aux Id: ERL-518

## Asn1 5.0.3

### Fixed Bugs and Malfunctions

- Compiling an ASN.1 module using the option \{n2n, EnumTypeName\} when
  EnumTypeName contains a hyphen like for example Cause-Misc caused syntax
  errors when compiling the generated Erlang code. This is now corrected.

  Own Id: OTP-14495 Aux Id: ERL-437

## Asn1 5.0.2

### Fixed Bugs and Malfunctions

- Default values now work in extension for PER, so if you give the atom
  `asn1_DEFAULT` instead of a value it will become the default value.

  Own Id: OTP-13011 Aux Id: ERIERL-60

## Asn1 5.0.1

### Fixed Bugs and Malfunctions

- Fixed compilation error of generated code caused by a missing quotation of
  function names as part of an external call for encoding.

  Own Id: OTP-14519 Aux Id: ERIERL-49

## Asn1 5.0

### Fixed Bugs and Malfunctions

- Add compile option `-compile(no_native)` in modules with `on_load` directive
  which is not yet supported by HiPE.

  Own Id: OTP-14316 Aux Id: PR-1390

### Improvements and New Features

- The `error` tuple returned from the `encode` and `decode` functions will now
  include the stack backtrace to make it easier to understand what went wrong.

  Own Id: OTP-13961

- The deprecated module `asn1rt` has been removed. The deprecated functions
  `asn1ct:encode/3` and `asn1ct:decode/3` have been removed. The undocumented
  function `asn1ct:encode/2` has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14146

- The new '`maps`' option changes the representation of the types `SEQUENCE` and
  `SET` to be maps (instead of records).

  Own Id: OTP-14219

## Asn1 4.0.4

### Fixed Bugs and Malfunctions

- Compiling multiple ASN.1 modules in the same directory with parallel make
  (make -j) should now be safe.

  Own Id: OTP-13624

## Asn1 4.0.3

### Improvements and New Features

- Internal changes

  Own Id: OTP-13551

## Asn1 4.0.2

### Fixed Bugs and Malfunctions

- When compiling to the PER format, the ASN.1 compiler would crash when
  attempting to compile an ASN.1 module with a constrained INTEGER with more
  than 65536 values and named values. (Thanks to Ingars for reporting this bug.)

  Own Id: OTP-13257

- The ASN.1 compiler will now emit Dialyzer suppressions for improper lists.
  Thus, there is no longer any need to use `--Wno_improper_lists` when analyzing
  modules generated by the ASN.1 compiler.

  Own Id: OTP-13324

## Asn1 4.0.1

### Fixed Bugs and Malfunctions

- Trying to encode an empty named BIT STRING in BER would fail with a
  `function_clause` exception. (Thanks to Svilen Ivanov for reporting this bug.)

  Own Id: OTP-13149

## Asn1 4.0

### Fixed Bugs and Malfunctions

- Many bugs have been eliminated in the ASN.1 compiler so that it can now
  successfully compile many more ASN.1 specifications. Error messages have also
  been improved.

  Own Id: OTP-12395

### Improvements and New Features

- The documentation for `asn1ct:test/1,2,3` and `asn1ct:value/2` has been
  updated with information about the limitations of the functions.

  Own Id: OTP-12765 Aux Id: seq12866, seq12867

## Asn1 3.0.4

### Fixed Bugs and Malfunctions

- The ASN.1 compiler would crash if a SEQUENCE ended with a double set of
  ellipses (`...`).

  Own Id: OTP-12546 Aux Id: seq12815

## Asn1 3.0.3

### Fixed Bugs and Malfunctions

- When decoding BER, primitives with an indefinite length will be immediately
  rejected. (Thanks to Simon Cornish for reporting this bug.)

  Own Id: OTP-12205

- BER: A bug with compliance to X.680 (200811) s31.2.7 has been fixed.
  Basically, when TagDefault is AUTOMATIC then tags are IMPLICIT unless EXPLICIT
  is given.

  Own Id: OTP-12318

- Usage of the `EXTERNAL` 1994 variant type was broken.

  Own Id: OTP-12326

## Asn1 3.0.2

### Fixed Bugs and Malfunctions

- Several problems where the ASN.1 compiler would crash when attempting to
  compile correct specifications have been corrected.

  Own Id: OTP-12125

- Robustness when decoding incorrect BER messages has been improved.

  Own Id: OTP-12145

## Asn1 3.0.1

### Fixed Bugs and Malfunctions

- The ASN.1 compiler now generates code that don't trigger Dialyzer warnings.
  Along the way, a few minor bugs were fixed.

  Own Id: OTP-11372 Aux Id: seq12397

## Asn1 3.0

### Fixed Bugs and Malfunctions

- Subtyping an extensible ENUMERATED would cause an compilation error. (Thanks
  to Morten Nygaard Åsnes for reporting this bug.)

  Own Id: OTP-11700

- When specifying the value for an OCTET STRING in a specification, the ASN.1
  standard clearly states that the value must be either a bstring or an hstring,
  but NOT a cstring. The ASN.1 compiler will now generate a compilation error if
  the value of an OCTET STRING is given as a character string.

  That is, the following example is now illegal:

  `string OCTET STRING ::= "Now illegal"`

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11727

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- By giving --enable-static-\{nifs,drivers\} to configure it is now possible to
  statically linking of nifs and drivers to the main Erlang VM binary. At the
  moment only the asn1 and crypto nifs of the Erlang/OTP nifs and drivers have
  been prepared to be statically linked. For more details see the Installation
  Guide in the System documentation.

  Own Id: OTP-11258

- Code generation for the `per` and `uper` backends has been somewhat improved.

  Own Id: OTP-11573

- The OCTET STRING and BIT STRING types now have a more natural mapping to
  Erlang types (binary and bitstring, respectively), which is more efficient and
  will avoid useless conversions between lists and binaries/bitstrings.

  This is an incompatible change. To revert to the old mapping to support
  existing applications, use the `legacy_erlang_types` option.

  Impact: There is a potential for better performance, as it is now possible to
  avoid conversions between lists and binaries both in the generated ASN.1
  encode/decode code and in the application itself.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11594

- All functions in the `asn1rt` module, as well as `asn1ct:decode/3` and
  `asn1ct:encode/3`, are now deprecated.

  Own Id: OTP-11731

- Generated .hrl files are now protected from being included more than once.

  Own Id: OTP-11804

## Asn1 2.0.4

### Fixed Bugs and Malfunctions

- The default value for a `BIT STRING` would not always be recognized, causing
  the encoding to be incorrect for the DER/PER/UPER encodings.

  Own Id: OTP-11319

- The ASN.1 application would fail to build if the `.erlang` file printed
  something to standard output.

  Own Id: OTP-11360

- An union of integer ranges in an INTEGER constraint could sometimes be
  interpreted as the intersection of the range.

  Own Id: OTP-11411 Aux Id: seq12443

- Extensible, multiple single value constraints (such as `INTEGER (1|17, ...)`)
  would be incorrectly encoded.

  Own Id: OTP-11415

- The ASN.1 compiler would fail to compile a constraint with values given for
  the extension part (such as `INTEGER (1..10, ..., 11..20)`).

  Own Id: OTP-11504

### Improvements and New Features

- The new option '`no_ok_wrapper`' generates M:encode/2 and M:decode/2 functions
  that don't wrap the return value in an \{ok,...\} tuple.

  Own Id: OTP-11314

## Asn1 2.0.3

### Fixed Bugs and Malfunctions

- Open types greater than 16383 bytes will now be correctly encoded and decoded.

  Own Id: OTP-11262 Aux Id: seq12386, OTP-11223

### Improvements and New Features

- For the PER and UPER formats, code generation especially for encoding has been
  improved.

  When encoding BIT STRINGs, values longer than the maximum size for the BIT
  STRING type would be truncated silently - they now cause an exception.

  Open types greater than 16383 bytes will now be correctly encoded and decoded.

  IMPORTANT NOTE: For ASN.1 specifications that depend on each other, such as
  the S1AP-\* specifications, it is important to recompile all specifications
  (compiling some with this version of the compiler and some with an older
  version will not work).

  Own Id: OTP-11300

## Asn1 2.0.2

### Fixed Bugs and Malfunctions

- Fix some Makefile rules that didn't support silent rules. Thanks to Anthony
  Ramine.

  Own Id: OTP-11111

- PER/UPER: A semi-constrained INTEGER with a non-zero lower bound would be
  incorrectly decoded. This bug was introduced in R16.

  PER/UPER: Given `INTEGER (10..MAX, ...)`, attempting to decode any integer
  below 10 would cause the encoder to enter an infinite loop.

  PER/UPER: For a type with an extensible SIZE constraint, sizes outside of the
  root range were incorrectly encoded.

  Given a constraint such as `(SIZE (5, ...))`, encoding a size less than 5
  would fail (PER/UPER). Similarly, for BER decoding would fail.

  PER: The encoder did not align a known multiplier string (such as IA5String)
  of length 16 bits (exactly) to an octet boundary.

  In rare circumstances, DEFAULT values for the UPER backend could be wrongly
  encoded.

  Own Id: OTP-11134

- UPER: The compiler would crash when compiling an ENUMERATED having more than
  63 extended values.

  PER/UPER: A SEQUENCE with more 64 extended values could not be decoded.

  Own Id: OTP-11153

- When decoding a SEQUENCE defined inline inside a an extension addition group,
  the record named generated by the decoding code would not match the name in
  the generated .hrl file.

  Own Id: OTP-11154 Aux Id: seq12339

### Improvements and New Features

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

## Asn1 2.0.1.2

### Fixed Bugs and Malfunctions

- When an object set is an actual parameter, the extension marker for the object
  set could get lost (which would cause the decoding of unknown values to fail).

  Own Id: OTP-10995 Aux Id: seq12290

## Asn1 2.0.1.1

### Fixed Bugs and Malfunctions

- The generated decoder for the 'per' and 'uper' backends did not correctly
  decode ENUMERATEDs with a single value.

  The generated encoder for the 'per' and 'uper' backends generated an empty
  binary for a top-level type that did not need to be encoded (such as an
  ENUMERATED with a single value). The correct result should be a binary
  containing a 0 byte.

  Own Id: OTP-10916 Aux Id: seq12270

## Asn1 2.0.1

### Fixed Bugs and Malfunctions

- Fixed broken table constraints within a SET OF or SEQUENCE OF for the BER
  backend.

  Own Id: OTP-10853 Aux Id: seq12245

## Asn1 2.0

### Fixed Bugs and Malfunctions

- Encoding SEQUENCEs with multiple extension addition groups with optional
  values could fail (depending both on the specification and whether all values
  were provided).

  Own Id: OTP-10664

### Improvements and New Features

- The options for the ASN.1 compiler has been drastically simplified. The
  backend is chosen by using `ber`, `per`, or `uper`. The options `optimize`,
  `nif`, and `driver` are no longer needed. The old options will still work, but
  will issue a warning.

  Another change is that generated `encode/2` function will always return a
  binary (some backends used to return an iolist).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10410 Aux Id: kunagi-254 \[165]

- The ASN.1 compiler generates faster decode functions for PER and UPER. Some
  minor improvements have also been made for PER/UPER encoding, and to the BER
  backend.

  Own Id: OTP-10519 Aux Id: kunagi-322 \[233]

- The ASN.1 compiler will now always include necessary run-time functions in the
  generated Erlang modules (except for `asn1rt_nif` which is still needed). If
  the option '`inline`' is used the ASN.1 compiler will generate a warning. But
  if '`{inline,OutputFile}`' is use, the ASN.1 compiler will refuse to compile
  the file. (Use a `.set.asn` file if you need to remove the output file.)

  The '`BIT STRING`' type will now be decoded as Erlang bitstrings by default.
  Use the new `legacy_bit_string` option to encode as lists of ones and zeroes.
  (The `compact_bit_string` option still works as before.)

  Open types are now always returned as binaries (when there is no information
  allowing them to be decoded).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10588 Aux Id: kunagi-341 \[252]

## Asn1 1.8.1

### Fixed Bugs and Malfunctions

- ASN.1 decoders generated with the options `-bber_bin +optimize +nif` would
  decode open types with a size larger than 511 incorrectly. That bug could
  cause decoding by `public_key` to fail. The bug was in the NIF library
  `asn1_erl_nif.so`; therefore there is no need re-compile ASN.1 specifications
  that had the problem.

  Own Id: OTP-10805 Aux Id: seq12244

- Encoding SEQUENCEs with multiple extension addition groups with optional
  values could fail (depending both on the specification and whether all values
  were provided).

  Own Id: OTP-10811 Aux Id: OTP-10664

## Asn1 1.8

### Fixed Bugs and Malfunctions

- Encoding and decoding of integer ranges can now be done with an upper bound
  larger than the previous limit of 16^10. The new upper bound in per encoding
  and decodings for constrained whole numbers is 2^2040 (close to 16^508)

  Own Id: OTP-10128

- Per encoding/decoding now works correctly for single value subtyping of an
  integer type where a subtype is a predefined value. Previously a predefined
  value could cause a non-valid range-check in the generated Erlang code for per
  encoding/decoding due to a bug in the constraint checking.

  Own Id: OTP-10139

- Fix typo error in selected decode function (Thanks to Artem Teslenko)

  Own Id: OTP-10152

- Better error indication when detecting unexpected tags during decoding of BER
  encoded data.

  Own Id: OTP-10186

- asn1rt_check: Fix transform_to_EXTERNAL1990 for binary input (Thanks to Harald
  Welte)

  Own Id: OTP-10233

### Improvements and New Features

- Add support for multiple ExtensionAdditionGroups

  Own Id: OTP-10058

- Add support for extensible enumeration types in n2n generated functions.

  Own Id: OTP-10144

## Asn1 1.7

### Improvements and New Features

- Some ASN.1 INTEGER type and SEQUENCE constructor variants previously not
  handled by the ASN.1 compiler are now correctly handled

  Own Id: OTP-9688

- An INTEGER with a value constraint where unions are used e.g. X1 ::= INTEGER
  (1..4 | 6 | 8 | 10 | 20) is not handled correctly. For PER the value is
  encoded in wrong number of bits.

  Own Id: OTP-9946

## Asn1 1.6.19

### Improvements and New Features

- The linked-in driver used for ber decode and per encode has been replaced with
  nifs. To enable the usage of nifs pass the nif option to erlc or
  asn1rt:compile when compiling. If you previously used the linked-in driver,
  you have to recompile your ASN1 modules with the current version of asn1
  application as the linked-in driver modules have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9419

- A few of the heavy calculations which are done for encoding and decoding
  operations when dealing with SEQUENCE OF and DEFAULT in runtime have been
  moved to be done in compile time instead.

  Own Id: OTP-9440

- When compiling an ASN.1 ber module with the +nif option, the module will use a
  new nif for ber encoding, increasing performance by about 5%.

  Own Id: OTP-9441

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

## Asn1 1.6.18

### Fixed Bugs and Malfunctions

- Implement or fix -Werror option

  If -Werror is enabled and there are warnings no output file is written. Also
  make sure that error/warning reporting is consistent. (Thanks to Tuncer Ayaz)

  Own Id: OTP-9536

## Asn1 1.6.17

### Fixed Bugs and Malfunctions

- Test cases which started failing when timer:tc was changed to not catch are
  corrected.

  Own Id: OTP-9286

- The bounds checking in the asn1_erl_driver when the length value of a TLV is a
  Long Definite Length is corrected. Thanks to Vance Shipley.

  Own Id: OTP-9303

## Asn1 1.6.16

### Fixed Bugs and Malfunctions

- asn1ct: Make formatting of errors and warnings consistent

  Consistently format warning and error reports. Warning and error options from
  erlc now also work in asnc1ct. (thanks to Tuncer Ayaz)

  Own Id: OTP-9062

- Shut off some dialyzer warnings

  Own Id: OTP-9063

### Improvements and New Features

- Crash in asn1ct_check, componentrelation_leadingattr fixed. (Thanks to
  Stephane Pamelard for finding the bug)

  Own Id: OTP-9092

## Asn1 1.6.15

### Fixed Bugs and Malfunctions

- The encoding of ExtensionAdditionGroup (for PER and UPER) is corrected.

  Own Id: OTP-8866 Aux Id: OTP-8797, SEQ-11557

- A race condition when several processes in parallel start to do encode/decode
  using the driver could cause an error log regarding crashing port owner
  process. This race is now eliminated.

  Own Id: OTP-8948 Aux Id: seq11733

## Asn1 1.6.14.1

### Fixed Bugs and Malfunctions

- Extension Addition Groups are now supported by the parser and in all backends.

  Own Id: OTP-8598 Aux Id: seq-11557

- Extension Addition Groups are now supported in nested types within a SEQUENCE
  and CHOICE as well (missed that in previous fix)

  Own Id: OTP-8797 Aux Id: seq-11557

### Improvements and New Features

- Bug in UNALIGNED PER regarding encoding and decoding of constrained numbers
  with a valuerange > 1024. (Thanks to Vincent de Phily)

  Own Id: OTP-8779

- Minor corrections in the User Guide.

  Own Id: OTP-8829

## Asn1 1.6.14

### Improvements and New Features

- By default, the ASN.1 compiler is now silent in the absence of warnings or
  errors. The new '`verbose`' option or the '`-v`' option for `erlc` can be
  given to show extra information (for instance, about the files that are
  generated). (Thanks to Tuncer Ayaz.)

  Own Id: OTP-8565

## Asn1 1.6.13

### Fixed Bugs and Malfunctions

- Harmless buffer overflow by one byte in asn1 and ram_file_drv.

  Own Id: OTP-8451

### Improvements and New Features

- Cross compilation improvements and other build system improvements.

  Most notable:

  - Lots of cross compilation improvements. The old cross compilation support
    was more or less non-existing as well as broken. Please, note that the cross
    compilation support should still be considered as experimental. Also note
    that old cross compilation configurations cannot be used without
    modifications. For more information on cross compiling Erlang/OTP see the
    `$ERL_TOP/INSTALL-CROSS.md` file.
  - Support for staged install using
    [DESTDIR](http://www.gnu.org/prep/standards/html_node/DESTDIR.html). The old
    broken `INSTALL_PREFIX` has also been fixed. For more information see the
    `$ERL_TOP/INSTALL.md` file.
  - Documentation of the `release` target of the top `Makefile`. For more
    information see the `$ERL_TOP/INSTALL.md` file.
  - `make install` now by default creates relative symbolic links instead of
    absolute ones. For more information see the `$ERL_TOP/INSTALL.md` file.
  - `$ERL_TOP/configure --help=recursive` now works and prints help for all
    applications with `configure` scripts.
  - Doing `make install`, or `make release` directly after `make all` no longer
    triggers miscellaneous rebuilds.
  - Existing bootstrap system is now used when doing `make install`, or
    `make release` without a preceding `make all`.
  - The `crypto` and `ssl` applications use the same runtime library path when
    dynamically linking against `libssl.so` and `libcrypto.so`. The runtime
    library search path has also been extended.
  - The `configure` scripts of `erl_interface` and `odbc` now search for thread
    libraries and thread library quirks the same way as ERTS do.
  - The `configure` script of the `odbc` application now also looks for odbc
    libraries in `lib64` and `lib/64` directories when building on a 64-bit
    system.
  - The `config.h.in` file in the `erl_interface` application is now
    automatically generated in instead of statically updated which reduces the
    risk of `configure` tests without any effect.

  (Thanks to Henrik Riomar for suggestions and testing)

  (Thanks to Winston Smith for the AVR32-Linux cross configuration and testing)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8323

- Add support for prefixing macro names generated by the compiler

  This is useful when multiple protocols that contains macros with identical
  names are included in a single module.

  Add the missing `record_name_prefix` compiler option to the documentation.

  Own Id: OTP-8453

- Cleanups suggested by tidier and modernization of types and specs.

  Own Id: OTP-8455

- Support for `EXTENSIBILITY IMPLIED` and `SET/SEQ OF NamedType` is added.

  Own Id: OTP-8463

## Asn1 1.6.12

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8256

## Asn1 1.6.11

### Improvements and New Features

- A new option `{n2n,TypeName}` can be used to enable generation of conversion
  functions from name to number and vice versa for selected ENUMERATION types.
  The option can be repeated many times in order to specify several types in the
  same file.  
  If the `TypeName` specified does not exists or is not an ENUMERATION type, the
  compilation will be terminated with an error code.  
  Below follows an example on how to use the option from the command line with
  `erlc`:  
  `erlc -bper+"{n2n,'CauseMisc'}" +"{n2n,'CausePcl'}" MyModyle.asn`

  Own Id: OTP-8136 Aux Id: seq11347

- Range checks added for BIT STRING with fixed SIZE constraint.

  Own Id: OTP-7972 Aux Id: seq11280

- Now support multiple-line comments in asn1-specs as specified in ASN1 X.680
  (07/2002), section 11.6.4

  Own Id: OTP-8043

- Now parses and adds abstract syntax for PATTERN subtype constraint. No other
  action is taken on this type of constraint.

  Own Id: OTP-8046

- The ASN1 subtype constraint `CONTAINING Type`,
  `CONTAINING Type ENCODED BY Value` and `ENCODED BY Value` now is parsed.
  Abstract syntax is added but no further action in generated code is taken.

  Own Id: OTP-8047

## Asn1 1.6.10

### Fixed Bugs and Malfunctions

- A faulty receive case that catch-ed all messages in the initialization of the
  driver has been removed, the initialization has been restructured.

  Own Id: OTP-7954 Aux Id: seq11220

### Improvements and New Features

- The anonymous part of the decode that splits the ASN1 TLV into Tag Value
  tuples has been optimized.

  Own Id: OTP-7953

## Asn1 1.6.9

### Fixed Bugs and Malfunctions

- Error that caused crash when drivers were loaded is now corrected. Parallel
  driver for asn1 now enabled.

  Own Id: OTP-7904 Aux Id: seq11220

### Improvements and New Features

- Optimized code for ENUMERATION type in encoder/decoder.

  Own Id: OTP-7909

## Asn1 1.6.8.1

### Fixed Bugs and Malfunctions

- Removed parallel-driver functionality due to failure when loading the driver.

  Own Id: OTP-7900 Aux Id: seq11220

### Improvements and New Features

- Generated code now uses guards that is not obsolete, e.g.
  [`is_integer/1`](`is_integer/1`) instead of `integer/1`.

  Own Id: OTP-7910

## Asn1 1.6.8

### Fixed Bugs and Malfunctions

- A BIT STRING with a size constraint that has a single value and an extension
  as in `BIT STRING (SIZE (16,...))` was erroneous encoded/decoded. This is now
  corrected and follows X.691 Section 15.6.

  Own Id: OTP-7876 Aux Id: seq11220

## Asn1 1.6.7

### Improvements and New Features

- Now asn1 starts multiple drivers to enable simultaneous encode/decode in
  different processes for the asn1-backends using linked-in driver.

  Own Id: OTP-7801

## Asn1 1.6.6

### Fixed Bugs and Malfunctions

- Decode of an open_type when the value was empty tagged type encoded with
  indefinite length failed. This is now corrected.

  Own Id: OTP-7759 Aux Id: seq11166

- Encode of BIT STRING with size of exact length, on compact_bit_string format
  in UNALIGNED PER failed when value had the right size, i.e. no padding needed.

  Own Id: OTP-7763 Aux Id: seq11182

## Asn1 1.6.5

### Fixed Bugs and Malfunctions

- For a BIT STRING with SIZE constraint higher than 255 compiled with
  `[per_bin,optimize, compact_bit_string]` an improper io-list was created and
  sent to the c-driver for complete encoding. This error has been resolved.

  Own Id: OTP-7734 Aux Id: seq11170

## Asn1 1.6.4

### Fixed Bugs and Malfunctions

- A a SEQUENCE OF with a type that is a CHOICE with ellipses occurred falsely a
  compile error. The error causing that is now removed.

  Own Id: OTP-7708 Aux Id: seq11136

## Asn1 1.6.3

### Fixed Bugs and Malfunctions

- constrained number with a value-range greater than 512 now has the proper
  interpretation of the values that causes shift to the next number of units
  (bits), According to limit condition `2^m < "range" =< 2^(m + 1)` then the
  number of bits are m + 1.

  Own Id: OTP-7681 Aux Id: seq11114

### Improvements and New Features

- Can now handle default values of simple types that is provided on its own
  format, i.e. not just as asn1_DEFAULT.

  Own Id: OTP-7678 Aux Id: seq11114

## Asn1 1.6.2

### Fixed Bugs and Malfunctions

- comparison of two value definitions failed due to new module name field in
  valuedef record. It is now corrected.

  Own Id: OTP-7608

## Asn1 1.6.1

### Fixed Bugs and Malfunctions

- Bug regarding propagation of parameters of parameterized type fixed.

  Own Id: OTP-7174 Aux Id: seq10864

- A bug, related to instantiation of a parameterized type with a type definition
  in the parameter-list, has been removed. The definition of the parameter type
  was in another module than the instance definition causing limited module
  info.

  Own Id: OTP-7299 Aux Id: seq10864

- Removed hard-coded name that may cause name collision.

  Own Id: OTP-7322 Aux Id: seq10864

- Object set of a class with id with properties UNIQUE OPTIONAL and the id field
  is lacking in the object is for now treated as a object without a unique
  identifier, i.e. no table is generated for this object.

  Own Id: OTP-7332 Aux Id: seq10864

- Compiler crashed when failed to handle a OID as ValueFromObject.

  Own Id: OTP-7476 Aux Id: seq10999

- A corrupted encoding may cause a loop when a buffer of at least two bytes of
  zero matches tag and length of a SET component. This behavior occurred only
  with decoder generated with `ber` or `ber_bin` options. Now a control breaks
  the loop.

  Own Id: OTP-7533

- Encode of BIT STRING longer than 255 bits with a `SIZE(integer())` constraint
  caused a crash when spec was compiled with `per_bin, optimize` options.

  Own Id: OTP-7602 Aux Id: seq11079

### Improvements and New Features

- Now supports REAL type of base 2 and 10

  Own Id: OTP-7166 Aux Id: seq10864

- By the asn1 compiler option `{record_name_prefix Name}` a prefix is chosen to
  the name of the record generated in the .hrl and used in the generated .erl
  files.

  Own Id: OTP-7204 Aux Id: seq10853

- The TypeFromObject production now covered

  Own Id: OTP-7295 Aux Id: seq10468

- Extended support for ObjectSetFromObjects. Production occurred as a part of
  the RootElementSetSpec of the ObjectSetSpec. Added also support for Exclusion
  of Element in ObjectSetSpec.

  Own Id: OTP-7306 Aux Id: seq10864

- Now implements RELATIVE-OID

  Own Id: OTP-7334 Aux Id: seq10864

## Asn1 1.6

### Fixed Bugs and Malfunctions

- Now is ordering, according to the canonical order, of components in a SET
  added. Canonical order is described in X.691 9.2 and X.680 8.6

  Own Id: OTP-7375 Aux Id: unaligned PER

- The precedence rules for extended constraints have been misinterpreted. The
  rule says for instance that if there are more than one constraint on a type
  that have extension-mark, only the last of the extension-marks would be kept.
  This affects the encoding of PER and is now corrected.

  Own Id: OTP-7400 Aux Id: OTP-7335

- A constrained number with a single-value constraint that is extensible was
  falsely encoded/decoded in aligned/unaligned PER. This is now corrected.

  Own Id: OTP-7403

### Improvements and New Features

- The ASN.1 compiler has got a new backend supporting PER UNALIGNED. Previously
  it was only support for PER ALIGNED.

  Own Id: OTP-7335

- Now the asn1-compiler handles unions and intersections of PermittedAlphabet
  constraints.

  Own Id: OTP-7374 Aux Id: unaligned PER

- With the undocumented option `no_final_padding` the whole encoded message is
  not padded to a border of a byte. Thus the returned encoded message is a
  `bitstring`.

  Own Id: OTP-7407

## Asn1 1.5.2

### Fixed Bugs and Malfunctions

- When duplicates of object fields were removed only one table access function
  for each unique identifier value was generated. This can occur when several
  object sets are merged by use of ObjectSetFromObjects.

  Own Id: OTP-7263 Aux Id: seq10864

- DER: For some complex types and components with reference to type in several
  steps the default value check function was not generated. This is now fixed.

  Own Id: OTP-7268 Aux Id: seq10684

- Now is the tag in a tagged type as parameter propagated to the instance.

  Own Id: OTP-7273 Aux Id: seq10864

### Improvements and New Features

- Added type T61String that is similar to TeletexString

  Own Id: OTP-7264 Aux Id: seq10864

## Asn1 1.5.1

### Fixed Bugs and Malfunctions

- A bug related to renaming of types has been fixed.This occurred using the
  .set.asn functionality.

  Own Id: OTP-7149 Aux Id: seq10853

- syntax error in ASN1 value now correctly shown

  Own Id: OTP-7154 Aux Id: seq10864

- Now a COMPONENTS OF construct in a parameterized type is expanded correctly

  Own Id: OTP-7155 Aux Id: seq10864

- Now the asn1-compiler also handles empty SEQUENCE DEFAULT values as `{}`.

  Own Id: OTP-7169 Aux Id: seq10864

- Now SelectionType gets the tag of the selected type.

  Own Id: OTP-7171 Aux Id: seq10864

- Correction of generated code for decode of an open type in a SEQUECNE OF/ SET
  OF

  Own Id: OTP-7193 Aux Id: seq10875

### Improvements and New Features

- Misc improvements and bug corrections regarding default values.

  Own Id: OTP-7199 Aux Id: seq10864

## Asn1 1.5

### Improvements and New Features

- Now generating records in .hrl file for instances of parameterized SEQUENCE or
  SET.

  Own Id: OTP-6835

- Optimization using bitstr in encode/decode functions. Active with
  `[per_bin, optimize]` options.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6882

## Asn1 1.4.6

### Fixed Bugs and Malfunctions

- Parsing and encoding/decoding of type constrained with SIZE with extension is
  now recovered.

  Own Id: OTP-6763

- `inline` failed because trying to use a removed module.

  Own Id: OTP-6769

- Fixed problem with a reference to a type from an object. The failure was
  caused bye change of type name when using `inline` option.

  Own Id: OTP-6770

- Handling of decode pattern for exclusive decode was false in the case when an
  un-decoded component had more than one following elements that should be
  decoded.

  Own Id: OTP-6786

### Improvements and New Features

- Now the asn1-compiler supports two root lists in SEQUENCE and SET according to
  alternative three in ComponentTypeLists (X.680 07/2002 section 24.1), i.e.
  with an extension list between two ellipses.

  Own Id: OTP-5067 Aux Id: seq8452

## Asn1 1.4.5

### Fixed Bugs and Malfunctions

- Merging modules by `inline` earlier disabled the driver (used in modules
  generated with \[optimized]/\[optimized,driver] options). Now this is
  repaired.

  Own Id: OTP-6601

- Checking phase now aware of which module an INSTANCE OF is declared in.

  Own Id: OTP-6702

### Improvements and New Features

- The compiler now handle all forms of ObjectSetSpec according to ITU-T
  recommendation X.681 (ISO/IEC 8824-2:2002).

  Own Id: OTP-6698

- Enhanced support of referencing object sets by ObjectSetFromObjects.

  Own Id: OTP-6707

- Support for parameterized object in an object set.

  Own Id: OTP-6717
