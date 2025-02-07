/* SPDX-License-Identifier: BSD-3-Clause */

#define PCRE2_CODE_UNIT_WIDTH 8

/* Do not make any symbols visble */
#define PCRE2_EXPORT

/* This limits the amount of memory that may be used while matching a pattern.
   It applies to both pcre2_match() and pcre2_dfa_match(). It does not apply
   to JIT matching. The value is in kibibytes (units of 1024 bytes). */
#define HEAP_LIMIT 20000000

/* The value of LINK_SIZE determines the number of bytes used to store links
   as offsets within the compiled regex. The default is 2, which allows for
   compiled patterns up to 65535 code units long. This covers the vast
   majority of cases. However, PCRE2 can also be compiled to use 3 or 4 bytes
   instead. This allows for longer patterns in extreme cases. */
#define LINK_SIZE 2

/* The value of MATCH_LIMIT determines the default number of times the
   pcre2_match() function can record a backtrack position during a single
   matching attempt. The value is also used to limit a loop counter in
   pcre2_dfa_match(). There is a runtime interface for setting a different
   limit. The limit exists in order to catch runaway regular expressions that
   take forever to determine that they do not match. The default is set very
   large so that it does not accidentally catch legitimate cases. */
#define MATCH_LIMIT 10000000

/* The above limit applies to all backtracks, whether or not they are nested.
   In some environments it is desirable to limit the nesting of backtracking
   (that is, the depth of tree that is searched) more strictly, in order to
   restrict the maximum amount of heap memory that is used. The value of
   MATCH_LIMIT_DEPTH provides this facility. To have any useful effect, it
   must be less than the value of MATCH_LIMIT. The default is to use the same
   value as MATCH_LIMIT. There is a runtime method for setting a different
   limit. In the case of pcre2_dfa_match(), this limit controls the depth of
   the internal nested function calls that are used for pattern recursions,
   lookarounds, and atomic groups. */
#define MATCH_LIMIT_DEPTH MATCH_LIMIT

/* This limit is parameterized just in case anybody ever wants to change it.
   Care must be taken if it is increased, because it guards against integer
   overflow caused by enormously large patterns. */
#define MAX_NAME_COUNT 10000

/* This limit is parameterized just in case anybody ever wants to change it.
   Care must be taken if it is increased, because it guards against integer
   overflow caused by enormously large patterns. */
#define MAX_NAME_SIZE 128

/* The value of MAX_VARLOOKBEHIND specifies the default maximum length, in
   characters, for a variable-length lookbehind assertion. */
#define MAX_VARLOOKBEHIND 255

/* The value of NEWLINE_DEFAULT determines the default newline character
   sequence. PCRE2 client programs can override this by selecting other values
   at run time. The valid values are 1 (CR), 2 (LF), 3 (CRLF), 4 (ANY), 5
   (ANYCRLF), and 6 (NUL). */
#define NEWLINE_DEFAULT 2

/* The value of PARENS_NEST_LIMIT specifies the maximum depth of nested
   parentheses (of any kind) in a pattern. This limits the amount of system
   stack that is used while compiling a pattern. */
#define PARENS_NEST_LIMIT 10000

/* Define to any value to enable support for Unicode and UTF encoding. This
   will work even in an EBCDIC environment, but it is incompatible with the
   EBCDIC macro. That is, PCRE2 can support *either* EBCDIC code *or*
   ASCII/Unicode, but not both at once. */
#define SUPPORT_UNICODE /**/

