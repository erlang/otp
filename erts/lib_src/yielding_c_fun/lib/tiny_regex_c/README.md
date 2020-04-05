# tiny-regex-c
# A small regex implementation in C
### Description
Small and portable [Regular Expression](https://en.wikipedia.org/wiki/Regular_expression) (regex) library written in C. 

Design is inspired by Rob Pike's regex-code for the book *"Beautiful Code"* [available online here](http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html).

Supports a subset of the syntax and semantics of the Python standard library implementation (the `re`-module).

### Current status
All supported regex-operators seem to work properly according to the test-set, with the following exception:

There is a problem with ranges (e.g. `[0-9]` for a digit 0-9) combined with inverted character-cases, e.g. `[^ab]` for anything but 'a' or 'b' - like `[^-0-9]` for anything not '-' or a digit 0-9. I think the code mathces too broadly in that case. 

I think you should test the patterns you are going to use. You can easily modify the test-harness to generate tests for your intended patterns to check for compliance.

**I will gladly accept patches correcting bugs.**

### Design goals
The main design goal of this library is to be small, correct, self contained and use few resources while retaining acceptable performance and feature completeness. Clarity of the code is also highly valued.

### Notable features and omissions
- Small code and binary size: <500 SLOC, ~3kb binary for x86. Statically #define'd memory usage / allocation.
- No use of dynamic memory allocation (i.e. no calls to `malloc` / `free`).
- To avoid call-stack exhaustion, iterative searching is preferred over recursive by default (can be changed with a pre-processor flag).
- No support for capturing groups or named capture: `(^P<name>group)` etc.
- Thorough testing : [exrex](https://github.com/asciimoo/exrex) is used to randomly generate test-cases from regex patterns, which are fed into the regex code for verification. Try `make test` to generate a few thousand tests cases yourself.
- Compiled for x86 using GCC 4.7.4 and optimizing for size, the binary takes up ~2-3kb code space and allocates ~0.5kb RAM :
  ```
  > gcc -Os -c re.c
  > size re.o
      text     data     bss     dec     hex filename
      2319        0     544    2863     b2f re.o
      
  ```
  For ARM/Thumb using GCC 4.8.1 it's around 1.5kb code and less RAM :
  ```
  > arm-none-eabi-gcc -Os -mthumb -c re.c
  > size re.o
      text     data     bss     dec     hex filename
      1418        0     280    1698     6a2 re.o

  ```
  For 8-bit AVR using AVR-GCC 4.8.1 it's around 2kb code and less RAM :
  ```
  > avr-gcc -Os -c re.c
  > size re.o
      text     data     bss     dec     hex filename
      2128        0     130    2258     8d2 re.o
  ```



### API
This is the public / exported API:
```C
/* Typedef'd pointer to hide implementation details. */
typedef struct regex_t* re_t;

/* Compiles regex string pattern to a regex_t-array. */
re_t re_compile(const char* pattern);

/* Finds matches of the compiled pattern inside text. */
int  re_matchp(re_t pattern, const char* text);

/* Finds matches of pattern inside text (compiles first automatically). */
int  re_match(const char* pattern, const char* text);
```

### Supported regex-operators
The following features / regex-operators are supported by this library.

NOTE: inverted character classes are buggy - see the test harness for concrete examples.


  -  `.`         Dot, matches any character
  -  `^`         Start anchor, matches beginning of string
  -  `$`         End anchor, matches end of string
  -  `*`         Asterisk, match zero or more (greedy)
  -  `+`         Plus, match one or more (greedy)
  -  `?`         Question, match zero or one (non-greedy)
  -  `[abc]`     Character class, match if one of {'a', 'b', 'c'}
  -  `[^abc]`   Inverted class, match if NOT one of {'a', 'b', 'c'}
  **`NOTE: This feature is currently broken for some usage of character ranges!`**
  -  `[a-zA-Z]` Character ranges, the character set of the ranges { a-z | A-Z }
  -  `\s`       Whitespace, \t \f \r \n \v and spaces
  -  `\S`       Non-whitespace
  -  `\w`       Alphanumeric, [a-zA-Z0-9_]
  -  `\W`       Non-alphanumeric
  -  `\d`       Digits, [0-9]
  -  `\D`       Non-digits

### Usage
Compile a regex from ASCII-string (char-array) to a custom pattern structure using `re_compile()`.

Search a text-string for a regex and get an index into the string, using `re_match()` or `re_matchp()`.

The returned index points to the first place in the string, where the regex pattern matches.

If the regular expression doesn't match, the matching function returns an index of -1 to indicate failure.

### Examples
Example of usage:
```C
/* Standard null-terminated C-string to search: */
const char* string_to_search = "ahem.. 'hello world !' ..";

/* Compile a simple regular expression using character classes, meta-char and greedy + non-greedy quantifiers: */
re_t pattern = re_compile("[Hh]ello [Ww]orld\\s*[!]?");

/* Check if the regex matches the text: */
int match_idx = re_matchp(pattern, string_to_search);
if (match_idx != -1)
{
  printf("match at idx %d.\n", match_idx);
}
```

For more usage examples I encourage you to look at the code in the `tests`-folder.

### TODO
- Fix the implementation of inverted character classes.
- Fix implementation of branches (`|`), and see if that can lead us closer to groups as well, e.g. `(a|b)+`.
- Add `example.c` that demonstrates usage.
- Add `tests/test_perf.c` for performance and time measurements.
- Testing: Improve pattern rejection testing.

### FAQ
- *Q: What differentiates this library from other C regex implementations?*

  A: Well, the small size for one. <500 lines of C-code compiling to 2-3kb ROM, using very little RAM.

### License
All material in this repository is in the public domain.



 
