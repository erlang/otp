<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Welcome to EDoc

EDoc is the Erlang program documentation generator. Inspired by the Javadoc(TM)
tool for the Java(TM) programming language, EDoc is adapted to the conventions
of the Erlang world, and has several features not found in Javadoc.

EDoc can generate static HTML documentation accessible with any web browser or
[EEP-48](https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html) doc
chunks with [erlang+html(3)](`e:edoc:doc_storage.md`) to provide
documentation for other tools like `m:shell_docs`.

## Contents

- [Introduction](chapter.md#introduction)
- [Running EDoc](chapter.md#Running_EDoc)
- [The overview page](chapter.md#The_overview_page)
- [Generic tags](chapter.md#Generic_tags)
- [Overview tags](chapter.md#Overview_tags)
- [Module tags](chapter.md#Module_tags)
- [Function tags](chapter.md#Function_tags)
- [References](chapter.md#references)
- [Notes on XHTML](chapter.md#Notes_on_XHTML)
- [Wiki notation](chapter.md#Wiki_notation)
- [Macro expansion](chapter.md#Macro_expansion)
- [Type specifications](chapter.md#Type_specifications)
- [Doc chunks](chapter.md#Doc_chunks)
- [Acknowledgements](chapter.md#acknowledgements)

## Introduction

EDoc lets you write the documentation of an Erlang program as comments in the
source code itself, using _tags_ on the form "`@Name ...`". A source file does
not have to contain tags for EDoc to generate its documentation, but without
tags the result will only contain the basic available information that can be
extracted from the module.

A tag must be the first thing on a comment line, except for leading '`%`'
characters and whitespace. The comment must be between program declarations, and
not on the same line as any program text. All the following text - including
consecutive comment lines - up until the end of the comment or the next tagged
line, is taken as the _content_ of the tag.

Tags are associated with the nearest following program construct "of
significance" (the module name declaration and function definitions). Other
constructs are ignored; e.g., in:

```text
   %% @doc Prints the value X.

   -record(foo, {x, y, z}).

   print(X) -> ...
```

the `@doc` tag is associated with the function `print/1`.

Note that in a comment such as:

```text
   % % @doc ...
```

the tag is ignored, because only the first '`%`' character is considered
"leading". This allows tags to be "commented out".

Some tags, such as `@type`, do not need to be associated with any program
construct. These may be placed at the end of the file, in the "footer".

[](){: #Running_EDoc }

## Running EDoc

The following are the main functions for running EDoc:

- `edoc:application/2`: Creates documentation for a typical Erlang application.
- `edoc:files/2`: Creates documentation for a specified set of source files.
- `edoc:run/2`: General interface function; the common back-end for the above
  functions. Options are documented here.

Note that the function `edoc:file/2` belongs to the old, deprecated interface
(from EDoc version 0.1), and should not be used.

It's also possible to run EDoc directly from the command line, using the
[bin/edoc EScript](edoc_cmd.md). The script acts as a command line entry point
to both `edoc:application/2` and `edoc:files/2` functions. It also allows to
generate just the EEP-48 doc chunks (using the `-chunks` flag) instead of the
complete HTML documentation.

[](){: #The_overview_page }

## The overview page

When documentation is generated for an entire application, an overview page, or
"front page", is generated. (The page you are now reading is an overview page.)
This should contain the high-level description or user manual for the
application, leaving the finer details to the documentation for individual
modules. By default, the overview page is generated from the file
`overview.edoc` in the target directory (typically, this is the `doc`
subdirectory of the application directory); see `m:edoc_doclet` for details.

The format of the overview file is the same as for EDoc documentation comments
(see [Introduction](chapter.md#introduction)), except that the lines do not have
leading '`%`' characters. Furthermore, all lines before the first tag line are
ignored, and can be used as a comment. All tags in the overview file, such as
`@doc`, `@version`, etc., refer to the application as a whole; see
[Overview tags](chapter.md#Overview_tags) for details.

Here is an example of the contents of an overview file:

```text
   ** this is the overview.doc file for the application 'frob' **

   @author R. J. Hacker <rjh@acme.com>
   @copyright 2007 R. J. Hacker
   @version 1.0.0
   @title Welcome to the `frob' application!
   @doc `frob' is a highly advanced frobnicator with low latency,
   ...
```

[](){: #Generic_tags }

## Generic tags

The following tags can be used anywhere within a module:

- **`@clear`** - [](){: #gtag-clear } This tag causes all tags above it (up to
  the previous program construct), to be discarded, including the `@clear` tag
  itself. The text following the tag is also ignored. _This is typically only
  useful in code containing conditional compilation, when preprocessing is
  turned on._ (Preprocessing is turned off by default.) E.g., in

  ```text
     -ifdef(DEBUG).
     %% @doc ...
     foo(...) -> ...
     -endif.
     %% @clear

     %% @doc ...
     bar(...) -> ...
  ```

  the `@clear` tag makes sure that EDoc does not see two `@doc` tags before the
  function `bar`, even if the code for function `foo` is removed by
  preprocessing. (There is no way for EDoc to see what the first `@doc` tag
  "really" belongs to, since preprocessing strips away all such information.)

- **`@docfile`** - [](){: #gtag-docfile } Reads a plain documentation file (on
  the same format as an overview file - see
  [The overview page](chapter.md#The_overview_page) for details), and uses the
  tags in that file as if they had been written in place of the `@docfile` tag.
  The content is the name of the file to be read; leading and trailing
  whitespace is ignored. See also [@headerfile](chapter.md#gtag-headerfile).

- **`@end`** - [](){: #gtag-end } The text following this tag is always ignored.
  Use this to mark the end of the previous tag, when necessary, as e.g. in:

  ```text
     %% ----------------------------------
     %% ...
     %% @doc ...
     %% ...
     %% @end
     %% ----------------------------------
  ```

  to avoid including the last "ruler" line in the `@doc` tag.

  _Note: using some other "dummy" @-tag for the same purpose might work in a
  particular implementation of EDoc, but is not guaranteed to. Always use @end
  to ensure future compatibility._

- **`@headerfile`** - [](){: #gtag-headerfile } Similar to the
  [@docfile tag](chapter.md#gtag-docfile), but reads a file containing Erlang
  source code - generally this should be a header file (with the extension
  `.hrl`). If the file turns out to contain one or more function definitions or
  a module declaration, all tags that occur above the last such definition or
  module declaration are ignored, and EDoc will print a warning. This tag allows
  you to write documentation in a header file and insert it at a specific place
  in the documentation, even if the header file is used (i.e., included) by
  several modules. The `includes` option can be used to specify a search path
  (see `edoc:read_source/2`).

- **`@todo` (or `@TODO`)** - [](){: #gtag-todo } Attaches a To-Do note to a
  function, module or overview-page. The content can be any XHTML text
  describing the issue, e.g.:

  ```text
     %% @TODO Finish writing the documentation.
  ```

  or

  ```text
     %% @todo Implement <a href="http://www.ietf.org/rfc/rfc2549.txt">RFC 2549</a>.
  ```

  These tags can also be written as "`TODO:`", e.g.:

  ```text
     %% TODO: call your mother
  ```

  see [Wiki notation](chapter.md#Wiki_notation) for more information. To-Do
  notes are normally not shown unless the `todo` option is turned on (see
  `edoc:get_doc/2`).

- **`@type`** - [](){: #gtag-type } Documents an abstract data type or type
  alias. The content consists of a type declaration or definition, optionally
  followed by a period ('`.`') separator and XHTML text describing the type
  (i.e., its purpose, use, etc.). There must be at least one whitespace
  character between the '`.`' and the text. See
  [Type specifications](chapter.md#Type_specifications) for syntax and examples.
  All data type descriptions are placed in a separate section of the
  documentation, regardless of where the tags occur.

  Instead of specifying the complete type alias in an EDoc documentation
  comment, type definitions from the actual Erlang code can be re-used for
  documentation. See [Type specifications](chapter.md#Type_specifications) for
  examples.

[](){: #Overview_tags }

## Overview tags

The following tags can be used in an overview file.

- **`@author`** - [](){: #otag-author } See the
  [@author module tag](chapter.md#mtag-author) for details.

- **`@copyright`** - [](){: #otag-copyright } See the
  [@copyright module tag](chapter.md#mtag-copyright) for details.

- **`@doc`** - [](){: #otag-doc } See the [@doc module tag](chapter.md#mtag-doc)
  for details.

- **`@reference`** - [](){: #otag-reference } See the
  [@reference module tag](chapter.md#mtag-reference) for details.

- **`@see`** - [](){: #otag-see } See the [@see module tag](chapter.md#mtag-see)
  for details.

- **`@since`** - [](){: #otag-since } See the
  [@since module tag](chapter.md#mtag-since) for details.

- **`@title`** - [](){: #otag-title } Specifies a title for the overview page.
  This tag can _only_ be used in an overview file. The content can be arbitrary
  text.

- **`@version`** - [](){: #otag-version } See the
  [@version module tag](chapter.md#mtag-version) for details.

[](){: #Module_tags }

## Module tags

The following tags can be used before a module declaration:

- **`@author`** - [](){: #mtag-author } Specifies the name of an author, along
  with contact information. An e-mail address can be given within `<...>`
  delimiters, and a URI within `[...]` delimiters. Both e-mail and URI are
  optional, and any surrounding whitespace is stripped from all strings.

  The name is the first nonempty string that is not within `<...>` or `[...]`,
  and does not contain only whitespace. (In other words, the name can come
  before, between, or after the e-mail and URI, but cannot be split up; any
  sections after the first are ignored.) If an e-mail address is given, but no
  name, the e-mail string will be used also for the name. If no `<...>` section
  is present, but the name string contains an '`@`' character, it is assumed to
  be an e-mail address. Not both name and e-mail may be left out.

  Examples:

  ```text
     %% @author Richard Carlsson
  ```

  ```text
     %% @author Richard Carlsson <carlsson.richard@gmail.com>
     %%   [http://example.net/richardc/]
  ```

  ```text
     %% @author <carlsson.richard@gmail.com>
  ```

  ```text
     %% @author carlsson.richard@gmail.com [http://example.net/richardc/]
  ```

- **`@copyright`** - [](){: #mtag-copyright } Specifies the module copyrights.
  The content can be arbitrary text; for example:

  ```text
     %% @copyright 2001-2003 Richard Carlsson
  ```

- **`@deprecated`** - [](){: #mtag-deprecated } Mark the module as deprecated,
  indicating that it should no longer be used. The content must be well-formed
  XHTML, and should preferably include a `{@link}` reference to a replacement;
  as in:

  ```text
     %% @deprecated Please use the module {@link foo} instead.
  ```

- **`@doc`** - [](){: #mtag-doc } Describes the module, using well-formed XHTML
  text. The first sentence is used as a summary (see the
  [@doc function tag](chapter.md#ftag-doc) for details). For example.:

  ```text
     %% @doc This is a <em>very</em> useful module. It is ...
  ```

- **`@hidden`** - [](){: #mtag-hidden } Marks the module so that it will not
  appear in the documentation (even if "private" documentation is generated).
  Useful for sample code, test modules, etc. The content can be used as a
  comment; it is ignored by EDoc.

- **`@private`** - [](){: #mtag-private } Marks the module as private (i.e., not
  part of the public interface), so that it will not appear in the normal
  documentation. (If "private" documentation is generated, the module will be
  included.) The content can be used as a comment; it is ignored by EDoc.

- **`@reference`** - [](){: #mtag-reference } Specifies a reference to some
  arbitrary external resource, such as an article, book, or web site. The
  content must be well-formed XHTML text. Examples:

  ```text
     %% @reference Pratchett, T., <em>Interesting Times</em>,
     %% Victor Gollancz Ltd, 1994.
  ```

  ```text
     %% @reference See <a href="www.google.com">Google</a> for
     %% more information.
  ```

- **`@see`** - [](){: #mtag-see } See the
  [@see function tag](chapter.md#ftag-see) for details.

- **`@since`** - [](){: #mtag-since } Specifies when the module was introduced,
  with respect to the application, release or distribution it is part of. The
  content can be arbitrary text.

- **`@version`** - [](){: #mtag-version } Specifies the module version. The
  content can be arbitrary text.

[](){: #Function_tags }

## Function tags

The following tags can be used before a function definition:

- **`@deprecated`** - [](){: #ftag-deprecated } See the
  [@deprecated module tag](chapter.md#mtag-deprecated) for details.

- **`@doc`** - [](){: #ftag-doc } XHTML text describing the function. The first
  sentence of the text is used as a quick summary; this ends at the first period
  character ('`.`') or exclamation mark ('`!`') that is followed by a whitespace
  character, a line break, or the end of the tag text, and is not within XML
  markup. (As an exception, the first sentence may be within an initial
  paragraph element)

- **`@equiv`** - [](){: #ftag-equiv } Specify equivalence to another function
  call/expression. The content must be a proper Erlang expression. If the
  expression is a function call, a cross-reference to the called function is
  created automatically. Typically, this tag is used instead of `@doc`.

- **`@hidden`** - [](){: #ftag-hidden } Marks the function so that it will not
  appear in the documentation (even if "private" documentation is generated).
  Useful for debug/test functions, etc. The content can be used as a comment; it
  is ignored by EDoc.

- **`@param`** - [](){: #ftag-param } Provide more information on a single
  parameter of the enclosing function. The content consists of a parameter name,
  followed by one or more whitespace characters, and XHTML text.

- **`@private`** - [](){: #ftag-private } Marks the function as private (i.e.,
  not part of the public interface), so that it will not appear in the normal
  documentation. (If "private" documentation is generated, the function will be
  included.) Only useful for exported functions, e.g. entry points for `spawn`.
  (Non-exported functions are always "private".) The content can be used as a
  comment; it is ignored by EDoc.

- **`@returns`** - [](){: #ftag-returns } Specify additional information about
  the value returned by the function. Content consists of XHTML text.

- **`@see`** - [](){: #ftag-see } Make a reference to a module, function,
  datatype, or application. (See [References](chapter.md#references).) The
  content consists of a reference, optionally followed by a period ('`.`'), one
  or more whitespace characters, and XHTML text to be used for the label; for
  example "`@see edoc`" or "`@see edoc. <b>EDoc</b>`". If no label text is
  specified, the reference itself is used as the label.

- **`@since`** - [](){: #ftag-since } Specifies in what version of the module
  the function was introduced; cf. the
  [@version module tag](chapter.md#mtag-version). The content can be arbitrary
  text.

- **`@spec`** - [](){: #ftag-spec } Used to specify the function type; see
  [Type specifications](chapter.md#Type_specifications) for syntax details. If
  the function name is included in the specification, it must match the name in
  the actual code. When parameter names are not given in the specification,
  suitable names will be taken from the source code if possible, and otherwise
  synthesized.

  Instead of specifying the complete function type in an EDoc documentation
  comment, specifications from the actual Erlang code can be re-used for
  documentation. See [Type specifications](chapter.md#Type_specifications) for
  examples.

- **`@throws`** - [](){: #ftag-throws } Specifies which types of terms may be
  thrown by the function, if its execution terminates abruptly due to a call to
  `erlang:throw(Term)`. The content is a type expression (see
  [Type specifications](chapter.md#Type_specifications)), and can be a union
  type.

  Note that exceptions of type `exit` (as caused by calls to
  `erlang:exit(Term)`) and `error` (run-time errors such as `badarg` or
  `badarith`) are not viewed as part of the normal interface of the function,
  and cannot be documented with the `@throws` tag.

- **`@type`** - [](){: #ftag-type } See the
  [@type generic tag](chapter.md#gtag-type) for details. Placing a `@type` tag
  by a function definition may be convenient, but does not affect where the
  description is placed in the generated documentation.

## References

In several contexts (`@see` tags, `@link` macros, etc.), EDoc lets you refer to
the generated documentation for modules, functions, datatypes, and applications,
using a simple and compact syntax. The possible formats for references are:

| _Reference syntax_                    | _Example_                                       | _Scope_       |
| ------------------------------------- | ----------------------------------------------- | ------------- |
| `Module`                              | `m:edoc_run`, `erl.lang.list`                   | Global        |
| `Function/Arity`                      | `file/2`                                        | Within module |
| `Module:Function/Arity`               | `edoc:application/2`                            | Global        |
| `Type()`                              | `filename()`                                    | Within module |
| `Module:Type()`                       | [edoc:edoc_module()](`t:edoc:edoc_module/0`)    | Global        |
| `//Application`                       | [edoc](index.html)                              | Global        |
| `//Application/Module`                | `m:edoc_doclet`                                 | Global        |
| `//Application/Module:Function/Arity` | `edoc_run:file/1`                               | Global        |
| `//Application/Module:Type()`         | [edoc:edoc_module()](`t:edoc:edoc_module/0`)    | Global        |

_Table: reference syntax_

EDoc will resolve references using the information it finds in
`edoc-info`\-files at the locations specified with the `doc_path` option. EDoc
will automatically (and somewhat intelligently) try to find any local
`edoc-info`\-files using the current code path, and add them to the end of the
`doc_path` list. The target doc-directory is also searched for an existing info
file; this allows documentation to be built incrementally. (Use the `new` option
to ignore any old info file.)

Note that if the name of a module, function or datatype is explicitly qualified
with an application (as in "`//edoc/edoc_run`"), this overrides any other
information about that name, and the reference will be made relative to the
location of the application (if it can be found). This makes it possible to
refer to e.g. a module "`fred`" as "`//foo/fred`" without accidentally getting a
reference to "`//bar/fred`". You should not use this form of explicit references
for names that are local to the application you are currently creating - they
will always be resolved correctly.

Note that module-local references such as `file/2` only work properly within a
module. In an overview-page like this (i.e., the one you are currently reading),
no module context is available.

[](){: #Notes_on_XHTML }

## Notes on XHTML

In several places, XHTML markup can be used in the documentation text, in
particular in `@doc` tags. The main differences from HTML are the following:

- All elements must have explicit start and end tags, and be correctly nested.
  This means that you cannot e.g. write a `<li>` tag without also writing a
  corresponding `</li>` tag in the right place. This could be an annoyance at
  times, but has the great advantage that EDoc can report all malformed XHTML in
  your source code, rather than propagate the errors to the generated
  documentation.
- XHTML tag and attribute names should always be lower-case.
- Attributes must be quoted, as in e.g. `<a name="top">`.

To write an element like the HTML `<br>`, which has no actual content, you can
write either the full `<br></br>`, or better, use the XHTML abbreviated form
`<br/>`.

Since the purpose of EDoc is to document programs, there is also a limited form
of "wiki"-syntax available for making program code easier to write inline (and
to make the doc-comments easier to read). See
[Wiki notation](chapter.md#Wiki_notation) for details.

The HTML heading tags `h1` and `h2` are reserved for use by EDoc. Headings in
documentation source code should start at `h3`. There is however a special
syntax for writing headings which avoids using specific level numbers
altogether; see [Headings](chapter.md#headings) for details.

EDoc uses [XMerL](`e:xmerl:index.html`) to parse and export XML markup.

[](){: #Wiki_notation }

## Wiki notation

When EDoc parses XHTML, it does additional pre- and post-processing of the text
in order to expand certain notation specific to EDoc into proper XHTML markup.
This "wiki"
([http://en.wikipedia.org/wiki/Wiki](http://en.wikipedia.org/wiki/Wiki))
notation is intended to make it easier to write source code documentation.

[](){: #Empty_lines_separate_paragraphs }

### Empty lines separate paragraphs

Leaving an empty line in XHTML text (i.e., a line which except for any leading
start-of-comment '`%`' characters contains only whitespace), will make EDoc
split the text before and after the empty line into separate paragraphs. For
example:

```text
   %% @doc This will all be part of the first paragraph.
   %% It can stretch over several lines and contain <em>any
   %% XHTML markup</em>.
   %%
   %% This is the second paragraph. The above line is
   %% regarded as "empty" by EDoc, even though it ends with
   %% a space.
```

will generate the following text:

> This will all be part of the first paragraph. It can stretch over several
> lines and contain _any XHTML markup_.
>
> This is the second paragraph. The above line is regarded as "empty" by EDoc,
> even though it ends with a space.

Paragraph splitting takes place after the actual XHTML parsing. It only affects
block-level text, and not e.g., text within `<pre>` markup, or text that is
already within `<p>` markup.

### Headings

Section headings, sub-headings, and sub-sub-headings, can be written using the
following notation:

```text
   == Heading ==
   === Sub-heading ===
   ==== Sub-sub-heading ====
```

Such a heading must be alone on a line, except for whitespace, and cannot be
split over several lines. A link target is automatically created for the
heading, by replacing any whitespace within the text by a single underscore
character. E.g.,

```text
   == Concerning Hobbits ==
```

is equivalent to

```text
   <h3><a name="Concerning_Hobbits">Concerning Hobbits</a></h3>
```

Thus, headings using this notation should not contain characters that may not be
part of URL labels, except for whitespace. If you need to create such headings,
you have to use the explicit XHTML markup.

A hypertext link to a heading written this way can be created using the
`@section` macro, which transforms the argument text into a label as described
above. E.g.,

```text
   {@section Concerning Hobbits}
```

is equivalent to writing

```text
   <a href="#Concerning_Hobbits">Concerning Hobbits</a>
```

The above expansions take place before XML parsing.

[](){: #External_links }

### External links

Writing a URL within brackets, as in "`[http://www.w3c.org/]`", will generate a
hyperlink such as [http://www.w3c.org/](http://www.w3c.org/), using the URL both
for the destination and the label of the reference, equivalent to writing
"`<a href="http://www.w3c.org/"><code>http://www.w3c.org/</code></a>`". This
short-hand keeps external URL references short and readable. The recognized
protocols are `http`, `ftp`, and `file`. This expansion takes place before XML
parsing.

### TODO-notes

Lines that begin with the text "`TODO:`" (the colon is required) are recognized
as tags, as if they had been written as "`@todo ...`" (see
[@todo tags](chapter.md#gtag-todo) for further details).

[](){: #Verbatim_quoting }

### Verbatim quoting

In XHTML text, the '`` ` ``' character (Unicode `000060`, known as "grave
accent" or "back-quote") can be used for verbatim quoting. This expansion takes
place before XML parsing.

- A character sequence "`` `...' ``" or "` ``...'' `" will be expanded to
  "`<code>...</code>`", where all occurrences of the special XML characters
  '`<`' and '`&`' (and for completeness, also '`>`') in the quoted text have
  been escaped to "`&lt;`", "`&amp;`", and "`&gt;`", respectively. All
  whitespace is stripped from the beginning and end of the quoted text.

  Double back-quotes "` ``...'' `" can be used to quote text containing single
  '`'`' characters. The automatic stripping of any surrounding whitespace makes
  it possible to write things like "` `` 'foo@bar' '' `".

  To quote text containing "`''`" verbatim, explicit `<code>` markup or similar
  must be used.

- A character sequence "` ```...''' `" will be expanded to
  "`<pre><![CDATA[...]]></pre>`", which disables all XML markup within the
  quoted text, and displays the result in fixed-font with preserved indentation.
  Whitespace is stripped from the end of the quoted text, but not from the
  beginning, except for whole leading lines of whitespace. This is useful for
  multi-line code examples, or displayed one-liners.
- To produce a single '`` ` ``'-character in XML without beginning a new quote,
  you can write "`` `' ``" (no space between the '`` ` ``' and the '`'`'). You
  can of course also use the XML character entity "`&#x60;`".

Examples:

```text
     %% @doc ...where the variable `Foo' refers to...
```

```text
     %% @doc ...returns the atom `` 'foo@erlang.org' ''...
```

````text
     %% @doc ...use the command ```erl -name foo''' to...
````

````text
     %% @doc ...as in the following code:
     %% ```f(X) ->
     %%       case X of
     %%          ...
     %%       end'''
````

````text
     %% @doc ...or in the following:
     %% ```
     %%     g(X) ->
     %%       fun () -> ... end
     %% '''
````

[](){: #Macro_expansion }

## Macro expansion

Before the content of a tag is parsed, the text undergoes _macro expansion_. The
syntax for macro calls is:

```text
    {@name}
```

or

```text
    {@name argument}
```

where _name_ and _argument_ are separated by one or more whitespace characters.
The argument can be any text, which may contain other macro calls. The number of
non-escaped "`{@`" and "`}`" delimiters must be balanced.

The argument text is first expanded in the current environment, and the result
is bound to the _macro parameter_, written `{@?}`. (If no argument is given,
`{@?}` is bound to the empty string.) The macro definition is then substituted
for the call, and expansion continues over the resulting text. Recursive macro
expansions are not allowed.

[](){: #User-defined_macros }

### User-defined macros

Users can define their own macros by using the `def` EDoc option; see
`edoc:file/2` and `edoc:get_doc/2` for more information. User-defined macros
override predefined macros.

[](){: #Predefined_macros }

### Predefined macros

- **`{@date}`** - [](){: #predefmacro-date } Expands to the current date, as
  "`Month Day Year`", e.g. "Jan 29 2024".

- **`{@link reference. description}`** - [](){: #predefmacro-link } This creates
  a hypertext link; cf. the [@see function tag](chapter.md#ftag-see) above for
  details. The description text (including the period separator) is optional; if
  no text is given, the reference itself is used. For example,
  `{@link edoc:file/2}` creates the link `edoc:file/2`, and
  `{@link edoc:file/2. <em>this link</em>}` creates [this link](`edoc:file/2`).

- **`{@module}`** - [](){: #predefmacro-module } Expands to the name of the
  current module. Only defined when a module is being processed.

- **`{@section heading}`** - [](){: #predefmacro-section } Expands to a
  hypertext link to the specified section heading; see
  [Headings](chapter.md#headings) for more information.

- **`{@time}`** - [](){: #predefmacro-time } Expands to the current time, as
  "`Hr:Min:Sec`", e.g. "14:53:19".

- **`{@type type-expression}`** - [](){: #predefmacro-type } Formats a type
  expression within `<code>...</code>` markup and with hypertext links for data
  types. For example, `{@type {options, List::edoc:option_list()@}}` generates
  "`{options, List::edoc:option_list()}`". (Cf.
  [Escape sequences](chapter.md#Escape_sequences).)

- **`{@version}`** - [](){: #predefmacro-version } Intended for use in
  [@version tags](chapter.md#mtag-version). Defaults to a timestamp using
  `{@date}` and `{@time}`. Typically, this macro is redefined by the user when
  an official release of the application is generated.

[](){: #Escape_sequences }

### Escape sequences

To prevent certain characters from being interpreted as delimiters, for example
to produce the text "`{@`" in the output, or use a '`}`' character in the
argument text of a macro call, the following escape sequences may be used:

- **`@{`** - Expands to "`{`". Example:

  ```text
     %% @doc A macro call starts with the sequence "@{@".
  ```

- **`@}`** - Expands to "`}`". Example:

  ```text
     %% @doc ...{@foo ...{Key, Value@}...}...
  ```

- **`@@`** - Expands to "`@`". Example:

  ```text
     %% @doc Contact us at support@@{@hostname}
  ```

  Will generate the text "Contact us at support@vaporware.acme.com" if the macro
  `hostname` is bound to "`vaporware.acme.com`". Also:

  ```text
     %% @doc You might want to write something like
     %% @@foo that will expand to @foo and does not start
     %% a new tag even if it appears first in a line.
  ```

[](){: #Type_specifications }

## Type specifications

[](){: #Function_specifications }

### Function specifications

Note that although the syntax described in the following can still be used for
specifying functions we recommend that Erlang specifications (that is the
`-spec` and `-type` attributes) as described in
[Types and Function Specification](`e:system:typespec.md`) should be added to
the source code instead. This way the analyses of [Dialyzer](`m:dialyzer`)'s can
be utilized in the process of keeping the documentation consistent and
up-to-date.

Erlang specifications (`-spec` and `-type`) are required to properly generate
doc chunks. Redundant `-spec` attributes and `@spec` tags will cause warnings to
be emitted and the specifications to be skipped in chunks. Old-style `@spec` and
`@type` specifications can still be used to generate static HTML documentation.

The following grammar describes the form of the specifications following a
`@spec` tag. A '`?`' suffix implies that the element is optional. Function types
have higher precedence than union types; e.g.,
"`(atom()) -> atom() | integer()`" is parsed as
`((atom()) -> atom()) | integer()`, not as `(atom()) -> (atom() | integer())`.

| `Spec`          | ::= | `FunType "where"? DefList?           | FunctionName FunType "where"? DefList?`        |
| --------------- | --- | ------------------------------------ | ---------------------------------------------- | ------------------ | -------------------------------- | -------------------- | ------- | ------------------ | ---------- | ------------------- | ------------------------ | ------- | ----------------- | --------------------------- | ----------------- | ------- | ---------------------------- | ------------------------------------------- | ------------------------------------------------------------- |
| `FunctionName`  | ::= | `Atom`                               |
| `FunType`       | ::= | `"(" UnionTypes? ")" "->" UnionType` |
| `UnionTypes`    | ::= | `UnionType                           | UnionType "," UnionTypes`                      |
| `UnionType`     | ::= | `UnionList                           | Name "::" UnionList`                           |
| `Name`          | ::= | `Variable`                           |
| `UnionList`     | ::= | `Type                                | Type "+" UnionList                             | Type "             | " UnionList`                     |
| `Type`          | ::= | `TypeVariable                        | Atom                                           | Integer            | Float                            | Integer ".." Integer | FunType | "fun(" FunType ")" | "fun(...)" | "{" UnionTypes? "}" | "#" Atom "{" Fields? "}" | "[" "]" | "[" UnionType "]" | "[" UnionType "," "..." "]" | "(" UnionType ")" | BinType | TypeName "(" UnionTypes? ")" | ModuleName ":" TypeName "(" UnionTypes? ")" | "//" AppName "/" ModuleName ":" TypeName "(" UnionTypes? ")"` |
| `Fields`        | ::= | `Field                               | Fields "," Fields`                             |
| `Field`         | ::= | `Atom "=" UnionList`                 |
| `BinType`       | ::= | `"<<>>"                              | "<<" BaseType ">>"                             | "<<" UnitType ">>" | "<<" BaseType "," UnitType ">>"` |
| `BaseType`      | ::= | `"_" ":" Integer`                    |
| `UnitType`      | ::= | `"_" ":" "_" "*" Integer`            |
| `TypeVariable`  | ::= | `Variable`                           |
| `TypeName`      | ::= | `Atom`                               |
| `ModuleName`    | ::= | `Atom                                | ModuleName "." Atom`                           |
| `AppName`       | ::= | `Atom`                               |
| `DefList`       | ::= | `Def                                 | DefList Def                                    | DefList "," Def`   |
| `Def`           | ::= | `TypeVariable "=" UnionList          | TypeName "(" TypeVariables? ")" "=" UnionType` |
| `TypeVariables` | ::= | `TypeVariable                        | TypeVariable "," TypeVariables`                |

_Table: specification syntax grammar_

Examples:

```text
    -spec my_function(X :: integer()) -> integer().
    %% @doc Creates ...
```

```text
    %% @spec my_function(X::integer()) -> integer()
```

```text
    %% @spec (X::integer()) -> integer()
```

```text
    %% @spec sqrt(float()) -> float()
```

```text
    %% @spec pair(S, T) -> {S, T}
```

```text
    %% @spec append(List, List) -> List
    %%       List = [term()]
```

```text
    %% @spec append(A::List, B::List) -> List
    %%       List = [Item]
    %%       Item = term()
```

```text
    %% @spec open(File::filename()) -> FileDescriptor
    %% where
    %%       filename() = string() + atom(),
    %%       FileDescriptor = term()
```

```text
    %% @spec close(graphics:window()) -> ok
```

The first example shows the recommended way of specifying functions.

In the above examples, `X`, `A`, `B`, and `File` are parameter names, used for
referring to the parameters from the documentation text. The _type variables_
`S`, `T` and `List` are used to simplify the type specifications, and may be
supplied with definitions. It is also possible to give definitions for named
types, which means that the name is simply an alias. (Use the `@type` tag to
document abstract data types.) If a named type is defined in another module, it
can be referred to as `Module:TypeName(...)`. Note that the keyword '`where`' is
optional before a list of definitions, and that the definitions in the list may
optionally be separated by '`,`'.

Both the '`|`' and the '`+`' character may be used to separate alternatives in
union types; there is no semantic difference. Note that the notation `[Type]`
means "proper (nil-terminated) list whose elements all belong to `Type`"; For
example, `[atom()|integer()]` means the same thing as `[atom()+integer()]`,
i.e., a proper list of atoms and/or integers.

If only a type variable is given for a parameter, as in "`pair(S, T) -> ...`",
the same variable name may implicitly be used as the parameter name; there is no
need to write "`pair(S::S, T::T) -> ...`".

EDoc automatically extracts possible parameter names from the source code, to be
used if no parameter name is given in the specification (or if the specification
is missing altogether). If this fails, EDoc will generate a dummy parameter
name, such as `X1`. This way, EDoc can often produce helpful documentation even
for code that does not contain any annotations at all.

[](){: #Type_definitions }

### Type definitions

Note that although the syntax described in the following can still be used for
specifying types we recommend that Erlang types as described in
[Types and Function Specification](`e:system:typespec.md`) should be added to
the source code instead.

Old-style `@type` tags will result in just type names, but no definitions, in
doc chunks - please use `-type` attributes to ensure all available type
information is available in the chunks. For static HTML, Erlang types will be
used unless there is a type alias with the same name.

The following grammar (see above for auxiliary definitions) describes the form
of the definitions that may follow a `@type` tag:

| `Typedef` | ::= | `TypeName "(" TypeVariables? ")" DefList? | TypeName "(" TypeVariables? ")" "=" UnionList DefList?` |
| --------- | --- | ----------------------------------------- | ------------------------------------------------------- |

_Table: type definition grammar_

(For a truly abstract data type, no equivalence is specified.) The main
definition may be followed by additional local definitions. Examples:

```text
    -type my_list(X) :: [X]. %% A special kind of lists ...
```

```text
    -opaque another_list(X) :: [X].
    %% another_list() is a kind of list...
```

```text
    %% @type myList(X). A special kind of lists ...
```

```text
    %% @type filename() = string(). Atoms not allowed!
```

```text
    %% @type thing(A) = {thong, A}
    %%           A = term().
    %%   A kind of wrapper type thingy.
```

The first two examples show the recommended way of specifying types.

[](){: #Pre-defined_data_types }

### Pre-defined data types

The following data types are predefined by EDoc, and may not be redefined:

```text
    any()
    arity()
    atom()
    binary()
    bitstring()
    bool()        (allowed, but use boolean() instead)
    boolean()
    byte()
    char()
    cons()
    deep_string()
    float()
    function()
    integer()
    iodata()
    iolist()
    list()
    maybe_improper_list()
    mfa()
    module()
    nil()
    neg_integer()
    node()
    non_neg_integer()
    nonempty_improper_list()
    nonempty_list()
    nonempty_maybe_improper_list()
    nonempty_string()
    none()
    number()
    pid()
    port()
    pos_integer()
    reference()
    string()
    term()
    timeout()
    tuple()
```

Details:

- `t:any/0` means "any Erlang data type". `t:term/0` is simply an alias for
  `t:any/0`.
- `t:atom/0`, `t:binary/0`, `t:float/0`, `t:function/0`, `t:integer/0`,
  `t:pid/0`, `t:port/0` and `t:reference/0` are primitive data types of the
  Erlang programming language.
- `t:boolean/0` is the subset of `t:atom/0` consisting of the atoms `true` and
  `false`.
- `t:char/0` is the subset of `t:integer/0` representing Unicode character
  codes: hex 000000-10FFFF.
- `t:tuple/0` is the set of all tuples `{...}`.
- [`list(T)`](`t:list/1`) is just an alias for `[T]`; list() is an alias for
  [`list(any())`](`t:list/1`), i.e., `[any()]`.
- `t:nil/0` is an alias for the empty list `[]`.
- `cons(H,T)` is the list constructor. This is usually not used directly. It is
  possible to recursively define `list(T) := nil()+cons(T,list(T))`.
- `t:string/0` is an alias for `[char()]`.
- `deep_string()` is recursively defined as `[char()+deep_string()]`.
- `t:none/0` means "no data type". E.g., a function that never returns has type
  `(...) -> none()`

[](){: #Doc_chunks }

## Doc chunks

EDoc implements
[EEP-48](https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html) and
allows to output doc chunks for Erlang projects which use the EDoc language for
source code documentation.

There are two ways to generate the doc chunks: either by using
[bin/edoc](edoc_cmd.md) or by using the
[Rebar3 edoc command](http://rebar3.org/docs/commands/#edoc). Ultimately,
they're both just entry points to EDoc, the application, so deciding which to
use is just a matter of preference.

[](){: #Using_edoc_EScript }

### Using edoc EScript

In order to generate doc chunks using [bin/edoc](edoc_cmd.md), make sure the
compiled Erlang app can be found in the Erlang code path:

```text
$ pwd
/tmp/recon
$ rebar3 compile
$ export PATH="$(dirname `which erl`)/../lib/erlang/lib/edoc-0.12/bin:$PATH"
$ edoc -app recon -chunks -pa _build/default/lib/recon/ebin
Running with opts:
#{app => recon,
  code_paths => ["_build/default/lib/recon/ebin"],
  files => [],mode => chunks,run => app}
$ ls _build/default/lib/recon/doc/chunks/
recon.chunk        recon_lib.chunk    recon_rec.chunk
recon_alloc.chunk  recon_map.chunk    recon_trace.chunk
```

The project does not have to be built with Rebar3 - the above is just an
example. From now on the chunks will be available as the source of
[documentation for the Erlang shell](`m:shell_docs`):

```text
$ erl -pa _build/default/lib/recon/ebin
...
1> h(recon).

   recon

  Recon, as a module, provides access to the high-level functionality contained in the
  Recon application.
  ...

2> h(recon_alloc, allocators, 0).

  -spec allocators() -> [allocdata(term())].

  returns a dump of all allocator settings and values
```

[](){: #Using_Rebar3_edoc_command }

### Using Rebar3 edoc command

Doc chunks can also be built using `rebar3 edoc`, given `edoc_opts` is properly
set up:

```text
$ pwd
/tmp/recon
$ cat >> rebar.config <<EOF
> {edoc_opts, [{doclet, edoc_doclet_chunks},
>              {layout, edoc_layout_chunks},
>              {preprocess, true},
>              {dir, "_build/docs/lib/recon/doc"}]}.
> EOF
$ rebar3 edoc
$ ls _build/docs/lib/recon/doc/chunks/
recon.chunk        recon_lib.chunk    recon_rec.chunk
recon_alloc.chunk  recon_map.chunk    recon_trace.chunk
```

`rebar3 shell` will set up the paths for us if we use the `docs` profile:

```text
$ rebar3 as docs shell
...
1> h(recon).

   recon

  Recon, as a module, provides access to the high-level functionality contained in the
  Recon application.
  ...
```

[](){: #Using_the_EDoc_API }

### Using the EDoc API

EDoc comes with two sets of doclet/layout pairs:

- `m:edoc_doclet` and `m:edoc_layout` \- the default pair which is used to
  generate static HTML documentation
- `m:edoc_doclet_chunks` and `m:edoc_layout_chunks` \- the
  [EEP-48](https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html)
  compliant pair which generates doc chunks with
  [erlang+html(3)](`e:edoc:doc_storage.md`)

In order to generate doc chunks using `edoc:application/2` or `edoc:files/2` we
have to specify which doclet and layout we want to use:

```text
Opts = [{doclet, edoc_doclet_chunks},
        {layout, edoc_layout_chunks}].
```

Then, it's just a matter of deciding whether we want to generate documentation
for the whole application (`edoc:application/2`) or only selected source files
(`edoc:files/2`):

```text
App = my_app.
edoc:application(App, Opts).
%% or
Files = ["src/my_app_mod1.erl", "src/my_app_mod2.erl"].
edoc:files(Files, Opts).
```

See `src/edoc_cli.erl` source code for an example of using this interface.

## Acknowledgements

Since the first version of EDoc, several people have come up with suggestions
(Luke Gorrie, Joe Armstrong, Erik Stenman, Sean Hinde, Ulf Wiger, ...), and some
have even submitted code to demonstrate their ideas (Vlad Dumitrescu, Johan
Blom, Vijay Hirani, ...). None of that code was actually included in the Great
Rewriting that followed the initial public release (EDoc version 0.1), but most
of the central points were addressed in the new system, such as better
modularization and possibility to plug in different layout engines, and making
EDoc understand the application directory layout.

It is now getting too hard to keep track of all the people who have made further
suggestions or submitted bug reports, but your input is always appreciated.
Thank you.
