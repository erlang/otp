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
# EEP-48: Documentation storage and format

This User's Guide describes the documentation storage format initially described
in [EEP-48](https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html).
By standardizing how API documentation is stored, it will be possible to write
tools that work across languages.

To fetch the EEP-48 documentation for a module, use `code:get_doc/1`.

To render the EEP-48 documentation for an Erlang module, use
`shell_docs:render/2`.

## The "Docs" storage

To look for documentation for a module named `example`, a tool should:

Look for `example.beam` in the code path, parse the BEAM file, and retrieve the
`Docs` chunk. If the chunk is not available, it should look for `"example.beam"`
in the code path and find the `doc/chunks/example.chunk` file in the application
that defines the `example` module. If no `.chunk` file exists,
documentation is not available.

The choice of using a chunk or the filesystem is completely up to the language
or library. In both cases, the documentation can be added or removed at any
moment by stripping the `Docs` chunk (using `m:beam_lib`) or by removing the
`doc/chunks` directory.

For example, languages such as Elixir and LFE attach the `Docs` chunk at
compilation time, which can be controlled via a compiler flag, while
other languages might want to generate the documentation separate from
the compilation of the source code.

## The "Docs" format

In both storages, the documentation is written in the exactly same format: an
Erlang term serialized to binary via
[`term_to_binary/1`](`erlang:term_to_binary/1`). The term can be optionally
compressed when serialized. It must follow the type specification below:

```erlang
{docs_v1,
 Anno :: erl_anno:anno(),
 BeamLanguage :: atom(),
 Format :: binary(),
 ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
 Metadata :: map(),
 Docs ::
   [{{Kind, Name, Arity},
     Anno :: erl_anno:anno(),
     Signature :: [binary()],
     Doc :: #{DocLanguage := DocValue} | none | hidden,
     Metadata :: map()
    }]} when DocLanguage :: binary(),
             DocValue :: binary() | term()
```

where in the root tuple we have:

- **`Anno`** - annotation (line, column, file) of the definition itself (see
  `m:erl_anno`)

- **`BeamLanguage`** - an atom representing the language, for example: `erlang`,
  `elixir`, `lfe`, `alpaca`, and so on

- **`Format`** - the mime type of the documentation, such as `<<"text/markdown">>`
  or `<<"application/erlang+html">>`. For details of the format used by Erlang
  see the [`EEP-48 Chapter`](`e:edoc:doc_storage.md`) in EDoc's User's
  Guide.

- **`ModuleDoc`** - a map with the documentation language as key, such as
  `<<"en">>` or `<<"pt_BR">>`, and the documentation as a binary value. It can
  be atom `none` if no documentation exists or the atom `hidden` if
  documentation has been explicitly disabled for this entry.

- **`Metadata`** - a map of atom keys with any term as value. This can be used to
  add annotations like the `authors` of a module, `deprecated`, or anything else
  a language or documentation tool finds relevant.

- **`Docs`** - a list of documentation for other entities (such as functions and
  types) in the module.

For each entry in Docs, we have:

- **`{Kind, Name, Arity}`** - the kind, name and arity identifying the function,
  callback, type, and so on. The official entities are: `function`, `type`, and
  `callback`. Other languages will add their own. For instance, Elixir and LFE
  might add `macro`.

- **`Anno`** - annotation (line, column, file) of the module documentation or of
  the definition itself (see `m:erl_anno`).

- **`Signature`** - the signature of the entity. It is is a list of binaries.
  Each entry represents a binary in the signature that can be joined with
  whitespace or newline. For example,
  `[<<"binary_to_atom(Binary, Encoding)">>, <<"when is_binary(Binary)">>]` can
  be rendered as a single line or two lines. It exists exclusively for
  exhibition purposes.

- **`Doc`** - a map with the documentation language as key, such as `<<"en">>` or
  `<<"pt_BR">>`, and the documentation as a value. The documentation can either be
  a binary or any Erlang term, both described by `Format`. If it is an Erlang
  term, then `Format` must be `<<"application/erlang+SUFFIX">>`, such as
  `<<"application/erlang+html">>` when the documentation is an Erlang
  representation of an HTML document. `Doc` can also be atom `none`
  if no documentation exists or the atom `hidden` if documentation has been
  explicitly disabled for this entry.

- **`Metadata`** - a map of atom keys with any term as value.

This shared format is the heart of the EEP as it is what effectively allows
cross-language collaboration.

The Metadata field exists to allow languages, tools, and libraries to add custom
information to each entry. This EEP documents the following metadata keys:

- **`authors := [binary()]`** - a list of authors as binaries.

- **`cross_references := [module() | {module(), {Kind, Name, Arity}}]`** - a
  list of modules or module entries that can be used as cross references when
  generating documentation.

- **`deprecated := binary()`** - when present, it means the current entry is
  deprecated with a binary that represents the reason for deprecation and a
  recommendation to replace the deprecated code.

- **`since := binary()`** - a binary representing the version such entry was
  added, such as `<<"1.3.0">>` or `<<"20.0">>`.

- **`edit_url := binary()`** - a binary representing a URL to change the
  documentation itself.

Any key may be added to Metadata at any time. Keys that are frequently used by
the community can be standardized in future versions.

## See Also

`m:erl_anno`, `m:shell_docs`,
[`EEP-48 Chapter in EDoc's User's Guide`](`e:edoc:doc_storage.md`),
`code:get_doc/1`
