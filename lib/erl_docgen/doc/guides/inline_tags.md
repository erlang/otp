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
# Inline Tags

Inline tags are typically used within block tags, for example to highlight a
word within a paragraph.

[](){: #brTAG }

## <br> - Line Break

Forces a newline. The `<br>` tag is both a block- and an inline tag and is
described in the [Block Tags](block_tags.md#brTAG) section.

[](){: #cTAG }

## <c> - Code

Highlights things like variables and file names in a text flow. Can contain
plain text only. Newlines and tabs are ignored as opposed to the
[code](block_tags.md#codeTAG) tag. All
[character entities](character_entities.md) are expanded. Example:

```text
<p>Returns <c>true</c> if <c>Term</c> is an integer.</p>
```

results in:

Returns `true` if `Term` is an integer.

[](){: #emTAG }

## <em> - Emphasis

Highlights words which are important within a text flow. Example:

```text
<p>The application <em>must</em> be up and running.</p>
```

results in:

The application _must_ be up and running.

Contains plain text or a [<c>](inline_tags.md#cTAG) tag.

[](){: #markerTAG }

## <marker> - Marker

Used as an anchor for hypertext references. The `id` attribute defines the name
of the marker. Example:

[](){: #marker_example }

```text
<marker id="marker_example"/>
```

The [<see\*>](inline_tags.md#seeTAG) tags are used to refer to the marker.

The `<marker>` tag is both a block- and an inline tag.

[](){: #seeTAG }

## <see\*> - See tags

A cross reference (hypertext link) to a marker in the same file, a marker in
another file, or (the top of) another file, given by the `marker` attribute. The
syntax used within the `marker` attribute is `application:file#anchor` for the
general case. `application` and `file` can be omitted if the link target is the
current application or file.

There are several different see tags that are to be used depending on what it is
that they point to.

- **<seemfa>** - Points to an MFA using the syntax
  `application:module#function/arity`. These links must point to functions
  documented in a <funcs> section. Examples:

  ```text
  <seemfa marker="stdlib:string#length/1">string:length/1</seemfa>
  <seemfa marker="string#length/1">string:length/1</seemfa>
  <seemfa marker="#length/1">string:length/1</seemfa>
  ```

  results in: `string:length/1`.

- **<seeerl>** - Points to an Erlang module or a custom
  [marker](inline_tags.md#markerTAG) within a module. Example:

  ```text
  <seeerl marker="stdlib:string">string(3)</seeerl>,
  <seeerl marker="stdlib:string#obsolete-api-functions">Old API in string</seeerl>
  ```

  results in: `m:string`,[Old API in string](`m:string#obsolete-api-functions`).

- **<seetype>** - Points to a type using the syntax `application:module#type`.
  These links must point to types documented in a <datatypes> section. Example:

  ```text
  <seetype marker="stdlib:string#grapheme_cluster">string::grapheme_cluster()</seetype>
  ```

  results in: [string::grapheme_cluster()](`t:string:grapheme_cluster/0`).

- **<seeapp>** - Points to the application documentation. `index` can be used as
  the target file. Example:

  ```text
  <seeapp marker="stdlib:STDLIB_app">STDLIB app</seeapp>,
  <seeapp marker="stdlib:index">STDLIB index</seeapp>
  ```

  results in: [STDLIB](`e:stdlib:stdlib_app.md`),
  [STDLIB](`e:stdlib:index.html`).

- **<seecom>** - Points to the documentation of any command line utility.
  Example:

  ```text
  <seecom marker="erts:epmd">epmd</seecom>
  ```

  results in: [epmd](`e:erts:epmd_cmd.md`).

- **<seecref>** - Points to the documentation of any C reference. Example:

  ```text
  <seecref marker="erts:erl_nif">erl_nif</seecref>
  ```

  results in: [erl_nif](`e:erts:erl_nif.md`).

- **<seefile>** - Points to the documentation of a file format. Example:

  ```text
  <seefile marker="kernel:config">config(3)</seefile>
  ```

  results in: [config(3)](`e:kernel:config.md`).

- **<seeguide>** - Points to the User"s Guide of any application. `index` can be
  used as the target file. Example:

  ```text
  <seeguide marker="kernel:index">Kernel User's Guide Index</seeguide>,
  <seeguide marker="kernel:logger_chapter">Logging in the Kernel User's Guide</seeguide>
  ```

  results in: [Kernel User's Guide Index](`e:kernel:index.html`),
  [Logging in the Kernel User's Guide](`e:kernel:logger_chapter.md`).

[](){: #urlTAG }

## <url> - Non-Local Cross Reference

A reference to a file outside the documentation, a web address or similar, given
by the `href` attribute. Must contain plain text. Example:

```text
<url href="http://www.erlang.org">erlang.org</url>
```

results in: [erlang.org](http://www.erlang.org)
