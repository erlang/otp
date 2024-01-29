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
# User's Guide DTDs

[](){: #partDTD }

## The part DTD

The `part` DTD is intended for a "normal" document, like the User's Guide or
Release Notes. First are some paragraphs introducing the main contents. After
that follows chapters, written in separate files with the
[chapter](user_guide_dtds.md#chapterDTD) DTD.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE part SYSTEM "part.dtd">
<part>
  <header>
    <title>The chapter title</title>
    <prepared>The author</prepared>
    <docno/>
    <date/>
    <rev/>
  </header>

  <description>
    <p>Some text..</p>
  </description>

  <include file="file1"></include>
  <include file="file2"></include>
</part>
```

[](){: #partTAG }

## <part>

The top level tag of a `part` DTD.

Contains a [<header>](header_tags.md), an optional
[<description>](user_guide_dtds.md#descriptionTAG), followed by one or more
[<include>](user_guide_dtds.md#includeTAG).

[](){: #descriptionTAG }

## <description>

The introduction after the title and before the bulk of included chapters/manual
pages.

Contains any combination and any number of [block tags](block_tags.md) except
`<image>` and `<table>`.

[](){: #includeTAG }

## <include>

An empty tag. The attribute `file` specifies a file to include. The `.xml` file
extension should be omitted.

Example:

```text
<include file="notes"></include>
```

[](){: #chapterDTD }

## The chapter DTD

The `chapter` DTD is intended for a chapter in a User's Guide or similar with
text divided into sections, which can be nested.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE chapter SYSTEM "chapter.dtd">
<chapter>
  <header>
    <title>Title on first level</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <p>Introduction...</p>

  <section>
    <title>Title on second level</title>

    <p>First paragraph.</p>

    <p>Second paragraph etc.</p>

    <section>
      <title>Title on third level</title>

      <p>...</p>
    </section>
  </section>

  ...
</chapter>
```

[](){: #chapterTAG }

## <chapter>

The top level tag of a `chapter` DTD.

Contains a [<header>](header_tags.md), an optional introduction consisting of
any combination of [block tags](block_tags.md), followed by one or more
[<section>](user_guide_dtds.md#sectionTAG).

[](){: #sectionTAG }

## <section>

Subdivision of a chapter.

Contains an optional [<marker>](inline_tags.md#markerTAG), a
[<title>](user_guide_dtds.md#titleTAG), followed by any combination and any
number of [block tags](block_tags.md) and
`<section ghlink="maint/lib/erl_docgen/doc/src/user_guide_dtds.xml#L172">`.

[](){: #titleTAG }

## <title>

Section title, contains plain text.
