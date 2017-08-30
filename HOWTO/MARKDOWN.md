Erlangish Markdown Text Files
=============================

Introduction
------------

If you are looking for information on how to build and install Erlang/OTP you
want to read the [$ERL_TOP/HOWTO/INSTALL.md][] document instead of this document
(where `$ERL_TOP` is the top source directory in the source tree).

All files with the `.md` suffix (as well as this file) are ordinary text files
written using a Markdown like notation, and can be read using an ordinary text
editor such as for example `emacs`.

This document describes how `*.md` files in the Erlang/OTP source tree should
be written.

> *NOTE*: Before modifying a `*.md` file read all of this document.

Erlangish Markdown
------------------

We do not use Markdown straight out of the box. The Markdown syntax we use is
similar to the original Markdown but with a number of tweaks. The original
Markdown is documented at <http://daringfireball.net/projects/markdown/>. You
should read that documentation as well as this document before modifying any
Erlangish Markdown files in the Erlang/OTP source tree.

The original Markdown syntax was designed for generating HTML code from an
"easy to read/easy to write" plain text. Instead of generating HTML we generate
XML that fits into our documentation, i.e. we generate Erlangish XML from
Erlangish Markdown.

The `.md` suffix has been chosen since [github][] will generate HTML pages for
files with the `.md` suffix. Github does however not generate HTML according to
Erlangish Markdown, so some features do not work. The Erlangish Markdown
documents viewed at [our github repository][] will typically suffer from broken
links. The original Markdown script, gitub's Markdown, and our Erlangish
Markdown script will generate somewhat different results if you do not follow
indentation rules outlined in the Markdown documentation. You are encouraged to
try to write using a Markdown syntax that also looks nice on github. However,
it is *much* more important that the document is formatted correct in the
Erlang/OTP documentation.

### Differences Between Markdown and Erlangish Markdown ###

#### Missing Features ####

This functionality is missing compared to Markdown version 1.0.1. Do not
depend on the fact that these features are missing. Some of them might appear
in the future.

*   No inline HTML. Currently no inline XML is allowed either. Inline XML might
    be allowed in the future, but there is no real need for it since we use
    Erlangish Markdown for "readme"s that have a main purpose of being readable
    in plain text.

*   Backslash escapes all characters.

*   No support for "horizontal rules".

*   Links.
    *   No support for the "title" attribute.
    *   Automatic links does not support email addresses.

*   Images.
    *   No support for the "title" attribute. Specified "title" will however
        be used as `<icaption>`.
    *   No support for the "alt" attribute.

*   Lists aren't supported inside block quotes.

*   Nested block quotes can be generated, but current DTD does not
    support it.

*   Link and image definition names *are* case sensitive.

#### Additional Features ####

*   Automatic anchors at each heading.

*   Optionally automatically generated table of contents.

*   Note blocks.

*   Warning blocks.

#### Extra requirements ####

*   One and only one level 1 heading is allowed and required.

*   The level 1 heading must be the first heading in the document.

*   A level `X` heading must have a level `X-1` heading as parent heading.

*   Link and image definition names aren't allowed to begin with a
    question mark (?) character. Names beginning with a question mark have
    been reserved for other use.

*   The encoding of the file containing Erlangish Markdown should be
    UTF-8.

### Generated XML ###

> *WARNING*: The `emd2exml` script will blindly generate XML code according
> to the Erlangish Markdown in a file. Successfully generated XML does **not**
> imply that the generated XML adheres to used DTDs. `emd2exml` does very
> seldom fail and can easily generate XML that will cause the documentation
> build to fail. You always have to keep in mind that the XML generated
> should fit the chapter DTD of Erlang/OTP. Also note that even though HTML
> generation succeeds the PDF generation might fail, etc.
>
> *Always build the Erlang/OTP documentation after modifying an Erlangish
> Markdown document!*

A note about how we talk about "tags" below. When we say "generate(s) `<X>`
tags" this also imply that ending `</X>` tags will be generated at appropriate
places. Appropriate attributes to the `X` tag will also be generated.

*   Inline and reference style links will either generate `<seealso>` tags
    or `<url>` tags. If a "://" character sequence is found in the URL an
    `<url>` tag will be generated; otherwise, a `<seealso>` tag is generated.

*   Automatic links will only generate `<url>` tags. This since a
    "://" character sequence have to be present in the URL in order
    for the link to be identified.

*   Inline and reference style images will generate a `<image file="...">
    <icaption>...</icaption> </image>` sequence where the "title" will be
    placed between `<icaption>` and `</icaption>`.

*   Block quotes generate `<quote>` tags.

*   If the first line of a top level block quote begins with a `> *NOTE*:`
    character sequence, a `<note>` tag will be generated instead of a
    `<blockquote>` tag. The note will span the entire block quote.

*   If the first line of a top level block quote begins with a `> *WARNING*:`
    character sequence, a `<warning>` tag will be generated instead of a
    `<blockquote>` tag. The warning will span the entire block quote.

*   Paragraphs will generate `<p>` tags.

*   Break line (double trailing white space) will generate `<br/>` tags.

*   An unordered list generates a `<list type="bulleted">` tag and `<item>`
    tags for each item.

*   An ordered list generates a `<list type="ordered">` tag and `<item>` tags
    for each item.

*   Code blocks will generate `<code type="none">` tags.

*   Code span (backticks) will generate `<c>` tags.

*   Emphasis (single `*` or `_`) will generate `<em>` tags.

*   Strong emphasis (double `*` or `_`) will generate `<strong>` tags.

*   The level 1 heading will cause the following to be generated:

        <?xml version="1.0" encoding="utf8" ?>
        <!DOCTYPE chapter SYSTEM "chapter.dtd">
        <chapter>
        <header>
        <copyright>
        ...
        </copyright>
        <legalnotice>
        ...
        </legalnotice>

        <title>...</title>
        ...
        <file>...</file>
        </header>

        ...

        </chapter>

    The content of copyright section and the legalnotice section will
    contain information from a \%CopyrightBegin\%, \%CopyrightEnd\% block
    if such exist (see below).

*   A level `X` heading where `1 < X <= 6` will cause the the following
    to be generated:

        <section>
            <marker id="..."/>
            <title>...</title>
            ...
        </section>

    The marker id is automatically generated as a combination of all parent
    headings up to and including level 2 separated by a `_` character. As in
    `<marker heading 2>_<marker heading 3>_ ... _<current marker heading>`
    where each "marker heading" is constructed as follows. All characters a-z
    and A-Z are kept as they are, space and tab characters are replaced by
    `-` characters, and all other characters are dropped.

    This way it is relatively easy to make sure that all marker ids of a
    document are unique, but there is of course no guarantee that they are.

    The upside of these auto generated markers is that we wont have to clutter
    the document with XML or something else while being able to refer into
    the document. The downside is that if you change a level 2 heading you
    change a lot of marker ids which may break links into a document from
    other documents. That is, *be careful* when changing headings in an
    existing document.

*   A level `X` heading where `6 < X` will cause the the following
    to be generated:

        <marker id="..."/>
        <p><strong>...</strong></p>
        ...

    Current DTD:s used don't support deeper levels of sections, and we
    therefore simulate a section this way. The marker id is generated as for
    a true section (see above).

*   If a section enclosed by \%CopyrightBegin\% and \%CopyrightEnd\% is
    encountered, it will be interpreted as an EPL copyright and license,
    and will be used in the header section of the document. The
    \%CopyrightBegin\% and \%CopyrightEnd\% "tags" will be removed from
    the output.

*   All occurrences of \%OTP-REL% will be replaced by current OTP release number
    (e.g. 17).

*   All occurrences of \%OTP-VSN% will be replaced by current OTP version
    (e.g. 17.0).

*   All occurrences of \%ERTS-VSN% will be replaced by current ERTS version
    (e.g. 5.8).

*   Adding a `[?TOC]: true` line (optionally indented with three spaces)
    anywhere in the document will cause a table of contents to be automatically
    generated at the beginning of the generated document.

*   Unicode characters (encoded in UTF-8) are allowed and will be passed
    through as is to the output file.

Copyright and License
---------------------

%CopyrightBegin%

Copyright Ericsson AB 2010-2013. All Rights Reserved.

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



   [$ERL_TOP/HOWTO/INSTALL.md]: INSTALL.md
   [github]: http://github.com
   [our github repository]: http://github.com/erlang/otp


   [?TOC]: true
