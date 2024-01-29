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
# Header Tags

Each document begins with a header part, which looks the same for all DTDs. Here
the title of the document is specified, as well as administrative data like who
is responsible for the document, which version is it, when was it last changed
and such.

An full header looks like:

```text
<header>
  <copyright>...</copyright>
  <legalnotice>...</legalnotice>
  <title>...</title>
  <prepared>...</prepared>
  <responsible>...</responsible>
  <docno>...</docno>
  <approved>...</approved>
  <checked>...</checked>
  <date>...</date>
  <rev>...</rev>
  <file>...</file>
</header>
```

[](){: #headerTAG }

## <header>

Top level tag for the header part.

[](){: #copyrightTAG }

## <copyright>

The `copyright` element holds information about date(s) and holder(s) of a
document copyright. The `copyright` element is optional. The `copyright` element
has an inner structure containing one or more `year` elements followed by zero
of more `holder` elements.  
See example below:

```c
    <copyright>
      <year>1997</year>
      <year>2007</year>
      <holder>Ericsson AB</holder>
    </copyright>
```

[](){: #legalnoticeTAG }

## <legalnotice>

The `legalnotice` element is used to express copyright, trademark, license, and
other legal formalities of a document. The element contains only PCDATA in the
same manner as `code` and `pre`.

[](){: #titleTAG }

## <title>

For `part` and `application` documents, this will be the title of the document,
visible in the left frame and on the front page.

For `chapter` documents, this will be the chapter name.

For reference manual documents, this tag is ignored.

## <shorttitle>

This optional tag is ignored. It will likely be removed in the future.

[](){: #preparedTAG }

## <prepared>

This tag is intended for administrative use and is ignored.

[](){: #responsibleTAG }

## <responsible>

This optional tag is intended for administrative use and is ignored.

[](){: #docnoTAG }

## <docno>

Document number.

For `part` and `application` documents, the document number is visible in the
left frame and on the front page.

For other types of documents, this tag is ignored.

[](){: #approvedTAG }

## <approved>

This optional tag is intended for administrative use and is ignored.

[](){: #checkedTAG }

## <checked>

This optional tag is intended for administrative use and is ignored.

[](){: #dateTAG }

## <date>

This tag is intended for administrative use and is ignored.

[](){: #revTAG }

## <rev>

Document version.

For `part` and `application` documents, the document version is visible in the
left frame and on the front page.

For other types of documents, this tag is ignored.

[](){: #fileTAG }

## <file>

This optional tag is intended for administrative use and is ignored.
