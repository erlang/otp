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
# Character Entities

## Added Latin 1

The OTP DTD suite uses the same character entities as defined in HTML 3.2
(`ISO 8879-1986//ENTITIES Added Latin 1//EN//HTML`). That is: for an &
(ampersand), use the entity: `&amp;`, for ö use the entity `&ouml;` and so on.

| _Character_ | _Entity_ | _Description_                       |
| ----------- | -------- | ----------------------------------- |
| &           | &amp;    | ampersand                           |
| >           | &gt;     | greater than                        |
| <           | &lt;     | less than                           |
|             | &nbsp;   | no-break space                      |
| ¡           | &iexcl;  | inverted exclamation mark           |
| ¢           | &cent;   | cent sign                           |
| £           | &pound;  | pound sterling sign                 |
| ¤           | &curren; | general currency sign               |
| ¥           | &yen;    | yen sign                            |
| ¦           | &brvbar; | broken (vertical) bar               |
| §           | &sect;   | section sign                        |
| ¨           | &uml;    | umlaut (dieresis)                   |
| ©          | &copy;   | copyright sign                      |
| ª           | &ordf;   | ordinal indicator, feminine         |
| «           | &laquo;  | angle quotation mark, left          |
| ¬           | &not;    | not sign                            |
|             | &shy;    | soft hyphen                         |
| ®          | &reg;    | registered sign                     |
| ¯           | &macr;   | macron                              |
| °           | &deg;    | degree sign                         |
| ±           | &plusmn; | plus-or-minus                       |
| ²           | &sup2;   | superscript two                     |
| ³           | &sup3;   | superscript three                   |
| ´           | &acute;  | acute accent                        |
| µ           | &micro;  | micro sign                          |
| ¶           | &para;   | pilcrow (paragraph sign)            |
| ·           | &middot; | middle dot                          |
| ¸           | &cedil;  | cedilla                             |
| ¹           | &sup1;   | superscript one                     |
| º           | &ordm;   | ordinal indicator, masculine        |
| »           | &raquo;  | angle quotation mark, right         |
| ¼           | &frac14; | fraction one-quarter                |
| ½           | &frac12; | fraction one-half                   |
| ¾           | &frac34; | fraction three-quarters             |
| ¿           | &iquest; | inverted question mark              |
| À           | &Agrave; | capital A, grave accent             |
| Á           | &Aacute; | capital A, acute accent             |
| Â           | &Acirc;  | capital A, circumflex accent        |
| Ã           | &Atilde; | capital A, tilde                    |
| Ä           | &Auml;   | capital A, dieresis or umlaut mark  |
| Å           | &Aring;  | capital A, ring                     |
| Æ           | &AElig;  | capital AE diphthong (ligature)     |
| Ç           | &Ccedil; | capital C, cedilla                  |
| È           | &Egrave; | capital E, grave accent             |
| É           | &Eacute; | capital E, acute accen              |
| Ê           | &Ecirc;  | capital E, circumflex accent        |
| Ë           | &Euml;   | capital E, dieresis or umlaut mark  |
| Ì           | &Igrave; | capital I, grave accent             |
| Í           | &Iacute; | capital I, acute accent             |
| Î           | &Icirc;  | capital I, circumflex accent        |
| Ï           | &Iuml;   | capital I, dieresis or umlaut mark  |
| Ð           | &ETH;    | capital Eth, Icelandic              |
| Ñ           | &Ntilde; | capital N, tilde                    |
| Ò           | &Ograve; | capital O, grave accent             |
| Ó           | &Oacute; | capital O, acute accent             |
| Ô           | &Ocirc;  | capital O, circumflex accent        |
| Õ           | &Otilde; | capital O, tilde                    |
| Ö           | &Ouml;   | capital O, dieresis or umlaut mark  |
| ×           | &times;  | multiply sign                       |
| Ø           | &Oslash; | capital O, slash                    |
| Ù           | &Ugrave; | capital U, grave accent             |
| Ú           | &Uacute; | capital U, acute accent             |
| Û           | &Ucirc;  | capital U, circumflex accent        |
| Ü           | &Uuml;   | capital U, dieresis or umlaut mark  |
| Ý           | &Yacute; | capital Y, acute accent             |
| Þ           | &THORN;  | capital THORN, Icelandic            |
| ß           | &szlig;  | small sharp s, German (sz ligature) |
| à           | &agrave; | small a, grave accent               |
| á           | &aacute; | small a, acute accent               |
| â           | &acirc;  | small a, circumflex accent          |
| ã           | &atilde; | small a, tilde                      |
| ä           | &auml;   | small a, dieresis or umlaut mark    |
| å           | &aring;  | small a, ring                       |
| æ           | &aelig;  | small ae diphthong (ligature)       |
| ç           | &ccedil; | small c, cedilla                    |
| è           | &egrave; | small e, grave accent               |
| é           | &eacute; | small e, acute accent               |
| ê           | &ecirc;  | small e, circumflex accent          |
| ë           | &euml;   | small e, dieresis or umlaut mark    |
| ì           | &igrave; | small i, grave accent               |
| í           | &iacute; | small i, acute accent               |
| î           | &icirc;  | small i, circumflex accent          |
| ï           | &iuml;   | small i, dieresis or umlaut mark    |
| ð           | &eth;    | small eth, Icelandic                |
| ñ           | &ntilde; | small n, tilde                      |
| ò           | &ograve; | small o, grave accent               |
| ó           | &oacute; | small o, acute accent               |
| ô           | &ocirc;  | small o, circumflex accent          |
| õ           | &otilde; | small o, tilde                      |
| ö           | &ouml;   | small o, dieresis or umlaut mark    |
| ÷           | &divide; | divide sign                         |
| ø           | &oslash; | small o, slash                      |
| ù           | &ugrave; | small u, grave accent               |
| ú           | &uacute; | small u, acute accent               |
| û           | &ucirc;  | small u, circumflex accent          |
| ü           | &uuml;   | small u, dieresis or umlaut mark    |
| ý           | &yacute; | small y, acute accent               |
| þ           | &thorn;  | small thorn, Icelandic              |
| ÿ           | &yuml;   | small y, dieresis or umlaut mark    |

_Table: Accented Latin-1 alphabetic characters._
