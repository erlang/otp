%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%


%%
%% A grammar for the XSD 1.0 regular expression used by the YANG
%% pattern statement:
%%
%%   https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#regexs
%%
%% Produces an equivalent re(3) regular expression as an iolist(),
%% mapping constructs as required.
%%
%% The 1.0 grammar is ambiguous in several places, rules being stated
%% in text to try an remove the ambiguities. There are still problems
%% however, which are noted in comments below. In particular, { as a
%% regular character (Char) means that arbitrary lookahead is required
%% to decide of it should be interpreted as a qualification or not;
%% for example, 0{12} vs 0{12. The 1.1 specification recognizes this
%% and require both { and } to be escaped, which is the solution
%% adopted here: it's not clear if this is just a blunder in the 1.0
%% spec, but probably since {} are listed as metacharacters that
%% require escape to be interpreted as a normal character and no
%% mention is made of the issue.
%%

Header "%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the \"License\");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an \"AS IS\" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%".

Nonterminals
  %% from the 1.0 grammar:
  regExp branch piece atom
  quantifier quantity 'QuantExact'
  charClass 'Char' 'WildcardEsc'
  charClassExpr charGroup
  'XmlChar' 'SingleCharEsc' 'MultiCharEsc' charClassEsc catEsc
  charProp
  %% adaptations for yecc and disambiguation
  'quantifier?' 'quantity?' 'quantmax?'
  'branch*' 'digit*' 'subtract?' group
  'group*' char
  block 'block*' blockchar.

Terminals
  '.' '?' '*' '+' '(' ')' '|' '[' ']'  %% not a Normal Character
  '{' '}' ',' '-'  %% Quantifier
  '^' '$' ':'
  digit single multi property other.

Rootsymbol regExp.

Endsymbol eof.

%% ===========================================================================
%% This is the XSD grammar with some modifications to avoid lookahead
%% and otherwise adapt to yecc. The original XSD productions are
%% provided as comments, followed by their yecc implementation.

%% [1]     regExp     ::=      branch ( '|' branch )*

%% XSD regular expressions are implicitly anchored at both ends.
regExp -> branch 'branch*' : ['$1' | '$2'].

'branch*' -> '$empty'   : [].
'branch*' -> '|' regExp : [$| | '$2'].

%% [2]     branch     ::=      piece*

branch -> '$empty' : [].
branch -> piece branch : ['$1', '$2'].

%% [3]     piece      ::=      atom quantifier?

piece -> atom 'quantifier?' : ['$1', '$2'].
%% This is ambiguous since { can both be the start of a qualifier and
%% and atom in its own right (through Char). For example, 0{1} is both
%% a sequence of four atoms and one atom with a qualifier. Also,
%% 0{12345 is a sequence of 7 atoms, so even if { as a qualifier takes
%% precedence, this can require an unlimited amount of lookahead to
%% decide that { isn't the start of a qualifier.
%%
%% This can't be intentional, and the grammar has changed in XSD 1.1
%% to remove the ambiguity by disallowing {} in Char (renamed
%% NormalChar). This is adopted here as the only reasonable solution.

'quantifier?' -> '$empty' : [].
'quantifier?' -> quantifier : '$1'.

%% [4]     quantifier     ::=      [?*+] | ( '{' quantity '}' )
%% [5]     quantity       ::=      quantRange | quantMin | QuantExact
%% [6]     quantRange     ::=      QuantExact ',' QuantExact
%% [7]     quantMin       ::=      QuantExact ','
%% [8]     QuantExact     ::=      [0-9]+

quantifier -> '?' : "?".
quantifier -> '*' : "*".
quantifier -> '+' : "+".
quantifier -> '{' quantity '}' : [${, '$2', $}].

quantity -> 'QuantExact' 'quantity?' : ['$1' | '$2'].

'quantity?' -> '$empty' : [].
'quantity?' -> ',' 'quantmax?' : [$,, '$2'].

'quantmax?' -> '$empty' : [].
'quantmax?' -> 'QuantExact' : '$1'.

'QuantExact' -> digit 'digit*' : [value('$1') | '$2'].

'digit*' -> '$empty' : [].
'digit*' -> digit 'digit*' : [value('$1') | '$2'].

%% [9]     atom       ::=      Char | charClass | ( '(' regExp ')' )

atom -> 'Char' : '$1'.
atom -> charClass : '$1'.
atom -> '(' regExp ')' : [$(, '$2', $)].

%% [10]    Char       ::=      [^.\?*+()|#x5B#x5D]

'Char' -> other : value('$1').
'Char' -> digit : value('$1').

'Char' -> ',' : ",".
'Char' -> '-' : "-".

%% ^ and $ are not metacharacters in XSD regular expressions.
'Char' -> '^' : "\\^".
'Char' -> '$' : "\\$".

%% Allowing {} as the grammar specifies requires arbitrary lookahead
%% to decide whether { is the start of a quantifier or an atom in its
%% own right, assuming a quantifier takes precedence; for example,
%% 0{12345 vs 0{12345}. The 1.1 grammar recognises this and disallows
%% it, which is the only reasonable solution.
%'Char' -> '{' : "{".
%'Char' -> '}' : "}".

'Char' -> ':' : ":".

%% [11]    charClass ::= charClassEsc | charClassExpr | WildcardEsc

charClass -> 'SingleCharEsc' : '$1'.
charClass -> charClassEsc : '$1'.
charClass -> charClassExpr : '$1'.
charClass -> 'WildcardEsc' : '$1'.

%% [12]    charClassExpr ::= '[' charGroup ']'
%% [13]    charGroup     ::= posCharGroup | negCharGroup | charClassSub
%% [14]    posCharGroup  ::= ( charRange | charClassEsc )+
%% [15]    negCharGroup  ::= '^' posCharGroup
%% [16]    charClassSub  ::= ( posCharGroup | negCharGroup ) '-' charClassExpr

charClassExpr -> '[' charGroup ']' : '$2'.

%% Parse ^- as normal characters (char below) and verify the textual
%% rules on their use at the end of the CharClassExpr since expressing
%% it in grammar seems rife with shift/reduce conflict.
charGroup -> group 'subtract?' : group('$1', '$2').

%% Character Class Subtraction doesn't exist in PRCE. For example,
%% [a-d-[c]] means one of abd in XSD, but one of abcd-[ followed by ]
%% in PCRE. Map to a negative lookahead assertion: (?![c])[a-d]. These
%% nest with the expected semantics.
'subtract?' -> '$empty' : [].
'subtract?' -> charClassExpr : ["(?!", '$1', $)].

group -> char 'group*' : ['$1' | '$2'].

char -> '-' : $-.
char -> '^' : $^.
char -> 'XmlChar' : '$1'.
char -> 'SingleCharEsc' : '$1'.
char -> charClassEsc : {class, '$1'}.

'group*' -> '$empty' : [].
'group*' -> char 'group*' : ['$1' | '$2'].

%% [17]    charRange      ::= seRange | XmlCharIncDash
%% [18]    seRange        ::= charOrEsc '-' charOrEsc
%% [20]    charOrEsc      ::= XmlChar | SingleCharEsc
%% [21]    XmlChar        ::= [^\#x2D#x5B#x5D]
%% [22]    XmlCharIncDash ::= [^\#x5B#x5D]
%%
%% "A single XML character is a ·character range· that identifies the
%%  set of characters containing only itself. All XML characters are
%%  valid character ranges, except as follows:
%%
%%    The [, ], - and \ characters are not valid character ranges;
%%    The ^ character is only valid at the beginning of a ·positive
%%        character group· if it is part of a ·negative character group·
%%    The - character is a valid character range only at the beginning
%%        or end of a ·positive character group.
%%
%% Note: The grammar for ·character range· as given above is
%%       ambiguous, but the second and third bullets above together
%%       remove the ambiguity."

%% The second rule above disallows including ^ in a character group:
%% it can be excluded with [^^], but not included with [^]. (Which the
%% 1.1 specification explicitly notes.)

%% Anything except \[], or ^- to deal with the aforementioned ambiguity.
'XmlChar' -> '.' : ".".
'XmlChar' -> '?' : "?".
'XmlChar' -> '*' : "*".
'XmlChar' -> '+' : "+".
'XmlChar' -> '(' : "(".
'XmlChar' -> ')' : ")".
'XmlChar' -> '|' : "|".
'XmlChar' -> '{' : "{".
'XmlChar' -> '}' : "}".
'XmlChar' -> ',' : ",".
'XmlChar' -> '$' : "$".     %% not a metacharacter in a PCRE range
'XmlChar' -> ':' : "\\:". %% avoid [: :]
'XmlChar' -> digit : value('$1').
'XmlChar' -> other : value('$1').

%% [23]    charClassEsc ::= ( SingleCharEsc | MultiCharEsc | catEsc | complEsc )

%% Move SingleCharEsc up to be able to differentiate between
%% characters classes in a charGroup and as an atom.
%charClassEsc -> 'SingleCharEsc' : '$1'.
charClassEsc -> 'MultiCharEsc' : '$1'.
charClassEsc -> catEsc : '$1'.

%% [24]    SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]

'SingleCharEsc' -> single : escape('$1').

%% [25]    catEsc     ::= '\p{' charProp '}'
%% [26]    complEsc   ::= '\P{' charProp '}'

catEsc -> property '{' charProp '}' : prop(value('$1'), '$3', prop('$3')).

%% [27]    charProp    ::= IsCategory | IsBlock

%% [28]    IsCategory  ::= Letters | Marks | Numbers | Punctuation | Separators | Symbols | Others
%% [29]    Letters     ::= 'L' [ultmo]?
%% [30]    Marks       ::= 'M' [nce]?
%% [31]    Numbers     ::= 'N' [dlo]?
%% [32]    Punctuation ::= 'P' [cdseifo]?
%% [33]    Separators  ::= 'Z' [slp]?
%% [34]    Symbols     ::= 'S' [mcko]?
%% [35]    Others      ::= 'C' [cfon]?

%% [36]    IsBlock     ::= 'Is' [a-zA-Z0-9#x2D]+

charProp -> block : '$1'.

block -> blockchar 'block*' : ['$1' | '$2'].

'block*' -> '$empty' : [].
'block*' -> blockchar 'block*' : ['$1' | '$2'].

blockchar -> other : value('$1').
blockchar -> digit : value('$1').
blockchar -> '-' : $-.

%% [37]    MultiCharEsc ::= '\' [sSiIcCdDwW]

'MultiCharEsc' -> multi : escape('$1').

%% [37a]       WildcardEsc ::= '.'

'WildcardEsc' -> '.' : ".".

Erlang code.
-moduledoc false.

%% value/1

value({_,_,C}) ->
    C;
value({A,_}) ->
    atom_to_list(A).

%% escape/1

escape({_,_,$i}) ->
    ["[:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"];
escape({_,_,$I}) ->
    ["[^:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"];
escape({_,_,$c}) ->
    ["[-.0-9:A-Z_a-z\u00B7\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u037D\u037F-\u1FFF\u200C-\u200D\u203F\u2040\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"];
escape({_,_,$C}) ->
    ["[^-.0-9:A-Z_a-z\u00B7\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u037D\u037F-\u1FFF\u200C-\u200D\u203F\u2040\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"];
escape({_,_,C}) ->
    ["\\", C].

%% group/2
%%
%% The mapping of character groups is slightly complex since neither
%% Character Class Subtraction nor Block Escape exists in PCRE. The
%% former is mapped to a negative lookup assertion, the latter to a
%% range. The range is complemented if need be to deal with negation
%% in either the escape (ie. \P) or the group.
%%
%% Examples:
%%
%%  \P{IsBasicLatin}            ->  [^\U0000-\U+007F]
%%  [a-z\p{IsRunic}]            ->  [a-z\U+16A0-\U+16FF]
%%  [^a-z\p{IsRunic}]           ->  [^a-z\U+0000-\U+169F\U+1700-\U+FFFF]
%%  [0\P{IsBasicLatin}]         ->  [0\U+0080-\U+FFFF]
%%  [a-z\P{IsBasicLatin}-[ei]]  ->  (?!([e]|[i]))[a-z\U+0080-\U+FFFF]
%%
%% (Replace \U+XXXX with actual characters.)

group(Grp, Sub) ->
    [Neg, Pre | Rest] = group(Grp),
    [Sub, $[, Neg, Pre, alt(Rest, Sub /= [], Neg == $^), $]].

%% group/1
%%
%% Extract leading ^ and/or -.

group([$^, C | L])
  when C == $^;
       C == $- ->
    [$^, C | L];

group([$^ | L]) ->
    [$^, [] | L];

group([$- | L]) ->
    [[], $- | L];

group([_|_] = L) ->
    [[], [] | L].

%% alt/3
%%
%% Return a character group in which Block Escapes are inlined. The
%% error reporting isn't great, since it doesn't point at a location
%% in the expression, which would require passing more information
%% through the parse.

%%alt([$^ | _], _, _) ->
%%    fail({invalid_range, $^});

alt([$-, $-], false, _) ->
    fail({invalid_range, "-"});

alt([T, $-], Sub, Neg) ->
    [case T of {class, C} -> class(Neg, C); _ -> T end, [$- || not Sub]];

alt([$-], Sub, _) ->
    [$- || not Sub];

alt([_], true, _) ->
    fail({invalid_range, "["});

alt([$- | _], _, _) ->
    fail({invalid_range, "-"});

alt([{class, C} | Rest], Sub, Neg) ->
    [class(Neg, C) | alt(Rest, Sub, Neg)];

alt([C, $-, D | Rest], Sub, Neg) ->
    [C, $-, D | alt(Rest, Sub, Neg)];

alt([C | Rest], Sub, Neg) ->
    [C | alt(Rest, Sub, Neg)];

alt([] = L, false, _) ->
    L;

alt([], true, _) ->
    fail({invalid_range, $[}).

%% class/2
%%
%% Complement a range of a Block Escape if need be.

class(Neg, [$[, P, A, $-, B, $]])
  when P == "^", Neg;
       P == [], not Neg ->
    [A, $-, B];

class(_, [$[, _, <<A/utf8>>, $-, <<B/utf8>>, $]]) ->
    [[[<<L/utf8>>, $-, <<(A-1)/utf8>>] || L <- [0], L < A],
     [[<<(B+1)/utf8>>, $-, <<H/utf8>>] || H <- [16#FFFF], B < H]];

class(_, Cs) ->
    Cs.

%% property/3

prop(C, B, ok) ->
    ["\\", C, ${, B, $}];

prop(C, _, {Start, End}) ->
    [$[, [$^ || C == $P], <<Start/utf8>>, $-, <<End/utf8>>, $]];

prop(_, Block, false) ->
    fail({unknown_block, Block}).

%% prop/1
%%
%% All of the 1-2 letter properties are supported by PCRE, but the
%% latter also supports Cs, so guard that only XSD properties are
%% parsed.

prop([P])
  when P == $L;
       P == $M;
       P == $N;
       P == $P;
       P == $Z;
       P == $S;
       P == $C ->
    ok;

prop([$L, C])
  when C == $u;
       C == $l;
       C == $t;
       C == $m;
       C == $o ->
    ok;

prop([$M, C])
  when C == $n;
       C == $c;
       C == $e ->
    ok;

prop([$N, C])
  when C == $d;
       C == $l;
       C == $o ->
    ok;

prop([$P, C])
  when C == $c;
       C == $d;
       C == $s;
       C == $e;
       C == $i;
       C == $f;
       C == $o ->
    ok;

prop([$Z, C])
  when C == $s;
       C == $l;
       C == $p ->
    ok;

prop([$S, C])
  when C == $m;
       C == $c;
       C == $k;
       C == $o ->
    ok;

prop([$C, C])
  when C == $c;
       C == $f;
       C == $o;
       C == $n ->
    ok;

prop([$I, $s | Rest]) ->
    block(Rest);

prop(B) ->
    fail({unknown_property, B}).

%% block/1
%%
%% Some of these are supported by PCRE, but many aren't. Map to a
%% character range in each case.

block("BasicLatin") -> {16#0000, 16#007F};
block("Latin-1Supplement") -> {16#0080, 16#00FF};
block("LatinExtended-A") -> {16#0100, 16#017F};
block("LatinExtended-B") -> {16#0180, 16#024F};
block("IPAExtensions") -> {16#0250, 16#02AF};
block("SpacingModifierLetters") -> {16#02B0, 16#02FF};
block("CombiningDiacriticalMarks") -> {16#0300, 16#036F};
block("Greek") -> {16#0370, 16#03FF};
block("Cyrillic") -> {16#0400, 16#04FF};
block("Armenian") -> {16#0530, 16#058F};
block("Hebrew") -> {16#0590, 16#05FF};
block("Arabic") -> {16#0600, 16#06FF};
block("Syriac") -> {16#0700, 16#074F};
block("Thaana") -> {16#0780, 16#07BF};
block("Devanagari") -> {16#0900, 16#097F};
block("Bengali") -> {16#0980, 16#09FF};
block("Gurmukhi") -> {16#0A00, 16#0A7F};
block("Gujarati") -> {16#0A80, 16#0AFF};
block("Oriya") -> {16#0B00, 16#0B7F};
block("Tamil") -> {16#0B80, 16#0BFF};
block("Telugu") -> {16#0C00, 16#0C7F};
block("Kannada") -> {16#0C80, 16#0CFF};
block("Malayalam") -> {16#0D00, 16#0D7F};
block("Sinhala") -> {16#0D80, 16#0DFF};
block("Thai") -> {16#0E00, 16#0E7F};
block("Lao") -> {16#0E80, 16#0EFF};
block("Tibetan") -> {16#0F00, 16#0FFF};
block("Myanmar") -> {16#1000, 16#109F};
block("Georgian") -> {16#10A0, 16#10FF};
block("HangulJamo") -> {16#1100, 16#11FF};
block("Ethiopic") -> {16#1200, 16#137F};
block("Cherokee") -> {16#13A0, 16#13FF};
block("UnifiedCanadianAboriginalSyllabics") -> {16#1400, 16#167F};
block("Ogham") -> {16#1680, 16#169F};
block("Runic") -> {16#16A0, 16#16FF};
block("Khmer") -> {16#1780, 16#17FF};
block("Mongolian") -> {16#1800, 16#18AF};
block("LatinExtendedAdditional") -> {16#1E00, 16#1EFF};
block("GreekExtended") -> {16#1F00, 16#1FFF};
block("GeneralPunctuation") -> {16#2000, 16#206F};
block("SuperscriptsandSubscripts") -> {16#2070, 16#209F};
block("CurrencySymbols") -> {16#20A0, 16#20CF};
block("CombiningMarksforSymbols") -> {16#20D0, 16#20FF};
block("LetterlikeSymbols") -> {16#2100, 16#214F};
block("NumberForms") -> {16#2150, 16#218F};
block("Arrows") -> {16#2190, 16#21FF};
block("MathematicalOperators") -> {16#2200, 16#22FF};
block("MiscellaneousTechnical") -> {16#2300, 16#23FF};
block("ControlPictures") -> {16#2400, 16#243F};
block("OpticalCharacterRecognition") -> {16#2440, 16#245F};
block("EnclosedAlphanumerics") -> {16#2460, 16#24FF};
block("BoxDrawing") -> {16#2500, 16#257F};
block("BlockElements") -> {16#2580, 16#259F};
block("GeometricShapes") -> {16#25A0, 16#25FF};
block("MiscellaneousSymbols") -> {16#2600, 16#26FF};
block("Dingbats") -> {16#2700, 16#27BF};
block("BraillePatterns") -> {16#2800, 16#28FF};
block("CJKRadicalsSupplement") -> {16#2E80, 16#2EFF};
block("KangxiRadicals") -> {16#2F00, 16#2FDF};
block("IdeographicDescriptionCharacters") -> {16#2FF0, 16#2FFF};
block("CJKSymbolsandPunctuation") -> {16#3000, 16#303F};
block("Hiragana") -> {16#3040, 16#309F};
block("Katakana") -> {16#30A0, 16#30FF};
block("Bopomofo") -> {16#3100, 16#312F};
block("HangulCompatibilityJamo") -> {16#3130, 16#318F};
block("Kanbun") -> {16#3190, 16#319F};
block("BopomofoExtended") -> {16#31A0, 16#31BF};
block("EnclosedCJKLettersandMonths") -> {16#3200, 16#32FF};
block("CJKCompatibility") -> {16#3300, 16#33FF};
block("CJKUnifiedIdeographsExtensionA") -> {16#3400, 16#4DB5};
block("CJKUnifiedIdeographs") -> {16#4E00, 16#9FFF};
block("YiSyllables") -> {16#A000, 16#A48F};
block("YiRadicals") -> {16#A490, 16#A4CF};
block("HangulSyllables") -> {16#AC00, 16#D7A3};
block("PrivateUse") -> {16#E000, 16#F8FF};
block("CJKCompatibilityIdeographs") -> {16#F900, 16#FAFF};
block("AlphabeticPresentationForms") -> {16#FB00, 16#FB4F};
block("ArabicPresentationForms-A") -> {16#FB50, 16#FDFF};
block("CombiningHalfMarks") -> {16#FE20, 16#FE2F};
block("CJKCompatibilityForms") -> {16#FE30, 16#FE4F};
block("SmallFormVariants") -> {16#FE50, 16#FE6F};
block("ArabicPresentationForms-B") -> {16#FE70, 16#FEFE};
%block("Specials") -> {16#FEFF, 16#FEFF};
block("HalfwidthandFullwidthForms") -> {16#FF00, 16#FFEF};
%block("Specials") -> {16#FFF0, 16#FFFD};
block("Specials") -> {16#FEFF, 16#FFFD};

block(_) -> false.

%% fail/1

fail(T) ->
    error({?MODULE, T}).
