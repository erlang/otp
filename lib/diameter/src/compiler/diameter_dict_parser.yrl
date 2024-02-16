%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% A grammar for dictionary specification.
%%

Nonterminals
  application_id avp avp_code avp_def avp_defs avp_flags avp_header
  avp_header_tok avp_name avp_names avp_ref avp_spec avp_type
  avp_vendor avps bit bits command_def command_id diameter_name
  dictionary enum_def enum_defs group_def group_defs header header_tok
  ident idents message_defs module qual section sections.

Terminals
  avp_types avp_vendor_id codecs custom_types define enum grouped
  id inherits messages name prefix vendor
  number word
  '{' '}' '<' '>' '[' ']' '*' '::=' ':' ',' '-'
   code
  'answer-message'
  'AVP' 'AVP-Header'
  'Diameter' 'Diameter-Header' 'Header'
  'REQ' 'PXY' 'ERR'.

Rootsymbol dictionary.

Endsymbol '$end'.

%% ===========================================================================

dictionary -> sections : '$1'.

sections -> '$empty'         : [].
sections -> section sections : ['$1' | '$2'].

section -> name ident   : ['$1', '$2'].
section -> prefix ident : ['$1', '$2'].
section -> id number    : ['$1', '$2'].
section -> vendor number ident       : ['$1', '$2', '$3'].
section -> inherits module avp_names : ['$1', '$2' | '$3'].
section -> avp_types avp_defs        : ['$1' | '$2'].
section -> avp_vendor_id number avp_names : ['$1', '$2' | '$3'].
section -> custom_types module avp_names  : ['$1', '$2' | '$3'].
section -> codecs module avp_names      : ['$1', '$2' | '$3'].
section -> messages message_defs  : ['$1' | '$2'].
section -> grouped group_defs     : ['$1' | '$2'].
section -> enum ident enum_defs   : ['$1', '$2' | '$3'].
section -> define ident enum_defs : ['$1', '$2' | '$3'].

%% =====================================

module -> ident : '$1'.

avp_names -> idents : '$1'.  %% Note: not 'AVP'

avp_defs -> '$empty'         : [].
avp_defs -> avp_def avp_defs : ['$1' | '$2'].

avp_def -> ident number avp_type avp_flags : ['$1', '$2', '$3', '$4'].

avp_type -> ident : '$1'.

idents -> '$empty'     : [].
idents -> ident idents : ['$1' | '$2'].

avp_flags -> '-'   :
    {_, Lineno} = '$1',
    {word, Lineno, ""}.
avp_flags -> ident :
    '$1'.
%% Could support lowercase here if there's a use for distinguishing
%% between Must and Should in the future in deciding whether or not
%% to set a flag.

ident -> word : '$1'.

%% Don't bother mapping reserved words to make these usable in this
%% context. That an AVP can't be named Diameter-Header is probably no
%% great loss, and that it can't be named AVP may even save someone
%% from themselves. (Temporarily at least.)

group_defs -> '$empty'             : [].
group_defs -> group_def group_defs : ['$1' | '$2'].

message_defs -> '$empty'                 : [].
message_defs -> command_def message_defs : ['$1' | '$2'].

enum_defs -> '$empty'           : [].
enum_defs -> enum_def enum_defs : ['$1' | '$2'].

enum_def -> ident number : ['$1', '$2'].

%% =====================================
%% 3.2.  Command Code ABNF specification
%%
%%    Every Command Code defined MUST include a corresponding ABNF
%%    specification, which is used to define the AVPs that MUST or MAY be
%%    present when sending the message.  The following format is used in
%%    the definition:

%%   command-def      = <command-name> "::=" diameter-message
%%
%%   command-name     = diameter-name
%%
%%   diameter-name    = ALPHA *(ALPHA / DIGIT / "-")
%%
%%   diameter-message = header  [ *fixed] [ *required] [ *optional]

%% answer-message is a special case.
command_def -> 'answer-message' '::=' '<' header_tok ':' code
                                          ',' 'ERR' '[' 'PXY' ']' '>'
               avps
  : ['$1', false | '$13'].

command_def -> diameter_name '::=' header avps
  : ['$1', '$3' | '$4'].
%% Ensure the order fixed/required/optional by semantic checks rather
%% than grammatically since the latter requires more lookahead: don't
%% know until after a leading qual which of the three it is that's
%% being parsed.

diameter_name -> ident : '$1'.

%%   header           = "<" "Diameter Header:" command-id
%%                      [r-bit] [p-bit] [e-bit] [application-id] ">"
%%
%%   command-id       = 1*DIGIT
%%                      ; The Command Code assigned to the command
%%
%%   r-bit            = ", REQ"
%%                      ; If present, the 'R' bit in the Command
%%                      ; Flags is set, indicating that the message
%%                      ; is a request, as opposed to an answer.
%%
%%   p-bit            = ", PXY"
%%                      ; If present, the 'P' bit in the Command
%%                      ; Flags is set, indicating that the message
%%                      ; is proxiable.
%%
%%   e-bit            = ", ERR"
%%                      ; If present, the 'E' bit in the Command
%%                      ; Flags is set, indicating that the answer
%%                      ; message contains a Result-Code AVP in
%%                      ; the "protocol error" class.
%%
%%   application-id   = 1*DIGIT

header -> '<' header_tok ':' command_id bits application_id '>'
  : ['$4', '$5', '$6'].

command_id -> number : '$1'.

%% Accept both the form of the base definition and the typo (fixed in
%% 3588bis) of the grammar.
header_tok -> 'Diameter' 'Header'.
header_tok -> 'Diameter-Header'.

bits -> '$empty'     : [].
bits -> ',' bit bits : ['$2' | '$3'].

%% ERR only makes sense for answer-message so don't allow it here
%% (despite 3588).
bit -> 'REQ' : '$1'.
bit -> 'PXY' : '$1'.

application_id -> '$empty' : false.
application_id -> number   : '$1'.

%%   fixed            = [qual] "<" avp-spec ">"
%%                      ; Defines the fixed position of an AVP
%%
%%   required         = [qual] "{" avp-spec "}"
%%                      ; The AVP MUST be present and can appear
%%                      ; anywhere in the message.
%%
%%   optional         = [qual] "[" avp-name "]"
%%                      ; The avp-name in the 'optional' rule cannot
%%                      ; evaluate to any AVP Name which is included
%%                      ; in a fixed or required rule.  The AVP can
%%                      ; appear anywhere in the message.
%%                      ;
%%                      ; NOTE:  "[" and "]" have a slightly different
%%                      ; meaning than in ABNF (RFC 5234]). These braces
%%                      ; cannot be used to express optional fixed rules
%%                      ; (such as an optional ICV at the end). To do this,
%%                      ; the convention is '0*1fixed'.

avps -> '$empty' : [].
avps -> avp avps : ['$1' | '$2'].

avp -> avp_ref      : [false | '$1'].
avp -> qual avp_ref : ['$1' | '$2'].

avp_ref -> '<' avp_spec '>' : [$<, '$2'].
avp_ref -> '{' avp_name '}' : [${, '$2'].
avp_ref -> '[' avp_name ']' : [$[, '$2'].
%% Note that required can be an avp_name, not just avp_spec. 'AVP'
%% is specified as required by Failed-AVP for example.

%%   qual             = [min] "*" [max]
%%                      ; See ABNF conventions, RFC 5234 Section 4.
%%                      ; The absence of any qualifiers depends on
%%                      ; whether it precedes a fixed, required, or
%%                      ; optional rule. If a fixed or required rule has
%%                      ; no qualifier, then exactly one such AVP MUST
%%                      ; be present.  If an optional rule has no
%%                      ; qualifier, then 0 or 1 such AVP may be
%%                      ; present. If an optional rule has a qualifier,
%%                      ; then the value of min MUST be 0 if present.
%%
%%   min              = 1*DIGIT
%%                      ; The minimum number of times the element may
%%                      ; be present. If absent, the default value is zero
%%                      ; for fixed and optional rules and one for required
%%                      ; rules. The value MUST be at least one for for
%%                      ; required rules.
%%
%%   max              = 1*DIGIT
%%                      ; The maximum number of times the element may
%%                      ; be present. If absent, the default value is
%%                      ; infinity. A value of zero implies the AVP MUST
%%                      ; NOT be present.

qual -> number '*' number : {'$1', '$3'}.
qual -> number '*'        : {'$1', true}.
qual -> '*' number        : {true, '$2'}.
qual -> '*'               : true.

%%   avp-spec         = diameter-name
%%                      ; The avp-spec has to be an AVP Name, defined
%%                      ; in the base or extended Diameter
%%                      ; specifications.

avp_spec -> diameter_name : '$1'.

%%   avp-name         = avp-spec / "AVP"
%%                      ; The string "AVP" stands for *any* arbitrary AVP
%%                      ; Name, not otherwise listed in that command code
%%                      ; definition. Addition this AVP is recommended for
%%                      ; all command ABNFs to allow for extensibility.

avp_name -> 'AVP'    : '$1'.
avp_name -> avp_spec : '$1'.

%%   The following is a definition of a fictitious command code:
%%
%%   Example-Request ::= < Diameter Header: 9999999, REQ, PXY >
%%                       { User-Name }
%%                     * { Origin-Host }
%%                     * [ AVP ]

%% =====================================
%% 4.4.   Grouped AVP Values
%%
%%    The Diameter protocol allows AVP values of type 'Grouped'.  This
%%    implies that the Data field is actually a sequence of AVPs.  It is
%%    possible to include an AVP with a Grouped type within a Grouped type,
%%    that is, to nest them.  AVPs within an AVP of type Grouped have the
%%    same padding requirements as non-Grouped AVPs, as defined in Section
%%    4.
%%
%%    The AVP Code numbering space of all AVPs included in a Grouped AVP is
%%    the same as for non-grouped AVPs.  Receivers of a Grouped AVP that
%%    does not have the 'M' (mandatory) bit set and one or more of the
%%    encapsulated AVPs within the group has the 'M' (mandatory) bit set
%%    MAY simply be ignored if the Grouped AVP itself is unrecognized.  The
%%    rule applies even if the encapsulated AVP with its 'M' (mandatory)
%%    bit set is further encapsulated within other sub-groups; i.e. other
%%    Grouped AVPs embedded within the Grouped AVP.
%%
%%    Every Grouped AVP defined MUST include a corresponding grammar, using
%%    ABNF [RFC5234] (with modifications), as defined below.

%%          grouped-avp-def  = <name> "::=" avp
%%
%%          name-fmt         = ALPHA *(ALPHA / DIGIT / "-")
%%
%%          name             = name-fmt
%%                             ; The name has to be the name of an AVP,
%%                             ; defined in the base or extended Diameter
%%                             ; specifications.
%%
%%          avp              = header  [ *fixed] [ *required] [ *optional]

group_def -> ident '::=' avp_header avps : ['$1', '$3' | '$4'].

%%          header           = "<" "AVP-Header:" avpcode [vendor] ">"
%%
%%          avpcode          = 1*DIGIT
%%                             ; The AVP Code assigned to the Grouped AVP
%%
%%          vendor           = 1*DIGIT
%%                             ; The Vendor-ID assigned to the Grouped AVP.
%%                             ; If absent, the default value of zero is
%%                             ; used.

avp_header -> '<' avp_header_tok ':' avp_code avp_vendor '>'
  : ['$4', '$5'].

avp_header_tok -> 'AVP-Header'.
avp_header_tok -> 'AVP' 'Header'.

avp_code -> number : '$1'.

avp_vendor -> '$empty' : false.
avp_vendor -> number   : '$1'.
