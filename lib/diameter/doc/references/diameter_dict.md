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
# diameter_dict

Dictionary interface of the diameter application.

## Description

A diameter service, as configured with `diameter:start_service/2`, specifies one
or more supported Diameter applications. Each Diameter application specifies a
dictionary module that knows how to encode and decode its messages and AVPs. The
dictionary module is in turn generated from a file that defines these messages
and AVPs. The format of such a file is defined in
[FILE FORMAT](diameter_dict.md#FILE_FORMAT) below. Users add support for their
specific applications by creating dictionary files, compiling them to Erlang
modules using either [diameterc(1)](diameterc_cmd.md) or `m:diameter_make` and
configuring the resulting dictionaries modules on a service.

Dictionary module generation also results in a hrl file that defines records for
the messages and Grouped AVPs defined by the dictionary, these records being
what a user of the diameter application sends and receives, modulo other
possible formats as discussed in `m:diameter_app`. These records and the
underlying Erlang data types corresponding to Diameter data formats are
discussed in [MESSAGE RECORDS](diameter_dict.md#MESSAGE_RECORDS) and
[DATA TYPES](diameter_dict.md#DATA_TYPES) respectively. The generated hrl also
contains macro definitions for the possible values of AVPs of type Enumerated.

The diameter application includes five dictionary modules corresponding to
applications defined in section 2.4 of RFC 6733: `diameter_gen_base_rfc3588` and
`diameter_gen_base_rfc6733` for the Diameter Common Messages application with
application identifier 0, `diameter_gen_accounting` (for RFC 3588) and
`diameter_gen_acct_rfc6733` for the Diameter Base Accounting application with
application identifier 3 and `diameter_gen_relay` the Relay application with
application identifier 0xFFFFFFFF.

The Common Message and Relay applications are the only applications that
diameter itself has any specific knowledge of. The Common Message application is
used for messages that diameter itself handles: CER/CEA, DWR/DWA and DPR/DPA.
The Relay application is given special treatment with regard to encode/decode
since the messages and AVPs it handles are not specifically defined.

[](){: #FILE_FORMAT }

## FILE FORMAT

A dictionary file consists of distinct sections. Each section starts with a tag
followed by zero or more arguments and ends at the the start of the next section
or end of file. Tags consist of an ampersand character followed by a keyword and
are separated from their arguments by whitespace. Whitespace separates
individual tokens but is otherwise insignificant.

The tags, their arguments and the contents of each corresponding section are as
follows. Each section can occur multiple times unless otherwise specified. The
order in which sections are specified is unimportant.

- **`@id Number`{: #id }** - Defines the integer Number as the Diameter
  Application Id of the application in question. Can occur at most once and is
  required if the dictionary defines `@messages`. The section has empty content.

  The Application Id is set in the Diameter Header of outgoing messages of the
  application, and the value in the header of an incoming message is used to
  identify the relevant dictionary module.

  Example:

  ```text
  @id 16777231
  ```

- **`@name Mod`{: #name }** - Defines the name of the generated dictionary
  module. Can occur at most once and defaults to the name of the dictionary file
  minus any extension. The section has empty content.

  Note that a dictionary module should have a unique name so as not collide with
  existing modules in the system.

  Example:

  ```text
  @name etsi_e2
  ```

- **`@prefix Name`{: #prefix }** - Defines Name as the prefix to be added to
  record and constant names (followed by a `'_'` character) in the generated
  dictionary module and hrl. Can occur at most once. The section has empty
  content.

  A prefix is optional but can be be used to disambiguate between record and
  constant names resulting from similarly named messages and AVPs in different
  Diameter applications.

  Example:

  ```text
  @prefix etsi_e2
  ```

- **`@vendor Number Name`{: #vendor }** - Defines the integer Number as the the
  default Vendor-Id of AVPs for which the V flag is set. Name documents the
  owner of the application but is otherwise unused. Can occur at most once and
  is required if an AVP sets the V flag and is not otherwise assigned a
  Vendor-Id. The section has empty content.

  Example:

  ```text
  @vendor 13019 ETSI
  ```

- **`@avp_vendor_id Number`{: #avp_vendor_id }** - Defines the integer Number as
  the Vendor-Id of the AVPs listed in the section content, overriding the
  `@vendor` default. The section content consists of AVP names.

  Example:

  ```text
  @avp_vendor_id 2937

  WWW-Auth
  Domain-Index
  Region-Set
  ```

- **`@inherits Mod`{: #inherits }** - Defines the name of a dictionary module
  containing AVP definitions that should be imported into the current
  dictionary. The section content consists of the names of those AVPs whose
  definitions should be imported from the dictionary, an empty list causing all
  to be imported. Any listed AVPs must not be defined in the current dictionary
  and it is an error to inherit the same AVP from more than one dictionary.

  Note that an inherited AVP that sets the V flag takes its Vendor-Id from
  either `@avp_vendor_id` in the inheriting dictionary or `@vendor` in the
  inherited dictionary. In particular, `@avp_vendor_id` in the inherited
  dictionary is ignored. Inheriting from a dictionary that specifies the
  required `@vendor` is equivalent to using `@avp_vendor_id` with a copy of the
  dictionary's definitions but the former makes for easier reuse.

  All dictionaries should typically inherit RFC 6733 AVPs from
  `diameter_gen_base_rfc6733`.

  Example:

  ```text
  @inherits diameter_gen_base_rfc6733
  ```

- **`@avp_types`{: #avp_types }** - Defines the name, code, type and flags of
  individual AVPs. The section consists of definitions of the form

  `Name Code Type Flags`

  where Code is the integer AVP code, Type identifies an AVP Data Format as
  defined in section [DATA TYPES](diameter_dict.md#DATA_TYPES) below, and Flags
  is a string of V, M and P characters indicating the flags to be set on an
  outgoing AVP or a single `'-'` (minus) character if none are to be set.

  Example:

  ```text
  @avp_types

  Location-Information   350  Grouped     MV
  Requested-Information  353  Enumerated   V
  ```

  > #### Warning {: .warning }
  >
  > The P flag has been deprecated by RFC 6733.

- **`@custom_types Mod`{: #custom_types }** - Specifies AVPs for which module
  Mod provides encode/decode functions. The section contents consists of AVP
  names. For each such name, `Mod:Name(encode|decode, Type, Data, Opts)` is
  expected to provide encode/decode for values of the AVP, where Name is the
  name of the AVP, Type is it's type as declared in the `@avp_types` section of
  the dictionary, Data is the value to encode/decode, and Opts is a term that is
  passed through encode/decode.

  Example:

  ```text
  @custom_types rfc4005_avps

  Framed-IP-Address
  ```

- **`@codecs Mod`{: #codecs }** - Like `@custom_types` but requires the
  specified module to export `Mod:Type(encode|decode, Name, Data, Opts)` rather
  than `Mod:Name(encode|decode, Type, Data, Opts)`.

  Example:

  ```text
  @codecs rfc4005_avps

  Framed-IP-Address
  ```

- **`@messages`{: #messages }** - Defines the messages of the application. The
  section content consists of definitions of the form specified in section 3.2
  of RFC 6733, "Command Code Format Specification".

  ```text
  @messages

  RTR ::= < Diameter Header: 287, REQ, PXY >
          < Session-Id >
          { Auth-Application-Id }
          { Auth-Session-State }
          { Origin-Host }
          { Origin-Realm }
          { Destination-Host }
          { SIP-Deregistration-Reason }
          [ Destination-Realm ]
          [ User-Name ]
        * [ SIP-AOR ]
        * [ Proxy-Info ]
        * [ Route-Record ]
        * [ AVP ]

  RTA ::= < Diameter Header: 287, PXY >
          < Session-Id >
          { Auth-Application-Id }
          { Result-Code }
          { Auth-Session-State }
          { Origin-Host }
          { Origin-Realm }
          [ Authorization-Lifetime ]
          [ Auth-Grace-Period ]
          [ Redirect-Host ]
          [ Redirect-Host-Usage ]
          [ Redirect-Max-Cache-Time ]
        * [ Proxy-Info ]
        * [ Route-Record ]
        * [ AVP ]
  ```

- **`@grouped`** - Defines the contents of the AVPs of the
  application having type Grouped. The section content consists of definitions
  of the form specified in section 4.4 of RFC 6733, "Grouped AVP Values".

  Example:

  ```text
  @grouped

  SIP-Deregistration-Reason ::= < AVP Header: 383 >
                                { SIP-Reason-Code }
                                [ SIP-Reason-Info ]
                              * [ AVP ]
  ```

  Specifying a Vendor-Id in the definition of a grouped AVP is equivalent to
  specifying it with `@avp_vendor_id`.

- **`@enum Name`{: #enum }** - Defines values of AVP Name having type
  Enumerated. Section content consists of names and corresponding integer
  values. Integer values can be prefixed with 0x to be interpreted as
  hexadecimal.

  Note that the AVP in question can be defined in an inherited dictionary in
  order to introduce additional values to an enumeration otherwise defined in
  another dictionary.

  Example:

  ```text
  @enum SIP-Reason-Code

  PERMANENT_TERMINATION    0
  NEW_SIP_SERVER_ASSIGNED  1
  SIP_SERVER_CHANGE        2
  REMOVE_SIP_SERVER        3
  ```

- **`@end`{: #end }** - Causes parsing of the dictionary to terminate: any
  remaining content is ignored.

Comments can be included in a dictionary file using semicolon: characters from a
semicolon to end of line are ignored.

[](){: #MESSAGE_RECORDS }

## MESSAGE RECORDS

The hrl generated from a dictionary specification defines records for the
messages and grouped AVPs defined in `@messages` and `@grouped` sections. For
each message or grouped AVP definition, a record is defined whose name is the
message or AVP name, prefixed with any dictionary prefix defined with `@prefix`,
and whose fields are the names of the AVPs contained in the message or grouped
AVP in the order specified in the definition in question. For example, the
grouped AVP

```text
SIP-Deregistration-Reason ::= < AVP Header: 383 >
                              { SIP-Reason-Code }
                              [ SIP-Reason-Info ]
                            * [ AVP ]
```

will result in the following record definition given an empty prefix.

```text
-record('SIP-Deregistration-Reason', {'SIP-Reason-Code',
                                      'SIP-Reason-Info',
                                      'AVP'}).
```

The values encoded in the fields of generated records depends on the type and
number of times the AVP can occur. In particular, an AVP which is specified as
occurring exactly once is encoded as a value of the AVP's type while an AVP with
any other specification is encoded as a list of values of the AVP's type. The
AVP's type is as specified in the AVP definition, the RFC 6733 types being
described below.

[](){: #DATA_TYPES }

## DATA TYPES

The data formats defined in sections 4.2 ("Basic AVP Data Formats") and 4.3
("Derived AVP Data Formats") of RFC 6733 are encoded as values of the types
defined here. Values are passed to `diameter:call/4` in a request record when
sending a request, returned in a resulting answer record and passed to a
[handle_request/3](`c:diameter_app:handle_request/3`) callback upon reception of
an incoming request.

In cases in which there is a choice between string() and binary() types for
OctetString() and derived types, the representation is determined by the value
of [diameter:service_opt()](`m:diameter#service_opt`)
[string_decode](`m:diameter#string_decode`).

_Basic AVP Data Formats_

```erlang
OctetString() = string() | binary()
Integer32()   = -2147483647..2147483647
Integer64()   = -9223372036854775807..9223372036854775807
Unsigned32()  = 0..4294967295
Unsigned64()  = 0..18446744073709551615
Float32()     = '-infinity' | float() | infinity
Float64()     = '-infinity' | float() | infinity
Grouped()     = record()
```

On encode, an OctetString() can be specified as an iolist(), excessively large
floats (in absolute value) are equivalent to `infinity` or `'-infinity'` and
excessively large integers result in encode failure. The records for grouped
AVPs are as discussed in the previous section.

_Derived AVP Data Formats_

[](){: #Address }

```text
Address() = OctetString()
          | tuple()
```

On encode, an OctetString() IPv4 address is parsed in the usual x.x.x.x format
while an IPv6 address is parsed in any of the formats specified by section 2.2
of RFC 2373, "Text Representation of Addresses". An IPv4 tuple() has length 4
and contains values of type 0..255. An IPv6 tuple() has length 8 and contains
values of type 0..65535. The tuple representation is used on decode.

[](){: #Time }

```erlang
Time() = {date(), time()}

where

  date() = {Year, Month, Day}
  time() = {Hour, Minute, Second}

  Year   = integer()
  Month  = 1..12
  Day    = 1..31
  Hour   = 0..23
  Minute = 0..59
  Second = 0..59
```

Additionally, values that can be encoded are limited by way of their encoding as
four octets as required by RFC 6733 with the required extension from RFC 2030.
In particular, only values between `{{1968,1,20},{3,14,8}}` and
`{{2104,2,26},{9,42,23}}` (both inclusive) can be encoded.

[](){: #UTF8String }

```text
UTF8String() = [integer()] | binary()
```

List elements are the UTF-8 encodings of the individual characters in the
string. Invalid codepoints will result in encode/decode failure. On encode, a
UTF8String() can be specified as a binary, or as a nested list of binaries and
codepoints.

[](){: #DiameterIdentity }

```text
DiameterIdentity() = OctetString()
```

A value must have length at least 1.

[](){: #DiameterURI }

```text
DiameterURI() = OctetString()
              | #diameter_URI{type = Type,
                              fqdn = FQDN,
                              port = Port,
                              transport = Transport,
                              protocol  = Protocol}

where

  Type = aaa | aaas
  FQDN = OctetString()
  Port = integer()
  Transport = sctp | tcp
  Protocol  = diameter | radius | 'tacacs+'
```

On encode, fields port, transport and protocol default to 3868, sctp and
diameter respectively. The grammar of an OctetString-valued DiameterURI() is as
specified in section 4.3 of RFC 6733. The record representation is used on
decode.

[](){: #Enumerated }

```text
Enumerated() = Integer32()
```

On encode, values can be specified using the macros defined in a dictionary's
hrl file.

[](){: #IPFilterRule } [](){: #QoSFilterRule }

```erlang
IPFilterRule()  = OctetString()
QoSFilterRule() = OctetString()
```

Values of these types are not currently parsed by diameter.

## SEE ALSO

[diameterc(1)](diameterc_cmd.md), `m:diameter`, `m:diameter_app`,
`m:diameter_codec`, `m:diameter_make`
