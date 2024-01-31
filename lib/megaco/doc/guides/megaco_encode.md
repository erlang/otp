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
# Internal form and its encodings

This version of the stack is compliant with:

- Megaco/H.248 version 1 (RFC3525) updated according to Implementors Guide
  version 10-13.
- Megaco/H.248 version 2 as defined by draft-ietf-megaco-h248v2-04 updated
  according to Implementors Guide version 10-13.
- Megaco/H.248 version 3 as defined by ITU H.248.1 (09/2005).

## Internal form of messages

We use the same internal form for both the binary and text encoding. Our
internal form of Megaco/H.248 messages is heavily influenced by the internal
format used by ASN.1 encoders/decoders:

- "SEQUENCE OF" is represented as a list.
- "CHOICE" is represented as a tagged tuple with size 2.
- "SEQUENCE" is represented as a record, defined in
  "megaco/include/megaco_message_v1.hrl".
- "OPTIONAL" is represented as an ordinary field in a record which defaults to
  'asn1_NOVALUE', meaning that the field has no value.
- "OCTET STRING" is represented as a list of unsigned integers.
- "ENUMERATED" is represented as a single atom.
- "BIT STRING" is represented as a list of atoms.
- "BOOLEAN" is represented as the atom 'true' or 'false'.
- "INTEGER" is represented as an integer.
- "IA5String" is represented as a list of integers, where each integer is the
  ASCII value of the corresponding character.
- "NULL" is represented as the atom 'NULL'.

In order to fully understand the internal form you must get hold on a ASN.1
specification for the Megaco/H.248 protocol, and apply the rules above. Please,
see the documentation of the ASN.1 compiler in Erlang/OTP for more details of
the semantics in mapping between ASN.1 and the corresponding internal form.

Observe that the 'TerminationId' record is not used in the internal form. It has
been replaced with a megaco_term_id record (defined in
"megaco/include/megaco.hrl").

## The different encodings

The Megaco/H.248 standard defines both a plain text encoding and a binary
encoding (ASN.1 BER) and we have implemented encoders and decoders for both. We
do in fact supply five different encoding/decoding modules.

In the text encoding, implementors have the choice of using a mix of short and
long keywords. It is also possible to add white spaces to improve readability.
We use the term compact for text messages with the shortest possible keywords
and no optional white spaces, and the term pretty for a well indented text
format using long keywords and an indentation style like the text examples in
the Megaco/H.248 specification).

Here follows an example of a text message to give a feeling of the difference
between the pretty and compact versions of text messages. First the pretty, well
indented version with long keywords:

```c
   MEGACO/1 [124.124.124.222]
   Transaction = 9998 {
           Context = - {
                   ServiceChange = ROOT {
                           Services {
                                   Method = Restart,
                                   ServiceChangeAddress = 55555,
                                   Profile = ResGW/1,
                                   Reason = "901 Cold Boot"
                           }
                   }
           }
   }
```

Then the compact version without indentation and with short keywords:

```text

   !/1 [124.124.124.222]
   T=9998{C=-{SC=ROOT{SV{MT=RS,AD=55555,PF=ResGW/1,RE="901 Cold Boot"}}}}
```

And the programmers view of the same message. First a list of ActionRequest
records are constructed and then it is sent with one of the send functions in
the API:

```erlang
  Prof = #'ServiceChangeProfile'{profileName = "resgw", version = 1},
  Parm = #'ServiceChangeParm'{serviceChangeMethod  = restart,
                              serviceChangeAddress = {portNumber, 55555},
                              serviceChangeReason  = "901 Cold Boot",
                              serviceChangeProfile = Prof},
  Req = #'ServiceChangeRequest'{terminationID = [?megaco_root_termination_id],
                                serviceChangeParms = Parm},
  Actions = [#'ActionRequest'{contextId = ?megaco_null_context_id,
                              commandRequests = {serviceChangeReq, Req}}],
  megaco:call(ConnHandle, Actions, Config).
```

And finally a print-out of the entire internal form:

```erlang
  {'MegacoMessage',
   asn1_NOVALUE,
   {'Message',
    1,
    {ip4Address,{'IP4Address', [124,124,124,222], asn1_NOVALUE}},
    {transactions,
     [
      {transactionRequest,
       {'TransactionRequest',
         9998,
         [{'ActionRequest',
           0,
           asn1_NOVALUE,
           asn1_NOVALUE,
           [
            {'CommandRequest',
             {serviceChangeReq,
              {'ServiceChangeRequest',
               [
                {megaco_term_id, false, ["root"]}],
                {'ServiceChangeParm',
                 restart,
                 {portNumber, 55555},
                 asn1_NOVALUE,
                 {'ServiceChangeProfile', "resgw", version = 1},
                 "901 MG Cold Boot",
                 asn1_NOVALUE,
                 asn1_NOVALUE,
                 asn1_NOVALUE
                }
              }
             },
             asn1_NOVALUE,
             asn1_NOVALUE
            }
           ]
          }
         ]
       }
      }
     ]
    }
   }
  }
```

The following encoding modules are provided:

- megaco_pretty_text_encoder - encodes messages into pretty text format, decodes
  both pretty as well as compact text.
- megaco_compact_text_encoder - encodes messages into compact text format,
  decodes both pretty as well as compact text.
- megaco_binary_encoder - encode/decode ASN.1 BER messages. This encoder
  implements the fastest of the BER encoders/decoders. Recommended binary codec.
- megaco_ber_encoder - encode/decode ASN.1 BER messages.
- megaco_per_encoder - encode/decode ASN.1 PER messages. N.B. that this format
  is not included in the Megaco standard.
- megaco_erl_dist_encoder - encodes messages into Erlangs distribution format.
  It is rather verbose but encoding and decoding is blinding fast. N.B. that
  this format is not included in the Megaco standard.

[](){: #erl_dist_config }

## Configuration of Erlang distribution encoding module

The encoding_config of the megaco_erl_dist_encoder module may be one of these:

- `[]` \- Encodes the messages to the standard distribution format. It is rather
  verbose but encoding and decoding is blinding fast.
- `[megaco_compressed]` \- Encodes the messages to the standard distribution
  format after an internal transformation. It is less verbose, but the total
  time of the encoding and decoding will on the other hand be somewhat slower
  (see the [performance](megaco_performance.md) chapter for more info).
- `[{megaco_compressed, Module}]` \- Works in the same way as the
  megaco_compressed config parameter, only here the user provide their own
  compress module. This module must implement the `m:megaco_edist_compress`
  behaviour.
- `[compressed]` \- Encodes the messages to a compressed form of the standard
  distribution format. It is less verbose, but the encoding and decoding will on
  the other hand be slower.

[](){: #text_config }

## Configuration of text encoding module(s)

When using text encoding(s), there is actually two different configs controlling
what software to use:

- `[]` \- An empty list indicates that the erlang scanner should be used.
- `[{flex, port()}]` \- Use the flex scanner when decoding (not optimized for
  SMP). See [initial configuration](megaco_run.md#initial_config) for more info.
- `[{flex, ports()}]` \- Use the flex scanner when decoding (optimized for SMP).
  See [initial configuration](megaco_run.md#initial_config) for more info.

The Flex scanner is a Megaco scanner written as a linked in driver (in C). There
are two ways to get this working:

- Let the Megaco stack start the flex scanner (load the driver).

  To make this happen the megaco stack has to be configured:

  - Add the `{scanner, flex}` (or similar) directive to an Erlang system config
    file for the megaco app (see
    [initial configuration](megaco_run.md#initial_config) chapter for details).
  - Retrieve the encoding-config using the [system_info](`m:megaco#system_info`)
    function (with `Item = text_config`).
  - Update the receive handle with the encoding-config (the `encoding_config`
    field).

  The benefit of this is that Megaco handles the starting, holding and the
  supervision of the driver and port.

- The Megaco client (user) starts the flex scanner (load the driver).

  When starting the flex scanner a port to the linked in driver is created. This
  port has to be owned by a process. This process must not die. If it does the
  port will also terminate. Therefor:

  - Create a permanent process. Make sure this process is supervised (so that if
    it does die, this will be noticed).
  - Let this process start the flex scanner by calling the
    `megaco_flex_scanner:start/0,1` function.
  - Retrieve the encoding-config and when initiating the
    `megaco_receive_handle`, set the field `encoding_config` accordingly.
  - Pass the `megaco_receive_handle` to the transport module.

[](){: #binary_config }

## Configuration of binary encoding module(s)

When using binary encoding, the structure of the termination id's needs to be
specified.

- `[native]` \- skips the transformation phase, i.e. the decoded message(s) will
  not be transformed into our internal form.
- `[integer()]` \- A list containing the size (the number of bits) of each
  level. Example: `[3,8,5,8]`.
- `t:integer/0` \- Number of one byte (8 bits) levels. N.B. This is currently
  converted into the previous config. Example: `3` (`[8,8,8]`).

[](){: #handling_versions }

## Handling megaco versions

There are two ways to handle the different megaco encoding versions. Either
using _dynamic version detection_ (only valid for for incoming messages) or by
_explicit version_ setting in the connection info.

For incoming messages:

- Dynamic version detection

  Set the protocol version in the megaco_receive_handle to `dynamic` (this is
  the default).  
  This works for those codecs that support partial decode of the version,
  currently _text_, and ber_bin (`megaco_binary_encoder` and
  `megaco_ber_bin_encoder`).  
  This way the decoder will detect which version is used and then use the proper
  decoder.

- Explicit version

  Explicitly set the actual protocol version in the megaco_receive_handle.  
  Start with version 1. When the initial service change has been performed and
  version 2 has been negotiated, upgrade the megaco_receive_handle of the
  transport process (control_pid) to version 2. See
  [megaco_tcp](`megaco_tcp:upgrade_receive_handle/2`) and
  [megaco_udp](`megaco_udp:upgrade_receive_handle/2`).  
  Note that if `udp` is used, the same transport process could be used for
  several connections. This could make upgrading impossible.  
  For codecs that does not support partial decode of the version, currently
  `megaco_ber_encoder` and `megaco_per_encoder`, `dynamic` will revert to
  version 1.

For outgoing messages:

- Update the connection info protocol_version.
- Override protocol version when sending a message by adding the item
  `{protocol_version, integer()}` to the Options. See [call](`m:megaco#call`) or
  [cast](`m:megaco#cast`).  
  Note that this does not effect the messages that are sent autonomously by the
  stack. They use the protocol_version of the connection info.

## Encoder callback functions

The encoder callback interface is defined by the `megaco_encoder` behaviour, see
`m:megaco_encoder`.
