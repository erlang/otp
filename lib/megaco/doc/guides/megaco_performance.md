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
# Performance comparison

## Comparison of encoder/decoders

The Megaco/H.248 standard defines both a plain text encoding and a binary
encoding (ASN.1 BER) and we have implemented encoders and decoders for both. We
do supply a bunch of different encoding/decoding modules and the user may in
fact implement their own (like our erl_dist module). Using a non-standard
encoding format has its obvious drawbacks, but may be useful in some
configurations.

We have made four different measurements of our Erlang/OTP implementation of the
Megaco/H.248 protocol stack, in order to compare our different
encoders/decoders. The result of each one is summarized in the table below.

| _Codec and config_                       | _Size_ | _Encode_ | _Decode_ | _Total_ |
| ---------------------------------------- | ------ | -------- | -------- | ------- |
| pretty                                   | 336    | 5        | 12       | 17      |
| pretty \[flex]                           | 336    | 5        | 11       | 16      |
| compact                                  | 181    | 4        | 10       | 14      |
| compact \[flex]                          | 181    | 4        | 9        | 13      |
| per bin                                  | 91     | 6        | 6        | 12      |
| per bin \[native]                        | 91     | 4        | 3        | 7       |
| ber bin                                  | 165    | 6        | 6        | 12      |
| ber bin \[native]                        | 165    | 4        | 3        | 7       |
| erl_dist                                 | 875    | 2        | 5        | 7       |
| erl_dist \[megaco_compressed]            | 405    | 1        | 2        | 3       |
| erl_dist \[compressed]                   | 345    | 15       | 9        | 24      |
| erl_dist \[megaco_compressed,compressed] | 200    | 11       | 4        | 15      |

_Table: Codec performance_

## Description of encoders/decoders

In Appendix A of the Megaco/H.248 specification (RFC 3525), there are about 30
messages that shows a representative call flow. We have also added a few extra
version 1, version 2 and version 3 messages. We have used these messages as
basis for our measurements. Our figures have not been weighted in regard to how
frequent the different kinds of messages that are sent between the media gateway
and its controller.

The test compares the following encoder/decoders:

- _pretty_ \- pretty printed text. In the text encoding, the protocol stack
  implementors have the choice of using a mix of short and long keywords. It is
  also possible to add white spaces to improve readability. The pretty text
  encoding utilizes long keywords and an indentation style like the text
  examples in the Megaco/H.248 specification.
- _compact_ \- the compact text encoding uses the shortest possible keywords and
  no optional white spaces.
- _ber_ \- ASN.1 BER.
- _per_ \- ASN.1 PER. Not standardized as a valid Megaco/H.248 encoding, but
  included for the matter of completeness as its encoding is extremely compact.
- _erl_dist_ \- Erlang's native distribution format. Not standardized as a valid
  Megaco/H.248 encoding, but included as a reference due to its well known
  performance characteristics. Erlang is a dynamically typed language and any
  Erlang data structure may be serialized to the erl_dist format by using
  built-in functions.

The actual encoded messages have been collected in one directory per encoding
type, containing one file per encoded message.

Here follows an example of a text message to give a feeling of the difference
between the pretty and compact versions of text messages. First the pretty
printed, well indented version with long keywords:

```c
MEGACO/1 [124.124.124.222]
  Transaction = 9998 {
    Context = - {
      ServiceChange = ROOT {
        Services {
          Method = Restart,
          ServiceChangeAddress = 55555,
          Profile = ResGW/1,
          Reason = "901 MG Cold Boot"
        }
      }
    }
  }
```

Then the compact text version without indentation and with short keywords:

```text
!/1 [124.124.124.222] T=9998{
  C=-{SC=ROOT{SV{MT=RS,AD=55555,PF=ResGW/1,RE="901 MG Cold Boot"}}}}
```

## Setup

The measurements has been performed on a Dell Precision 5550 Laptop with a
Intel(R) Core(TM) i7-10875H CPU @ 2.30GHz, with 40 GB memory and running Ubuntu
20.04 x86_64, kernel 5.4.0-91-generic. Software versions was open source OTP
24.2 (megaco-4.2).

## Summary

In our measurements we have seen that there are no significant differences in
message sizes between ASN.1 BER and the compact text format. Some care should be
taken when using the pretty text style (which is used in all the examples
included in the protocol specification and preferred during debugging sessions)
since the messages can then be quite large. If the message size really is a
serious issue, our per encoder should be used, as the ASN.1 PER format is much
more compact than all the other alternatives. Its major drawback is that it is
has not been approved as a valid Megaco/H.248 message encoding.

When it comes to pure encode/decode performance, it turns out that:

- our fastest binary encoder (ber) is about equal to our fastest text encoder
  (compact).
- our fastest binary decoder (ber) is about 66% faster than our fastest text
  decoder (compact).

If the pure encode/decode performance really is a serious issue, our erl_dist
encoder could be used, as the encoding/decoding of the erlang distribution
format is much faster than all the other alternatives. Its major drawback is
that it is has not been approved as a valid Megaco/H.248 message encoding.

There is no performance advantage of building (and using) a non-reentrant flex
scanner over a reentrant flex scanner (if flex supports building such a
scanner).

> #### Note {: .info }
>
> Please, observe that these performance figures are related to our
> implementation in Erlang/OTP. Measurements of other implementations using
> other tools and techniques may of course result in other figures.
