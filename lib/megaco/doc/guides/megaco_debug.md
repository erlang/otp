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
# Testing and tools

## Tracing

We have instrumented our code in order to enable tracing. Running the
application with tracing deactivated, causes a negligible performance overhead
(an external call to a function which returns an atom). Activation of tracing
does not require any recompilation of the code, since we rely on Erlang/OTP's
built in support for dynamic trace activation. In our case tracing of calls to a
given external function.

Event traces can be viewed in a generic message sequence chart tool, `et`, or as
standard output (events are written to stdio).

See [enable_trace](`m:megaco#enable_trace`),
[disable_trace](`m:megaco#disable_trace`) and [set_trace](`m:megaco#set_trace`)
for more info.

## Measurement and transformation

We have included some simple tool(s) for codec measurement (meas), performance
tests (mstone1 and mstone2) and message transformation.

The tool(s) are located in the example/meas directory.

### Requirement

- Erlang/OTP, version 24.2 or later.
- Version 4.2 or later of _this_ application.
- Version 5.0.17 or later of the _asn1_ application.
- The flex libraries. Without it, the flex powered codecs cannot be used.

### Meas results

The results from the measurement run (meas) is four excel-compatible textfiles:

- decode_time.xls -> Decoding result
- encode_time.xls -> Encoding result
- total_time.xls -> Total (Decoding+encoding) result
- message_size.xls -> Message size

### Instruction

The tool contain four things:

- The transformation module
- The measurement (meas) module(s)
- The mstone (mstone1 and mstone2) module(s)
- The basic message file

#### Message Transformation

The messages used by the different tools are contained in single message package
file (see below for more info). The messages in this file is encoded with just
one codec. During measurement initiation, the messages are read and then
transformed to all codec formats used in the measurement.

The message transformation is done by the transformation module. It is used to
transform a set of messages encoded with one codec into the other base codec's.

#### Measurement(s)

There are two different measurement tools:

- _meas_:

  Used to perform codec measurements. That is, to see what kind of performance
  can be expected by the different codecs provided by the megaco application.

  The measurement is done by iterating over the decode/encode function for
  approx 2 seconds per message and counting the number of decodes/encodes.

  Is best run by modifying the meas.sh.skel skeleton script provided by the
  tool.

  To run it manually do the following:

  ```erlang
          % erl -pa <path-megaco-ebin-dir> -pa <path-to-meas-module-dir>
          Erlang (BEAM) emulator version 5.6 [source]

          Eshell V12.2  (abort with ^G)
          1> megaco_codec_meas:start().
          ...
          2> halt().
  ```

  or to make it even easier, assuming a measure shall be done on all the codecs
  (as above):

  ```text
          % erl -noshell -pa <path-megaco-ebin-dir> \\
                -pa <path-to-meas-module-dir> \\
                -s megaco_codec_meas -s init stop
  ```

  When run as above (this will take some time), the measurement process is done
  as follows:

  ```text
  For each codec:
      For each message:
          Read the message from the file
              Detect message version
              Measure decode
                  Measure encode
            Write results, encode, decode and total, to file
  ```

- _mstone1 and mstone2_:

  These are two different SMP performance monitoring tool(s).

  _mstone1_ creates a process for each codec config supported by the megaco
  application and let them run for a specific time (all at the same time),
  encoding and decoding megaco messages. The number of messages processed in
  total is the mstone1(1) value.

  There are different ways to run the mstone1 tool, e.g. with or without the use
  of drivers, with _only_ flex-empowered configs.

  Is best run by modifying the mstone1.sh.skel skeleton script provided by the
  tool.

  The _mstone2_ is similar to the _mstone1_ tool, but in this case, each created
  process makes only _one_ run through the messages and then exits. A soon as a
  process exits, a new process (with the same config and messages) is created to
  takes its place. The number of messages processed in total is the mstone2(1)
  value.

Both these tools use the message package (time_test.msgs) provided with the
tool(s), although it can run on any message package as long as it has the same
structure.

#### Message package file

This is simply an erlang compatible text-file with the following structure:
`{codec_name(), messages_list()}`.

```erlang
codec_name() = pretty | compact | ber | per | erlang      (how the messages are encoded)
messages_list() = [{message_name(), message()}]
message_name() = atom()
message() = binary()
```

The codec name is the name of the codec with which all messages in the
`message_list()` has been encoded.

This file can be `exported` to a file structure by calling the
[export_messages](`m:megaco_codec_transform#export_messages`) function. This can
be usefull if a measurement shall be done with an external tool. Exporting the
messages creates a directory tree with the following structure:

```text
<message package>/pretty/<message-files>
                  compact/
                  per/
                  ber/<message-files>
                  erlang/
```

The file includes both version 1, 2 and version 3 messages.

### Notes

#### Binary codecs

There are two basic ways to use the binary encodings: With package related name
and termination id transformation (the 'native' encoding config) or without.
This transformation converts package related names and termination id's to a
more convenient internal form (equivalent with the decoded text message).

The transformation is done \_after\_ the actual decode has been done.

Therefor in the tests, binary codecs are tested with two different encoding
configs to determine exactly how the different options effect the performance:
with transformation ([]) and without transformation (\[native]).

#### Included test messages

Some of these messages are ripped from the call flow examples in an old version
of the RFC and others are created to test a specific feature of megaco.

#### Measurement tool directory name

Be sure _not_ no name the directory containing the measurement binaries starting
with 'megaco-', e.g. megaco-meas. This will confuse the erlang application
loader (erlang applications are named, e.g. megaco-5.2).
