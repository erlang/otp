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
# SNMP Appendix C

## Appendix C

### Compile time configuration

There is one compile/configure time option: Defining the size of an "empty" PDU.
This is used when processing get-bulk requests. The default value for this is
_21_, but can be _increased_ in two ways:

- configure: `--with-snmp-empty-pdu-size=SIZE`
- compile time: `environment variable: SNMP_EMPTY_PDU_SIZE=SIZE"`

Where `SIZE` is a value greater or equal to 21.
