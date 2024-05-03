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
# SNMP Appendix A

## Appendix A

This appendix describes the conversion of SNMPv2 to SNMPv1 error messages. The
instrumentation functions should return v2 error messages.

Mapping of SNMPv2 error message to SNMPv1:

| SNMPv2 message      | SNMPv1 message |
| ------------------- | -------------- |
| noError             | noError        |
| genErr              | genErr         |
| noAccess            | noSuchName     |
| wrongType           | badValue       |
| wrongLength         | badValue       |
| wrongEncoding       | badValue       |
| wrongValue          | badValue       |
| noCreation          | noSuchName     |
| inconsistentValue   | badValue       |
| resourceUnavailable | genErr         |
| commitFailed        | genErr         |
| undoFailed          | genErr         |
| notWritable         | noSuchName     |
| inconsistentName    | noSuchName     |

_Table: Error Messages_
