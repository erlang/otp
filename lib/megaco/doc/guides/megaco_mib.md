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
# Megaco mib

## Intro

The Megaco mib is as of yet not standardized and our implementation is based on
_draft-ietf-megaco-mib-04.txt_. Almost all of the mib cannot easily be
implemented by the megaco application. Instead these things should be
implemented by a user (of the megaco application).

So what part of the mib is implemented? Basically the relevant statistic
counters of the _MedGwyGatewayStatsEntry_.

## Statistics counters

The implementation of the statistic counters is lightweight. I.e. the statistic
counters are handled separately by different entities of the application. For
instance our two transport module(s) (see [megaco_tcp](`megaco_tcp:get_stats/0`) and
[megaco_udp](`megaco_udp:get_stats/0`)) maintain their own counters and the
application engine (see [megaco](`m:megaco#stats`)) maintain its own counters.

This also means that if a user implement their own transport service then it has
to maintain its own statistics.

## Distribution

Each megaco application maintains its own set of counters. So in a large
(distributed) MG/MGC it could be necessary to collect the statistics from
several nodes (each) running the megaco application (only one of them with the
transport).
