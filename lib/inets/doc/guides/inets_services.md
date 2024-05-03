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
# Inets

## Service Concept

Each client and server in `Inets` is viewed as a service. Services can be
configured to be started at application startup or dynamically in runtime. To
run `Inets` as a distributed application that handles application failover and
takeover, configure the services to be started at application startup. When
starting the `Inets` application, the `Inets` top supervisor starts a number of
subsupervisors and worker processes for handling the provided services. When
starting services dynamically, new children are added to the supervision tree,
unless the service is started with the standalone option. In this case the
service is linked to the calling process and all OTP application features, such
as soft upgrade, are lost.

Services to be configured for startup at application startup are to be put into
the Erlang node configuration file on the following form:

```erlang
      [{inets, [{services, ListofConfiguredServices}]}].
```

For details of what to put in the list of configured services, see the
documentation for the services to be configured.
