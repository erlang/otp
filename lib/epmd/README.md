<h1>ErlPMD</h1>

A drop-in replacement for epmd written in Erlang

<pre>
Copyright (c) 2012 - 2015 Peter Lemenkov.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
</pre>

<h1>Introduction</h1>

This is a drop-in replacement for the server-side part of the epmd (Erlang Port
Mapping Daemon). If you need a client-side part I advise you to use nc or epmd
itself.

It stores nodes' data in the ETS storage.

<h1>Installation</h1>
tbd

<h1>Usage</h1>

See [sample script](https://raw.github.com/lemenkov/erlpmd/master/priv/erlpmd.sh) sample script or run erl and type
application:start(erlpmd).

<h1>Official EPMD protocol description</h1>

See [this link](http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html) for further details.
