<h1>ErlPMD</h1>

A drop-in replacement for epmd written in Erlang

<pre>
Copyright (c) 2012 Peter Lemenkov.

The MIT License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
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
