Erlang/OTP
==========

**Erlang** is a programming language used to build massively scalable soft
real-time systems with requirements on high availability. Some of its
uses are in telecom, banking, e-commerce, computer telephony and
instant messaging. Erlang's runtime system has built-in support for
concurrency, distribution and fault tolerance.

**OTP** is set of Erlang libraries and design principles providing
middle-ware to develop these systems. It includes its own distributed
database, applications to interface towards other languages, debugging
and release handling tools.

ERTS and BEAM
-------------
**BEAM** is the name of the virtual machine where all Erlang code is executed.
Every compiled Erlang file has the suffix .beam. The virtual machine
is sometimes referred to as the emulator.

**ERTS** is the Erlang Runtime System where the BEAM, kernel and
standard libraries amongst others are included.

More information can be found at [erlang.org] [1].

Building and Installing
-----------------------

Information on building and installing Erlang/OTP can be found
in the [$ERL_TOP/HOWTO/INSTALL.md] [5] document.

Contributing to Erlang/OTP
--------------------------

Here are the [instructions for submitting patches] [2].

In short:

*   Submit your patch by opening a new Pull Request.

*   Go to the JIRA issue tracker at [bugs.erlang.org] [7] to
    see reported issues which you can contribute to.
    Search for issues with the status *Contribution Needed*.


Bug Reports
--------------------------

Please look at the [instructions for submitting bugs reports] [6].


Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Ericsson AB 2010-2014. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
>
> %CopyrightEnd%



   [1]: http://www.erlang.org
   [2]: http://wiki.github.com/erlang/otp/contribution-guidelines
   [3]: http://www.erlang.org/static/doc/mailinglist.html
   [4]: http://erlang.github.com/otp/
   [5]: HOWTO/INSTALL.md
   [6]: https://github.com/erlang/otp/wiki/Bug-reports
   [7]: http://bugs.erlang.org
