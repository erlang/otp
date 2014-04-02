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

*   We prefer to receive proposed updates via email on the
    [`erlang-patches`] [3] mailing list or through a pull request.

*   Pull requests will be handled once everyday and there will be 
    essential testing before we will take a decision on the outcome
    of the request. If the essential testings fails, the pull request
    will be closed and you will have to fix the problem and submit another
    pull request when this is done.

*   We merge all proposed updates to the `pu` (*proposed updates*) branch,
    typically within one working day.

*   At least once a day, the contents of the `pu` branch will be built on
    several platforms (Linux, Solaris, Mac OS X, Windows, and so on) and
    automatic test suites will be run. We will email you if any problems are
    found.

*   If a proposed change builds and passes the tests, it will be reviewed
    by one or more members of the Erlang/OTP team at Ericsson. The reviewer
    may suggest improvements that are needed before the change can be accepted
    and merged.

*   Once or twice a week, a status email called ["What's cooking in Erlang/OTP"] [4]
    will be sent to the [`erlang-patches`] [3] mailing list.

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Ericsson AB 2010-2014. All Rights Reserved.
>
> The contents of this file are subject to the Erlang Public License,
> Version 1.1, (the "License"); you may not use this file except in
> compliance with the License. You should have received a copy of the
> Erlang Public License along with this software. If not, it can be
> retrieved online at http://www.erlang.org/.
>
> Software distributed under the License is distributed on an "AS IS"
> basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
> the License for the specific language governing rights and limitations
> under the License.
>
> %CopyrightEnd%



   [1]: http://www.erlang.org
   [2]: http://wiki.github.com/erlang/otp/submitting-patches
   [3]: http://www.erlang.org/static/doc/mailinglist.html
   [4]: http://erlang.github.com/otp/
   [5]: HOWTO/INSTALL.md
