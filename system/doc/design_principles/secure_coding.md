<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

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

# Secure Coding Guidelines
[](){: #secure-coding }

This section provides a guideline for writing secure Erlang code, describing
common pitfalls and weaknesses best avoided. From time to time, it will
reference the [Common Weakness Enumeration] (CWE) maintained by [MITRE], as
well as the application security risks maintained by [OWASP].

There are also sections addressing the [Top 40 CWEs], the [OWASP Top 10]
security risks, and the [OWASP API Security Top 10]. Finally, there is a
section with concrete [Secure Coding Rules].

This is a living document and is updated as new best practices are discovered.
It is by no means complete, and contributions to improve it are warmly
welcomed. Those following these guidelines will hopefully avoid the listed
issues, but undiscovered issues remain.

[Common Weakness Enumeration]: https://cwe.mitre.org
[MITRE]: https://mitre.org
[OWASP]: https://owasp.org

## Background

Before going into rules and guidelines, it is important to describe a few
things specific to Erlang that may not be obvious coming from other languages.
We will begin with the threat model used throughout Erlang/OTP itself. Your
applications may have a different model, but it is nevertheless necessary to
understand what Erlang/OTP can be expected to protect against, and what it
cannot be.

Throughout this document we will use the terms _trusted_ and _untrusted_ to
mean _fully trusted_ and _not fully trusted_, respectively. No distinction is
made between _partially trusted_ and _untrusted_ and it is important to
remember that anything that is not _fully and completely trusted_ must be
considered as _untrusted_ as something given to you by a malicious actor.

#### What _is not_ protected against

* All loaded code is assumed to be trusted. There is no built-in sand-boxing
  mechanism for running untrusted Erlang code. Any code loaded and executed
  within the environment has unrestricted access to the system.
  * This means that a malicious BEAM module can do anything, including breaking
    the memory safety protections of the runtime system and crashing the
    virtual machine. This is no different from other languages, where for
    example modifying a Rust executable on disk could also break memory safety.
    
    It is therefore important that the user secures the host system so that an
    attacker cannot modify any files relevant to the program.
  * Excessive resource usage is not prevented by default. Unless safeguards are
    are put in place (for example heap size limitations), a process is free to
    consume enough resources to crash the whole program.
* All nodes connected through Erlang distribution are assumed to be trusted,
  and have unrestricted access to all other connected systems.
  
  Crucially, there is no authentication built into the default distribution
  protocol, merely a ["cookie" mechanism] that prevents the unintentional
  mixing of Erlang clusters on the same network. For secure communication over
  an unsecured network, the distribution should be configured to use
  [TLS with client certificate verification].
* Inappropriate usage of functionality is not guaranteed to crash or produce
  sensible results. The arguments given to many Erlang functions are assumed to
  be trusted, so providing valid-appearing but nonsensical input to a function
  can have it produce valid-appearing garbage instead of crashing or returning
  an error.
  
  Furthermore, using undocumented functionality, whether it is a function,
  option, et cetera, can result in almost anything happening.

[TLS with client certificate verification]: `e:ssl:ssl_distribution.html`
["cookie" mechanism]: `e:system:distributed.html#security`

#### What _is_ protected against

* Erlang is a memory-safe language.
  
  CWE categories related to memory safety (both spatial and temporal), such as
  [`CWE-416`], [CWE-465] or [CWE-1218], cannot occur. This safety extends to
  concurrent operation and race conditions can only affect application logic,
  which is further limited as Erlang only provides message-passing (as opposed
  to memory-sharing) concurrency; a classical data race ([`CWE-362`]) cannot
  occur unless the programmer has explicitly built a shared resource for which
  it can happen. Furthermore, errors relating to the use of uninitialized
  variables cannot occur.
  
  Importing memory-unsafe code through the Foreign Function Interfaces (FFI),
  such as [drivers] and [Native Implemented Functions] (NIFs), may, of course,
  violate this property, but there are no unsafe constructs in the language
  itself.
* Classical memory leaks cannot occur. While data can be explicitly referenced
  for longer than necessary by mistake, memory that is no longer referenced
  will eventually be reclaimed by the garbage collector.
* Erlang processes are isolated and can only affect other entities through the
  use of signals: messages, links, monitors, and so on. A process that crashes
  will do so without any impact on other processes other than that defined by
  the programmer.
  
  Furthermore, the execution of a process cannot block that of another process,
  other than as defined by the programmer such as waiting for a response to a
  message that the other process never sends. The effects of, for example,
  [CWE-835] are thus limited to the offending process and can be recovered from
  through termination.
* The behavior of all Erlang programs is defined for all possible inputs, and
  while an operation may appear partial in that it only returns a result for
  certain arguments and otherwise throws an exception, the latter always leaves
  the program in a well-defined state.
  
  Like with memory safety, this property can be violated if the foreign
  function interfaces are misused.
* Erlang uses arbitrary precision integer arithmetic, and overflow will only
  occur once the numbers reach several megabits in size, and will in any event
  throw an exception instead of wrapping around or otherwise behaving in an
  unsafe manner ([`CWE-190`]).

[Native Implemented Functions]: `e:erts:erl_nif.html`
[drivers]: `e:erts:erl_driver.html`
[CWE-465]: https://cwe.mitre.org/data/definitions/465.html
[CWE-835]: https://cwe.mitre.org/data/definitions/835.html
[CWE-1218]: https://cwe.mitre.org/data/definitions/1218.html

[](){: #h-error-handling }
### Error Handling
[Error Handling]: #h-error-handling

Generally, errors can be split into three categories regardless of the language
used:

1. The errors that we do not expect to handle gracefully, such as our local
   disk storage suddenly becoming unavailable. A sensible way to deal with this
   would be, for example, to raise an alarm.
1. The errors that we expect to handle gracefully, such as a web server
   responding with a 404 error when it cannot find a file.
1. _Program bugs_, such as writing to a closed file.

What we wish to do with these three categories is entirely different, but
unfortunately classical error handling tends to mix the three, either using
return values or exceptions for all of them, which causes issues more often
than not. The third category is especially sinister because of the difficulty
in ensuring that the offending code has not left parts of the program in an
invalid state, but it is nevertheless necessary for a language to be able to
handle it since a broken invariant in an insignificant part of the program
would otherwise tear the entire program down.

The isolation properties given by Erlang processes works together with the idea
of supervision trees to allow generalized recovery from third-category errors
without the programmer having to define each particular case. Whatever the
crashing process worked on is lost, but the program as a whole is spared.

This has far-reaching implications. Because the effects of a crash are so
limited, a cornerstone of idiomatic error handling in Erlang is to abandon
execution once something unexpected occurs, leaving the consequences to the
program's supervision structure instead of ignoring or trying to handle the
condition and continuing execution. If a program encounters an error of the
first or second categories, it can either handle them explicitly, or leave it
to the supervision structure by either pattern-matching on the return value or
leaving exceptions uncaught.

This greatly improves security and reliability, as blindly continuing execution
may not only result in unexpected behavior, but can also become a security
issue as assumptions that are made may no longer be valid.

That is not to say that _expected_ errors should not be handled, but rather
that each component that cannot meaningfully handle all the consequences of an
error on its own should not attempt to do so, but should instead leave it to
the supervision structure. Another way of looking at this is to say that
program execution should be "deny by default" and that it may only continue as
explicitly defined.

Needless to say, a well-designed supervision structure is a prerequisite for
this. To be effective it must adequately model the program's domain such that
local failures are contained locally: for example, in a server a single failed
request should not tear down the entire program, but only the subtree started
through said request. Care should also be taken to handle the errors that are
expected and can be handled locally to avoid spamming the error logs, for
example by responding to the request with an error message (such as "404 File
not found" in a web context).

The importance of this cannot be understated. Security issues are almost by
definition a result of unexpected program behavior, and restricting program
behavior to only that which is expected greatly reduces the surface area for
bugs and security issues.

[](){: #h-memory-management }
### Memory Management
[Memory Management]: #h-memory-management

Erlang is a memory-safe language, with automatic memory management handled by
its runtime system. Each process primarily allocates data on its own heap,
while large binaries can be shared efficiently between processes. Process heaps
undergo individual garbage collection, ensuring that unused memory within a
process is reclaimed promptly.

In addition to process heaps, Erlang provides Erlang Term Storage (ETS), an
in-memory storage system for terms. Memory for ETS entries is allocated
automatically upon insertion and freed when entries are removed. This automatic
memory management effectively eliminates many common memory-related
vulnerabilities typical in low-level languages, such as use-after-free errors.

Erlang's automatic memory management prevents memory leaks by releasing memory
once there are no remaining references. However, developers must still make
sure to drop references to unused data, just as in other programming languages.
Failing to do so can lead to memory retention issues similar to leaks, which
may cause performance degradation or potentially become exploitable
vulnerabilities. Therefore, careful management of data references remains
essential to maintain application reliability and security.

[](){: #h-atom-exhaustion }
### Atom Exhaustion
[Atom Exhaustion]: #h-atom-exhaustion

Data of the type `atom` is very space efficient and performant to use, but it
must be used with care as it is not intended to be used dynamically: the
intended use case is to provide named constants in code. The amount of atoms in
the system is limited (see [system limits] and [`CWE-770`]) and cannot be
reclaimed once created. That is, if you dynamically create a large amount of
atoms, the system might crash.

See [`DSG-003`] for ways to prevent this.

[system limits]: `e:system:system_limits.html`

[](){: #h-native-code }
### Native Code
[Native Code]: #h-native-code

Native code can be dynamically linked into the Erlang runtime system either as
a [driver] or [NIF library]. Drivers and NIF libraries are typically written in
C or C++ and have, if not full access, access to a large part of the runtime
system internals.

Drivers and NIF libraries are, of course, not necessarily unsafe and sometimes
their use is unavoidable, but it is much easier to avoid introducing
vulnerabilities and other problems by writing Erlang code whenever possible.
Thorough care needs to be taken both to follow secure programming guidelines
for the language that the native code is written in as well as the guidelines
for writing drivers and NIF libraries. Poorly written native code can both
introduce vulnerabilities as well as stability problems.

A NIF library is loaded using `erlang:load_nif/2` function while a driver is
loaded using one of the `m:erl_ddll` load functions.

[NIF library]: `e:erts:erl_nif.html`
[driver]: `e:erts:erl_driver.html`

## Application-Specific Guidelines

[](){: #h-crypto-application }
### [`crypto`] Application
[`crypto` Application]: #h-crypto-application
[`crypto`]: `e:crypto:index.html`

[](){: #initializing-crypto }
#### Initializing `crypto`
[Initializing `crypto`]: #initializing-crypto

The `libcrypto` library from OpenSSL will be loaded and configured when the
`m:crypto` module is loaded. During loading of the `m:crypto` module some crypto
application parameters will be read in order to configure the `libcrypto`
library. Such application parameters are only available after the application
has been started using the `application:start/1` functionality. That is, in
order to make sure that the `libcrypto` library is configured as expected, make
sure not to load the `m:crypto` module by any other means than calling
`application:start(crypto)`. Any calls to the `m:crypto` library will load the
module. Make sure not to call any functions in the `m:crypto` module, including
`crypto:start/0` prior to the call to `application:start(crypto)`.

In order to ensure that the FIPS mode is configured correctly in the
`libcrypto` library from OpenSSL, use the [`fips_mode`] application parameter
instead of calling `crypto:enable_fips_mode/1`.

[`fips_mode`]: `e:crypto:crypto_app.html#fips_mode`

[](){: #cryptographically-secure-random-numbers }
#### Cryptographically Secure Random Numbers
[Cryptographically Secure Random Numbers]: #cryptographically-secure-random-numbers

Do not use `crypto:rand_uniform/2` since it uses functionality from the
OpenSSL `libcrypto` library that in old versions of the library does not
produce cryptographically secure random numbers ([CWE-338]). An alternative is
using `rand:uniform/1` together with a cryptographically secure random number
generator like `crypto:rand_seed/0`. Note that `crypto:rand_seed/0` not
only produces a seed but also selects a generator that will be used by
`rand:uniform/1`.

The above mentioned functionality will store a state in the
[process dictionary] of the currently executing process. This state could be
modified to select another generator by other functionality called interleaved
with calls retrieving random numbers. A better solution is to use the
functional API, which will pass the state between calls instead of storing it in
the process dictionary. The functional API are functions in the `m:rand` and
`m:crypto` modules with function names having the suffix `_s`. That is, you
might want to consider using `crypto:rand_seed_s/0`/`rand:uniform_s/2`
instead of `crypto:rand_seed/0`/`rand:uniform/1`.

[process dictionary]: `e:system:ref_man_processes.html#process-dictionary`
[CWE-338]: https://cwe.mitre.org/data/definitions/338.html

[](){: #legacy-crypto-functions }
#### Legacy `crypto` Functions

RSA with PKCS-1 padding is weak and should be avoided: do not use the 
`rsa_pkcs1_padding` option with `crypto:private_encrypt/4`,
`crypto:private_decrypt/4`, `crypto:public_encrypt/4`, or
`crypto:public_decrypt/4`.

For signatures, use `crypto:sign/4`/`crypto:verify/5` instead.

[](){: #h-ssl-application }
### [`ssl`] Application
[`ssl` Application]: #h-ssl-application
[`ssl`]: `e:ssl:index.html`

The [`ssl`] application is secure by default and remains so if the
recommendations in its documentation is followed. One must be especially
careful when enabling legacy functionality, as it weakens security. It should
only be done when absolutely necessary.

However, disabling functionality can be beneficial. If you control both the
server and the client, consider disabling the (now old) TLS 1.2 in favor of
TLS 1.3. The [`cert_keys`](`t:ssl:common_option_cert/0`) option can also be
used to configure more than one possible certificate/key pair for a client or
server, giving good security while allowing interoperability with legacy
systems for a period of time.

[](){: #h-ssh-application }
### [`ssh`] Application
[`ssh` Application]: #h-ssh-application
[`ssh`]: `e:ssh:index.html`

See the [`ssh`] documentation's [hardening chapter](`e:ssh:hardening.html`).

[](){: #h-public_key-application }
### [`public_key`] Application
[`public_key` Application]: #h-public_key-application
[`public_key`]: `e:public_key:index.html`

RSA with PKCS-1 padding is weak and should be avoided: do not use the 
`rsa_pkcs1_padding` option with [`public_key:encrypt_private/2,3`],
[`public_key:decrypt_private/2,3`], [`public_key:encrypt_public/2,3`] or
[`public_key:decrypt_public/2,3`].

For signatures, use `public_key:sign/4`/`public_key:verify/5` instead.

[`public_key:encrypt_private/2,3`]: `public_key:encrypt_private/3`
[`public_key:decrypt_private/2,3`]: `public_key:decrypt_private/3`
[`public_key:encrypt_public/2,3`]: `public_key:encrypt_public/3`
[`public_key:decrypt_public/2,3`]: `public_key:decrypt_public/3`

[](){: #h-xmerl-application }
### [`xmerl`] Application
[`xmerl` Application]: #h-xmerl-application
[`xmerl`]: `e:xmerl:index.html`

The `m:xmerl_scan` module dynamically produces new atoms and is therefore not
suitable for decoding XML data originating untrusted sources.

When parsing untrusted XML, you want to prevent XML entity attacks by disabling
parsing of entities. In the `m:xmerl_sax_parser` you can do that by passing the
[`disallow_entities`](`t:xmerl_sax_parser:options/0`) option.

[](){: #secure-coding-rules }
## Rules
[Secure Coding Rules]: #secure-coding-rules

This section contains some general rules that may be helpful in writing more
secure code. It is important to remember that they are mere advice, and that
relaxing them may be warranted from case to case.

[](){: #rule-priority }
[Rule priority:]: #rule-priority
The rules link to related CWEs and OWASP risks, and also have a rough priority
for the order in which we think the rules should be applied on an existing code
base. The priorities are `Critical`, `High`, `Medium`, and `Recommendation`.
The priority of a rule largely reflects how likely it is for related flaws to
lead to a vulnerability, how severe said vulnerability is, and how easy it is
to fix.

Needless to say, new code should follow all the rules within reason.

### Code Style

[](){: #rule-stl-001 }
#### [`STL-001`] - Be Restrictive
[`STL-001`]: #rule-stl-001

As described in the [Error Handling] section, all unexpected errors should be
left to the supervision structure, and all unexpected _conditions_ should be
considered errors. In brief, this is because encountering something unexpected
means that we have left the _known and tested_ path, and continuing greatly
increases the risk for bugs and security issues.

Erlang code should be written as restrictively as possible, to provoke errors
whenever anything unexpected happens. The idea is to make the third error
category, program bugs, visible as a crash instead of silently continuing.

[Rule priority:] `High`

Related CWEs and OWASP risks: [CWE-252], [CWE-253], [CWE-391], [CWE-392],
[CWE-394], [CWE-396], [`A10:2025`]

[CWE-252]: https://cwe.mitre.org/data/definitions/252.html
[CWE-253]: https://cwe.mitre.org/data/definitions/253.html
[CWE-391]: https://cwe.mitre.org/data/definitions/391.html
[CWE-392]: https://cwe.mitre.org/data/definitions/392.html
[CWE-394]: https://cwe.mitre.org/data/definitions/394.html
[CWE-396]: https://cwe.mitre.org/data/definitions/396.html

```erlang
%% DO
case operation(A, B) of
    true -> C;
    false -> D
end.

%% DO NOT
case operation(A, B) of
    true -> C;
    %% What if operation/2 is extended to also return 'maybe', or someone
    %% misspells 'true' as 'tru'?
    _ -> D
end.

%% DO
ok = file:write(Fd, Data)

%% DO NOT
_ = file:write(Fd, Data)

%% DO
foo([First | Rest]) ->
    [bar(First) | foo(Rest)];
foo([]) ->
    [].

%% DO NOT
foo([First | Rest]) ->
    [bar(First) | foo(Rest)];
foo(_) ->
    [].

%% DO
input_to_atom(<<"foo">>) -> foo;
input_to_atom(<<"bar">>) -> bar;
input_to_atom(<<"quux">>) -> quux.

%% DO NOT, when set of possible atoms is known beforehand
input_to_atom(Text) -> binary_to_existing_atom(Text).

%% DO
try operation(A, B) of
    {ok, X} -> something(X)
catch
    error:specific_error -> error
end.

%% DO NOT
try operation(A, B) of
    {ok, X} -> something(X)
catch
    error:_ -> error
end.

%% PREFER
case my_filter(List0, unchanged) of
    unchanged -> List0;
    {changed, List} -> List
end

%% AVOID
case my_filter(List0, unchanged) of
    unchanged -> List0;
    %% What if a misspelled atom like 'uchanged' is returned?
    List -> List
end

%% PREFER
[op(L) || #my_record{}=L <:- ListOfMyRecord]

%% AVOID, this silently filters out entries that do not match #my_record{}
[op(L) || #my_record{}=L <- ListOfMyRecord]
```

[](){: #rule-stl-002 }
#### [`STL-002`] - Avoid Boolean Blindness
[`STL-002`]: #rule-stl-002

Whenever boolean values have a context, prefer using more descriptive atoms to
express the boolean value, for example `initialized`/`uninitialized` or
`changed`/`unchanged`. This makes it easier to distinguish between different
boolean variables when many of them are used together, especially when matching
in function heads and the like.

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [CWE-628]

[CWE-628]: https://cwe.mitre.org/data/definitions/628.html

```erlang
%% DO
case my_filter(List0, unchanged) of
    unchanged -> List0;
    {changed, List} -> List
end

%% DO NOT
case my_filter(List0, false) of
    false -> List0;
    {true, List} -> List
end
```

[](){: #rule-stl-003 }
#### [`STL-003`] - Use Uppercase Names for Macros
[`STL-003`]: #rule-stl-003

Macros are distinguished by a `?` prefix, so an accidental omission of the
prefix leaves the name there instead of applying the macro. For example,
`function_call(?my_macro, SomeArg)` becomes `function_call(my_macro, SomeArg)`
which is syntactically valid, hiding the error.

Static analysis tools can often find these issues, but a quicker way to find
them is to adopt the convention that all macros should be upper-case. Missing a
`?` will in most cases then lead to an unbound variable error or similar.

[Rule priority:] `Recommendation`

```erlang
%% DO
-define(MY_MACRO, 65535).

%% DO NOT
-define(my_macro, 65535).
```

### Deployment

[](){: #rule-dep-001 }
#### [`DEP-001`] - Do Not Expose Default Erlang Distribution on Untrusted Networks
[`DEP-001`]: #rule-dep-001

The builtin Erlang distribution makes it possible to easily and transparently
communicate between Erlang nodes. By default, communication is performed over
an unencrypted TCP connection with a rudimentary [cookie based authentication]
only present in order to prevent mistakes. This configuration should *only* be
used in a trusted network. In order to communicate between nodes over an
untrusted network, the Erlang distribution protocol should be configured to
[communicate between nodes using TLS].

Note that the Erlang Port Mapper Daemon ([EPMD]) service will respond to
unauthenticated requests, and can by this leak information about what Erlang
nodes exist and what ports they are listening on ([`CWE-668`], [`CWE-200`]). You
are therefore advised to [disable the default EPMD] and implement your own
[EPMD module] using another port lookup scheme and [enable that EPMD module].
The simplest solution, assuming only one Erlang node per IP address, would be
to use a statically assigned port, skip registration of nodes, and just assume
that a node will be listening on that port.

Also note that all nodes admitted into an Erlang cluster must be trusted. Once
a node is connected to the cluster, it gains complete access to the resources
and operations of all other nodes, making node trustworthiness a critical
security consideration.

If the distribution protocol is not required per se, Erlang/OTP includes the
[`ssh`] and [`ssl`] applications, enabling secure communication over untrusted
networks. Depending on the nature of the communication and the remote parties
involved, you may need to explicitly validate and sanitize incoming data to
ensure safety and correctness.

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [`CWE-200`], [`CWE-668`], [`A01:2025`],
[`A02:2025`], [`API2:2023`], [`API6:2023`], [`API8:2023`]

[cookie based authentication]: `e:system:distributed.html#security`
[communicate between nodes using TLS]: `e:ssl:ssl_distribution.html`
[EPMD]: `e:erts:erl_dist_protocol.html#epmd-protocol`
[disable the default EPMD]: `e:erts:erl_cmd.html#start_epmd`
[EPMD module]: `m:erl_epmd`
[enable that EPMD module]: `e:kernel:kernel_app.html#epmd_module`

[](){: #rule-dep-002 }
#### [`DEP-002`] - Build and Install Erlang/OTP Yourself
[`DEP-002`]: #rule-dep-002

Recommendations found elsewhere on the internet on how to build and install
Erlang/OTP are often problematic. While tools like `kerl` and `asdf` or
prebuilt docker images are convenient, they sometimes patch Erlang/OTP or set
`CFLAGS`/`LDFLAGS` themselves, which has caused problems in some cases. It is
better to [build Erlang/OTP] yourself instead of relying on external build
tools or already-built installations from other suppliers.

Setting `CFLAGS` and `LDFLAGS` may cause issues if you are not careful. When
building Erlang/OTP, a lot of different executables as well as shared libraries
will be built. Options set in `CFLAGS` and `LDFLAGS` will be used when
compiling both executables and shared libraries, so you only want to pass flags
that are useful for all executables as well as all shared libraries in that
manner.

Some guides recommend passing the `pie` options (position independent
executables), which *should not* be passed in `CFLAGS` and `LDFLAGS` since it
might conflict with the `pic` (position independent code) options used when
building shared libraries. In order to build position independent executables,
you instead want to pass the `--enable-pie` [`configure`] argument when
configuring the build, which will make sure to use `pie` options where
appropriate.

As of Erlang/OTP 28.0, the `configure` script that you run when building
Erlang/OTP will, on Unix like systems, try to enable most of the C/C++
hardening flags recommended by the Open Source Security Foundation. By setting
the environment variable `V=1` when building, you can see the full command
line for each invocation of the compiler as well as the linker. By inspecting
that output you can see what hardening flags is being used in your build.

Make sure to build Erlang/OTP using an up to date OpenSSL. By pointing out the
OpenSSL installation to use by passing the `--with-ssl=PATH` argument to
[`configure`] when building one can ensure that the build does not build
against another old OpenSSL installation found on the system. By passing the
`--disable-dynamic-ssl-lib` [`configure`] when building, the `libcrypto`
library from OpenSSL will be statically linked into the crypto NIF library
ensuring that the selected version from OpenSSL actually will be used, and not
another version being picked up from the system where Erlang/OTP is being run.
Note that only the `libcrypto` library from OpenSSL will be used and that
vulnerabilities in other parts of OpenSSL do not affect Erlang/OTP. The `ssl`
and `public_key` applications implement most things except for crypto
functionality, which is provided by the `crypto` application, which in turn
uses `libcrypto`.

[Rule priority:] `High`

Related CWEs and OWASP risks: [`A03:2025`]

[build Erlang/OTP]: `e:system:install.html`
[OTP Versions Tree]: https://erlang.org/download/otp_versions_tree.html
[`configure`]:  `e:system:install.html#advanced-configuration-and-build-of-erlang-otp`

[](){: #rule-dep-003 }
#### [`DEP-003`] - Use Actively Maintained Versions of Erlang/OTP
[`DEP-003`]: #rule-dep-003

Make sure to use an actively maintained version of Erlang/OTP. The
[OTP Versions Tree] page contains information about maintained Erlang/OTP
releases as well as CVEs affecting different OTP versions. [VEX documents],
which provide information about potential vulnerabilites, are regularly updated
for currently maintained OTP releases. Patches are announced on the
[`erlang-announce`] mailing list as well as on the [erlang forums] web site.

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [`A03:2025`]

[VEX documents]: https://erlang.org/download/vex/
[`erlang-announce`]: https://erlang.org/pipermail/erlang-announce
[erlang forums]: https://erlangforums.com/

[](){: #rule-dep-004 }
#### [`DEP-004`] - Protect the Code Path
[`DEP-004`]: #rule-dep-004

If a malicious actor can write to a folder in the code path used by the Erlang
VM (either the Erlang code path or the host system's shared library load path),
and they have the capability to coerce the system into loading that code (made
worse by `interactive` mode, see [`DEP-005`]), then it is possible for an
attacker to load malicious code.

To mitigate this, ensure that the folders in the code paths are only writable
by a dedicated user, separate from the user running the VM.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [`CWE-427`], [`A08:2025`]

[](){: #rule-dep-005 }
#### [`DEP-005`] - Avoid `interactive` Mode in Production
[`DEP-005`]: #rule-dep-005

`interactive` mode loads code on demand, making it easy to trigger code
loading. There is generally little reason to use this mode outside of
development, so consider using `embedded` mode instead.

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [`A08:2025`]

[](){: #rule-dep-006 }
#### [`DEP-006`] - Minimize VM Privileges
[`DEP-006`]: #rule-dep-006

The virtual machine should run with the fewest possible privileges your system
needs. It should not have the right to interact with (let alone affect)
anything it does not require for its operation.

The VM should at the very least run under its own dedicated user, and
preferably also be further restricted through `SELinux`/`AppArmor`,
containerization, and similar techniques.

[Rule priority:] `Medium`

### Design

[](){: #rule-dsg-001 }
#### [`DSG-001`] - Encode the Problem Domain in the Supervision Tree
[`DSG-001`]: #rule-dsg-001

The supervision tree is central to [Error Handling] in Erlang, and is what
allows graceful operation in the presence of errors regardless of their cause.

For this to be effective, the supervision structure must reflect the problem
that your program is trying to solve, and in particular the parts that are
isolated from each other in the problem domain should also be isolated as
separate Erlang processes.

At its face, this prevents unrelated flows from affecting others, limiting the
"blast radius" of problems and with it also the security implications (such as
in denial-of-service attacks).

However, one less well-understood benefit is that it moves a great deal of
complexity out of the program's _code_ and into its _overall structure_, where
it is much easier to handle. Error handling is notoriously difficult to get
right as errors are often uncommon by their very nature, and thereby difficult
to test.

When an unexpected condition merely crashes a single isolated process, _and the
program structure rolls back anything that process may have done_, the need for
pervasive error handling throughout the code at large is drastically reduced,
and with it a large source of bugs and security issues.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [CWE-389], [CWE-544], [CWE-653], [`A10:2025`]

[CWE-389]: https://cwe.mitre.org/data/definitions/389.html
[CWE-544]: https://cwe.mitre.org/data/definitions/544.html
[CWE-653]: https://cwe.mitre.org/data/definitions/653.html

[](){: #rule-dsg-002 }
#### [`DSG-002`] - Prefer Letting the User Decide What Warrants an Exception
[`DSG-002`]: #rule-dsg-002

Prefer to design your interfaces so that the user decides whether an error is
exceptional or not, by following the `{ok, Result} | {error, Reason}`
convention. Generally speaking the user of an interface has more context than
the one implementing it, and giving them the freedom to choose through pattern
matching tends to result in clearer code as the handling of raised exceptions
is more difficult to follow.

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [CWE-389], [`A10:2025`]

```erlang
%% PREFER
{ok, C} = some_function(A, B)

%% PREFER
case some_function(A, B) of
    {ok, C} ->
        %% Happy path
        ...;
    {error, Error} ->
        %% Handle it
end

%% AVOID
try some_function(A, B) of
    C -> 
        %% Happy path
        ...
catch
    error:_ ->
        %% Handle it
end
```

[](){: #rule-dsg-003 }
#### [`DSG-003`] - Do Not Abuse Atoms
[`DSG-003`]: #rule-dsg-003

Atoms are designed to provide an easy way to create named constants in code.
Defining them programmatically is not something that we have optimized for, and
may also lead to [Atom Exhaustion].

Specifically, the [`binary_to_atom/1,2`] and [`list_to_atom/1`] functions will
create a new atom if it does not already exist. Doing so without care might
cause the system to crash. If you know that the atom already should exist in
the system, you can use the [`binary_to_existing_atom/1,2`] and
[`list_to_existing_atom/1`] instead. These functions will throw an exception
instead of creating a new atom if the atom does not already exist. Note that
there are valid use-cases for using [`binary_to_atom/1,2`] and
[`list_to_atom/1`], so you cannot blindly replace these calls.

However, before using the `*_to_existing_atom()` functions, consider whether an
explicit conversion is more appropriate. Quite often there are only a few atoms
that are valid in the context the conversion is done, and in those cases it is
better to translate them yourself and reject invalid inputs, as
`*_to_existing_atom()` can return *any* atom that exists in the system, not
just those expected in the context.

There are also a number APIs that create general Erlang terms from data of some
serialized format. You should not use such APIs if the data is not trusted (see
[`DSG-011`]) unless the API also provides some way of preventing creation of
atoms. For example, [`binary_to_term/2`] with the `safe` option will prevent
new atoms from being created. However, note that even if the `safe` option is
used and the data originates from an untrusted source, it still has to be
validated and sanitized, since it can still be harmful to the Erlang
application in other ways.

In general, it is best to avoid using such functions altogether on untrusted
data, even with the `safe` option.

[`binary_to_atom/1,2`]: `erlang:binary_to_atom/2`
[`binary_to_existing_atom/1,2`]: `erlang:binary_to_existing_atom/2`
[`binary_to_term/2`]: `erlang:binary_to_term/2`
[`list_to_atom/1`]: `erlang:list_to_atom/1`
[`list_to_existing_atom/1`]: `erlang:list_to_existing_atom/1`

[Rule priority:] `High`

Related CWEs and OWASP risks: [`CWE-770`], [`API10:2023`]

```erlang
%% DO, AND PREFER (see STL-001)
input_to_atom(<<"foo">>) -> foo;
input_to_atom(<<"bar">>) -> bar;
input_to_atom(<<"quux">>) -> quux.

%% DO
input_to_atom(Text) -> binary_to_existing_atom(Text).

%% DO NOT
input_to_atom(Text) -> binary_to_atom(Text).
```

[](){: #rule-dsg-004 }
#### [`DSG-004`] - Do Not Use Undocumented Functionality
[`DSG-004`]: #rule-dsg-004

Undocumented functions or functionality must **never** be used. This includes
undocumented arguments to documented functions and undocumented system
services. Using such functionality poses a serious security risk. These
functions and features are intended strictly for internal use within Erlang/OTP
and are not supported for external use.

Merely passing the wrong arguments to these functions can cause the system to
behave in unexpected ways from that point on, and their behavior may change or
they may be removed without prior notice.

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [CWE-242], [CWE-477], [CWE-676]

[CWE-242]: https://cwe.mitre.org/data/definitions/242.html
[CWE-477]: https://cwe.mitre.org/data/definitions/477.html
[CWE-676]: https://cwe.mitre.org/data/definitions/676.html

[](){: #rule-dsg-005 }
#### [`DSG-005`] - Do Not Use Deprecated Functionality
[`DSG-005`]: #rule-dsg-005

When functionality is deprecated in Erlang/OTP, the documentation will
typically point to other or new functionality to use instead. The deprecation
may have been made for various reasons, which could include security issues,
but it is not the most frequent reason. However, if the compiler gives you a
deprecation warning it is worth taking a look at the functionality and how it
is used.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [CWE-242], [CWE-477], [CWE-676]

[](){: #rule-dsg-006 }
#### [`DSG-006`] - Do Not Use Unsafe Functionality
[`DSG-006`]: #rule-dsg-006

[Unsafe Functions] must not be used, and [Potentially Unsafe Functions] should
only be used in a safe manner, and are best avoided if possible.

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [CWE-242]

[](){: #rule-dsg-007 }
#### [`DSG-007`] - Know Your Data
[`DSG-007`]: #rule-dsg-007

Always know what data your code is operating on, and make sure it is
documented. Preferably in a way such that it can be checked by automated tools
such as declaring [`-nominal`](`e:system:nominals.html`) types to distinguish
conceptually different but otherwise identical data (consider `20 meters` and
`20 feet`, when both are represented as the number `20`).

Something as simple as marking data as trusted or untrusted can be a great help
since all code implicitly trusts the data it is given to different degrees.
Documenting what data your code expects to be operating on helps protect
against, for example, injection attacks in match specifications (see
[`MSC-005`]), or the leakage of sensitive data (see [`MSC-004`]).

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [`CWE-20`], [`CWE-22`], [`CWE-74`], [`CWE-78`],
[`CWE-89`], [`CWE-502`], [`CWE-532`], [`CWE-843`], [`CWE-918`], [CWE-1287]

[CWE-1287]: https://cwe.mitre.org/data/definitions/1287.html

[](){: #rule-dsg-008 }
#### [`DSG-008`] - Do Not Fight Reality
[`DSG-008`]: #rule-dsg-008

Much of what we do as programmers is to provide abstractions that make life
easier for other people. For the most part this works fine, but it is important
not to design interfaces that promise more than is possible to deliver.

The clearest example of this would be interfaces that hide the fact that they
operate on a distributed system, whether through promising absolute consistency
and availability at all times (a violation of [PACELC]), through omitting a
recovery strategy, or simply just not documenting the trade-offs. This is
especially important to understand as Erlang acts as a distributed system
internally, with message passing between processes instead of over a physical
network. While a solution may very well work fine as long as nothing crashes,
it may [subtly fail] in the face of unknown errors.

It can also be as simple as an interface whose operations are not fully
independent, such as described in [`DSG-010`], or an interface that cannot
behave in a transactional manner (and thus persisting a broken state if
crashing in the middle of or between operations).

Before finishing an interface, consider what promises your implementation can
and cannot deliver, determine which of these could leak through to your users,
and ask yourself whether it is better to change the interface than to leave it
as it is (or whether to document said behavior).

[subtly fail]: https://en.wikipedia.org/wiki/Two_Generals%27_Problem
[PACELC]: https://en.wikipedia.org/wiki/PACELC_design_principle

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [`CWE-362`]

[](){: #rule-dsg-009 }
#### [`DSG-009`] - Consider Providing Methods to Control Resource Consumption
[`DSG-009`]: #rule-dsg-009

When writing a library, consider providing ways for the user to limit resource
usage, such as a configurable upper bound on the number of concurrent sessions
or similar.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [`CWE-770`], [`API4:2023`]

[](){: #rule-dsg-010 }
#### [`DSG-010`] - Avoid Name Races
[`DSG-010`]: #rule-dsg-010

Erlang processes and ETS tables can have registered names, letting others find
them through their name instead of their process or table identifier. This
should be used with care, as a process or table may terminate at any time.
For example, if two messages are sent to a process through a registered name,
the second message may arrive to a newly restarted process that has not seen
the first, which may be significant ([CWE-386]).

To prevent these issues, either redesign your interface so that multiple
messages or lookups are not necessary, or look up the identifier from the
registered name and use the identifier instead.

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [CWE-386]

[CWE-386]: https://cwe.mitre.org/data/definitions/386.html

```erlang
%% DO
Pid = whereis(registered_process),
Pid ! hello,
Pid ! world.

Tid = ets:whereis(registered_table),
A = ets:lookup(Tid, KeyA),
B = ets:lookup(Tid, KeyB).

%% DO NOT
registered_process ! hello,
registered_process ! world.

A = ets:lookup(registered_table, KeyA),
B = ets:lookup(registered_table, KeyB).
```

[](){: #rule-dsg-011 }
#### [`DSG-011`] - Only Deserialize Trusted Data
[`DSG-011`]: #rule-dsg-011

Erlang/OTP provides various functionality that serializes and deserializes
general Erlang terms. Such functionality is intended to be used in a trusted
environment and is not suitable for communication with untrusted entities. For
example, you do not want to load a [`mnesia`] backup from an untrusted entity.
One issue with this being the potential for atom exhaustion, but more
importantly you could potentially end up with a `mnesia` table containing
harmful data ([`CWE-502`]). Other examples are `m:dets` and `m:disk_log`.

JSON is an example of a better format to use when communicating with untrusted
entities. Erlang/OTP provides the `m:json` module for JSON encoding/decoding.
XML is another example of a format that can be used. The [`xmerl`] application
provides functionality for encoding/decoding; see the [`xmerl` Application]
section below for more security related information.

The decoded data, of course, needs to be validated and sanitized if it does not
originate from a trusted entity. Using the `m:xmerl_sax_parser` when decoding XML
you can do this validation during decoding, and, for example, prevent issues
like memory exhaustion. The `m:json` module also provides a SAX style decoding
with user defined callbacks in which you can do the same.

[Rule priority:] `High`

Related CWEs and OWASP risks: [`CWE-74`], [`A05:2025`]

[`mnesia`]: `e:mnesia:index.html`

### Language

[](){: #rule-lng-001 }
#### [`LNG-001`] - Prefer Tuples Over Exporting Variables
[`LNG-001`]: #rule-lng-001

For historical reasons, Erlang does not employ lexical scoping, and variables
defined in "inner" expressions are available in "outer" expressions that follow
them. Using this makes code harder to reason about, and it is preferable to
write your code as if Erlang has lexical scoping by returning a tuple instead.
There are no performance penalties for doing this.

[Rule priority:] `Recommendation`

```erlang
%% DO
some_function(State0)
    {C, State1} = case foo(State0) of
                      {ok, A} ->
                          {a, bar(A)};
                      b ->
                          {b, State0}
                  end,
    bar(C, State1).

%% DO NOT
some_function(State0)
    C = case foo(State0) of
            {ok, A} ->
                State1 = bar(A),
                a;
            b ->
                State1 = State0,
                b
        end,
    bar(C, State1).
```

[](){: #rule-lng-002 }
#### [`LNG-002`] - Do Not Use `catch`
[`LNG-002`]: #rule-lng-002

The legacy `catch` construct cannot distinguish between `throw/1` and a normal
return, which can have very unexpected results. For instance, the
`m:gen_server` behavior will unintentionally accept any documented return value
when thrown because of its use of `catch`.

Instead, the modern [`try ... catch ... end`](`e:system:expressions.html`)
construct should be used.

The compiler can raise warnings for this by enabling the
`warn_deprecated_catch` option.

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [CWE-253], [CWE-480], [`A10:2025`]

[CWE-480]: https://cwe.mitre.org/data/definitions/480.html

```erlang
%% DO
try operation(A, B) of
    C -> ...
catch
    throw:Value ->
        ....;
    error:Reason ->
        ....
end

%% DO NOT
case (catch operation(A, B)) of
    {'EXIT', Reason} ->
        ...;
    C ->
        ...
end
```

[](){: #rule-lng-003 }
#### [`LNG-003`] - Do Not Use the Legacy `and` and `or` Operators
[`LNG-003`]: #rule-lng-003

These operators have been superseded by `andalso` and `orelse`, respectively.

The legacy operators have higher precedence than in most other languages. For
example `X and Y =:= 3` is parsed as `(X and Y) =:= 3`. In a function body,
this will crash, but when used in a guard it will silently fail. It can also
unexpectedly corrupt the intended logic without crashing when all operands are
booleans, for example `X and Y =:= Z` being parsed as `(X and Y) =:= Z`.

Instead, either use the modern `andalso` and `orelse` operators, or use `,`
and `;`, respectively.

[Rule priority:] `Recommendation`

Related CWEs and OWASP risks: [CWE-783]

[CWE-783]: https://cwe.mitre.org/data/definitions/783.html

### Miscellaneous

[](){: #rule-msc-001 }
#### [`MSC-001`] - Do Not Abuse `m:persistent_term`
[`MSC-001`]: #rule-msc-001

`m:persistent_term` is a very fast key-value storage intended for values that
rarely, if ever, change. Modifying one of these terms is allowed but comes at
an extreme performance cost as every single process in the system needs to be
scanned to potentially garbage-collect the old value. Use this feature with
great care.

[Rule priority:] `Medium`

[](){: #rule-msc-002 }
#### [`MSC-002`] - Consider Path Traversal Attacks
[`MSC-002`]: #rule-msc-002

The Erlang file routines do not protect against path traversal attacks by
default. When dealing with paths from an untrusted source (see [`DSG-007`]),
the `filelib:safe_relative_path/2` function should be used to get a safe path
relative to a given directory.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [`CWE-22`], [`CWE-59`]

[](){: #rule-msc-003 }
#### [`MSC-003`] - Use `open_port/2` With the `spawn_executable` Option To Start External Executables
[`MSC-003`]: #rule-msc-003

Calling [`open_port/2`] with the `{spawn, _}` argument will either start an
instance of a driver, or spawn an external program using the passed command
line. When spawning an external program, some platforms will start a shell,
which will search in the `PATH` in order to find the program. Environment
variable expansion may also be performed. If user input is to be part of the
command line for an external program, this makes it hard to verify and sanitize
this input. Even if user input is not part of the command, mistakes can easily
be made in non-trivial scenarios. Since loaded drivers and programs compete in
the same name-space, one can also unintentionally start an instance of a driver
instead of spawning an external program.

When spawning an external program, a safer approach is to use [`open_port/2`]
with the `{spawn_executable, _}` argument. No shell is used and the arguments
to the program needs to be explicitly listed using the `{args, Args}` option
where no environment variable expansion is made. You may also want to overwrite
certain environment variables using the `{env, Env}` option in order not to
expose those to the external program.

When starting an instance of a loaded driver, a safer approach is to use
[`open_port/2`] with the `{spawn_driver, _}` argument where no mix-up with
external programs may occur.

External OS commands can also be executed by calling `os:cmd/1` (or
`os:cmd/2`) with a command line as argument. `os:cmd/1,2` suffers from most of
the same issues as [`open_port/2`] with the `{spawn, _}` argument. A shell will
be started in which the passed command line is executed. The `PATH` will be
searched for the command and environment variables will be expanded. A safer
approach is to use [`open_port/2`] with the `{spawn_executable, _}` argument.
It requires gathering of the output from the command instead of getting it
returned as a string, but it is an easy task to handle.

[Rule priority:] `High`

Related CWEs and OWASP risks: [`CWE-78`]

[`open_port/2`]: `erlang:open_port/2`

[](){: #rule-msc-004 }
#### [`MSC-004`] - Protect Sensitive Data
[`MSC-004`]: #rule-msc-004

As Erlang logs many things by default, it is common for application data to
appear in log files and the like. This is not necessarily a problem but some
environments are under regulatory or compliance requirements not to leak
certain data to disk or similar in case an unauthorized person were to get
ahold of them (see [`CWE-532`]).

As the system cannot make any distinction between which data is sensitive and
which is not, it is up to the user to protect sensitive data. Ways of doing
this include, but are not limited to:

1. Using the `private` option for ETS tables containing sensitive data,
   preventing the table contents from being read by other processes.
1. Using `process_flag(sensitive, true)` for processes operating on sensitive
   data.
   
   This disables nearly all introspection for the process: other processes
   cannot inspect the message queue of this process, tracing is disabled, the
   process' data will not be included in a crash dump, and so on.
1. Implementing the `format_status/2` callback for `gen*` behaviors
   (`m:gen_server` et al) for processes operating on sensitive data.
   
   This lets you control how the process state is presented in introspection
   tools such as `observer`.

Furthermore, to prevent leaking data by stack traces or certain introspection
features, a common technique is to wrap secrets in a zero-arity `fun()` that is
then called in order to retrieve the secret. When passed around, the `fun()`
will appear instead of the data it retrieves when called. Whether the secret is
retrieved on demand or is part of the function environment is up to you, but
the latter has two drawbacks:

1. The secret becomes at least as long-lived as the `fun()`, increasing the
   risk of it showing up in a crash dump.
2. In a distributed system you may have differing versions of the code, and
   calling the `fun()` may crash because it's for a different version of its
   defining module. A workaround for that would be to extract the secret with
   `[Secret] = erlang:fun_info(Fun, env)` instead of calling the fun.

Another approach is to catch errors in sensitive sections of code and then
walking through each stack frame, discarding the arguments, before raising the
exception again. However, note that neither approach prevents the data from
leaking out through a crash or core dump.

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [CWE-209], [`CWE-532`]

[CWE-209]: https://cwe.mitre.org/data/definitions/209.html

[](){: #rule-msc-005 }
#### [`MSC-005`] - Treat Match Specifications As Code
[`MSC-005`]: #rule-msc-005

Match specifications, such as those used with ETS, are vulnerable to injection
attacks if they are constructed based on untrusted input.

When untrusted data is matched verbatim (such as a key), it is important to
wrap it in `{const, UntrustedData}` expressions. Building general queries based
on untrusted data should be avoided, but if that cannot be done, the query
should be the result of _parsing_ the untrusted data into a match
specification (where the final shape is controlled by the programmer), rather
than attempting to _validate_ the data before passing it in unaltered.

[Rule priority:] `High`

Related CWEs and OWASP risks: [`CWE-74`]

[](){: #rule-msc-006 }
#### [`MSC-006`] - Consider "Link Following" Attacks
[`MSC-006`]: #rule-msc-006

When accessing a file through a name, it is possible that the name does not
actually identify a file, but a link instead, which can in turn point at an
unintended resource, potentially outside of the intended boundaries.

This can be mitigated by using `file:read_link_info/2` to determine whether it
is a file or a symbolic link. It is important to call this with the file handle
_after_ opening it, instead of the file name before opening the file, in order
to avoid time-of-check time-of-use (TOCTOU) race condition where the file is
swapped out with a link between the check and opening the file (see [CWE-367]).

[Rule priority:] `Medium`

Related CWEs and OWASP risks: [`CWE-22`], [`CWE-59`], [CWE-61]

[`file:read_link_info/1,2`]: `file:read_link_info/2`
[CWE-61]: https://cwe.mitre.org/data/definitions/61.html
[CWE-367]: https://cwe.mitre.org/data/definitions/367.html

[](){: #rule-msc-007 }
#### [`MSC-007`] - Avoid Using Debug Functionality in Production
[`MSC-007`]: #rule-msc-007

Functionality that has been explicitly marked to be used only for debugging,
such as `erlang:list_to_pid/1` or the [`keep_secrets`](`t:ssl:common_option/0`)
[`ssl`] option should not be used in production environments, except during
interactive debugging. Unlike with normal functionality, there are no promises
of API stability for debug functionality, and they may change without notice.
They sometimes also have adverse effects on system properties while used (such
as greatly increasing scheduling latency), which are acceptable during
testing but not in production.

In production environments, debug functionality should be considered unsafe as
per [`DSG-006`].

[Rule priority:] `Critical`

Related CWEs and OWASP risks: [CWE-242], [CWE-489], [`A06:2025`]

[CWE-489]: https://cwe.mitre.org/data/definitions/489.html

[](){: #unsafe-functions }
### List of Unsafe Functionality that Should not be Used ([CWE-242])
[Unsafe Functions]: #unsafe-functions

| Unsafe functionality                       | Alternative functionality                                           | Note                                          |
|:-------------------------------------------|:--------------------------------------------------------------------|:----------------------------------------------|
| Undocumented functions/functionality       | *Only* use documented and supported functionality                   | See [`DSG-004`]                               |
| [`open_port/2`] with `{spawn, _}` argument | [`open_port/2`] with `{spawn_executable\|spawn_driver, _}` argument | See [`MSC-003`]                               |
| `m:http_uri` module                        | `m:uri_string` module                                               |                                               |
| `crypto:start/0`                           | `application:start(crypto)`                                         | See [Initializing `crypto`]                   |
| `crypto:enable_fips_mode/1`                | Use [`fips_mode`] application parameter instead                     | See [Initializing `crypto`]                   |
| `crypto:rand_uniform/2`                    | `rand:uniform_s/2` with a cryptographically strong generator        | See [Cryptographically Secure Random Numbers] |
| `crypto:private_encrypt/4` with `rsa_pkcs1_padding` option | For signatures use `crypto:sign/4` instead          | See [Legacy `crypto` Functions]               |
| `crypto:private_decrypt/4` with `rsa_pkcs1_padding` option |                                                     | See [Legacy `crypto` Functions]               |
| `crypto:public_encrypt/4` with `rsa_pkcs1_padding` option |                                                      | See [Legacy `crypto` Functions]               |
| `crypto:public_decrypt/4` with `rsa_pkcs1_padding` option | For signatures use `crypto:verify/5` instead         | See [Legacy `crypto` Functions]               |
| `public_key:encrypt_private/2`             |                                                                     | See [`public_key` Application]                |
| `public_key:encrypt_private/3` with `rsa_pkcs1_padding` option | For signatures use `public_key:sign/4` instead  | See [`public_key` Application]                |
| `public_key:decrypt_private/2`             |                                                                     | See [`public_key` Application]                |
| `public_key:decrypt_private/3` with `rsa_pkcs1_padding` option |                                                 | See [`public_key` Application]                |
| `public_key:encrypt_public/2`              |                                                                     | See [`public_key` Application]                |
| `public_key:encrypt_public/3` with `rsa_pkcs1_padding` option |                                                  | See [`public_key` Application]                |
| `public_key:decrypt_public/2`              |                                                                     | See [`public_key` Application]                |
| `public_key:decrypt_public/3` with `rsa_pkcs1_padding` option | For signatures use `public_key:verify/5` instead | See [`public_key` Application]                |

[Legacy `crypto` Functions]: #legacy-crypto-functions
[process dictionary]: `e:system:ref_man_processes.html#process-dictionary`

[](){: #potentially-unsafe-functions }
### List of Potentially Unsafe Functionality ([CWE-676])
[Potentially Unsafe Functions]: #potentially-unsafe-functions

*All of the below listed functions have valid use-cases*, but may become a concern if not used properly.

| Potentially unsafe functionality           | Potential alternative functionality                                 | Note                                   |
|:-------------------------------------------|:--------------------------------------------------------------------|:---------------------------------------|
| [`binary_to_atom/1`]                       | [`binary_to_existing_atom/1`]                                       | See [`DSG-003`]                        |
| [`binary_to_atom/2`]                       | [`binary_to_existing_atom/2`]                                       | See [`DSG-003`]                        |
| [`list_to_atom/1`]                         | [`list_to_existing_atom/1`]                                         | See [`DSG-003`]                        |
| `os:cmd/1`                                 | [`open_port/2`] with `{spawn_executable, _}` argument               | See [`MSC-003`]                        |
| `os:cmd/2`                                 | [`open_port/2`] with `{spawn_executable, _}` argument               | See [`MSC-003`]                        |
| `erlang:load_nif/2`                        |                                                                     | See [Native Code]                      |
| `erl_ddll:load/2`                          |                                                                     | See [Native Code]                      |
| `erl_ddll:load_driver/2`                   |                                                                     | See [Native Code]                      |
| `erl_ddll:try_load/3`                      |                                                                     | See [Native Code]                      |
| `erl_ddll:reload/2`                        |                                                                     | See [Native Code]                      |
| `erl_ddll:reload_driver/2`                 |                                                                     | See [Native Code]                      |
| `file:consult/1`                           |                                                                     | See [`DSG-003`] and [`DSG-011`]        |
| `file:path_consult/2`                      |                                                                     | See [`DSG-003`] and [`DSG-011`]        |
| `binary_to_term/1`                         | [`binary_to_term/2`] with `safe` option                             | See [`DSG-003`] and [`DSG-011`]        |
| `binary_to_term/2` without `safe` option   | [`binary_to_term/2`] with `safe` option                             | See [`xmerl` Application]              |
| `xmerl_scan:file/1`                        | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_scan:file/2`                        | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_scan:string/1`                      | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_scan:string/2`                      | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_scan:string/1`                      | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_scan:string/2`                      | `m:xmerl_sax_parser` module                                         | See [`xmerl` Application]              |
| `xmerl_sax_parser:file/2` without `disallow_entities` option | `xmerl_sax_parser:file/2` with `disallow_entities` option | See [`xmerl` Application]      |
| `xmerl_sax_parser:stream/2` without `disallow_entities` option | `xmerl_sax_parser:stream/2` with `disallow_entities` option | See [`xmerl` Application]  |
| `socket:open/1`                            |  `socket:open/2` with `dup` option, which must still be used with extreme care |                             |
| `socket:open/2` without `dup` option       |  `socket:open/2` with `dup` option, which must still be used with extreme care |                             |
| `ssl:prf/5`                                | `ssl:export_key_materials/4`                                        |                                        |
| `ssl:prf/5`                                | `ssl:export_key_materials/4`                                        |                                        |
| `ssl:prf/5`                                | `ssl:export_key_materials/4`                                        |                                        |
| Deprecated functionality                   |                                                                     | See [`DSG-005`]                        |

[`binary_to_atom/1`]: `erlang:binary_to_atom/1`
[`binary_to_atom/2`]: `erlang:binary_to_atom/2`
[`binary_to_existing_atom/1`]: `erlang:binary_to_existing_atom/1`
[`binary_to_existing_atom/2`]: `erlang:binary_to_existing_atom/2`
[`binary_to_term/1`]: `erlang:binary_to_term/1`

[](){: #h-top-40-cwes }
## Top 40 CWEs
[Top 40 CWEs]: #h-top-40-cwes

This section comments on the top 40 CWEs in 2024 and how they relate to
Erlang/OTP, covering both the [CWE Top 25] and the [On The Cusp] list.

[CWE Top 25]: https://cwe.mitre.org/top25/archive/2024/2024_cwe_top25.html
[On The Cusp]: https://cwe.mitre.org/top25/archive/2024/2024_onthecusp_list.html

   [](){: #cwe-79 }
1. [CWE-79] - Improper Neutralization of Input During Web Page Generation ('Cross-site Scripting')
   [`CWE-79`]: #cwe-79
   
   Erlang/OTP does not include any web frameworks, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/79.html) for
   general advice on how to deal with this.
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-79]: https://cwe.mitre.org/data/definitions/79.html
   [](){: #cwe-787 }
1. [CWE-787] - Out-of-bounds Write
   [`CWE-787`]: #cwe-787
   
   This issue cannot occur because Erlang is memory safe.
   
   [CWE-787]: https://cwe.mitre.org/data/definitions/787.html
   [](){: #cwe-89 }
1. [CWE-89] - Improper Neutralization of Special Elements used in an SQL Command ('SQL Injection')
   [`CWE-89`]: #cwe-89
   
   Erlang/OTP does not provide any SQL database adapters. However, similar
   attacks could potentially be targeted at match specifications when used in
   ETS, see [`MSC-005`]. As with other injection attacks, following [`DSG-007`]
   may also help.
   
   Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/89.html) for
   general advice on how to deal with this.
   
   [CWE-89]: https://cwe.mitre.org/data/definitions/89.html
   [](){: #cwe-352 }
1. [CWE-352] - Cross-Site Request Forgery (CSRF)
   [`CWE-352`]: #cwe-352
   
   Erlang/OTP does not include any web frameworks, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/352.html) for
   general advice on how to deal with this.
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-352]: https://cwe.mitre.org/data/definitions/352.html
   [](){: #cwe-22 }
1. [CWE-22] - Improper Limitation of a Pathname to a Restricted Directory ('Path Traversal')
   [`CWE-22`]: #cwe-22
   
   See [`MSC-002`]. As with other injection attacks, following [`DSG-007`] may
   also help.
   
   [CWE-22]: https://cwe.mitre.org/data/definitions/22.html
   [](){: #cwe-125 }
1. [CWE-125] - Out-of-bounds read
   [`CWE-125`]: #cwe-125
   
   This issue cannot occur because Erlang is memory safe.
   
   [CWE-125]: https://cwe.mitre.org/data/definitions/125.html
   [](){: #cwe-78 }
1. [CWE-78] - Improper Neutralization of Special Elements used in an OS Command ('OS Command Injection')
   [`CWE-78`]: #cwe-78
   
   See [`MSC-003`]. As with other injection attacks, following [`DSG-007`] may
   also help.
   
   [CWE-78]: https://cwe.mitre.org/data/definitions/78.html
   [](){: #cwe-416 }
1. [CWE-416] - Use After Free
   [`CWE-416`]: #cwe-416
   
   This issue cannot occur because Erlang is memory safe.
   
   However, as an example it is still possible to attempt to read from a file
   that the user has explicitly closed. Doing so results in an error being
   raised.
   
   [CWE-416]: https://cwe.mitre.org/data/definitions/416.html
   [](){: #cwe-862 }
1. [CWE-862] - Missing Authorization
   [`CWE-862`]: #cwe-862
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol, this is not an Erlang-specific issue.
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/862.html) for
   general advice on how to deal with this.
   
   [CWE-862]: https://cwe.mitre.org/data/definitions/862.html
   [](){: #cwe-434 }
1. [CWE-434] - Unrestricted Upload of File with Dangerous Type
   [`CWE-434`]: #cwe-434
   
   When using the `ssh_sftpd` module, a custom `file_handler` module can be
   provided to filter out unwanted uploads, but note that this does not permit
   a deep inspection of file contents before upload, and is under no
   circumstances a replacement for proper authentication.

   Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/434.html) for
   general advice on how to deal with this.
   
   [CWE-434]: https://cwe.mitre.org/data/definitions/434.html
   [](){: #cwe-94 }
1. [CWE-94] - Improper Control of Generation of Code ('Code Injection')
   [`CWE-94`]: #cwe-94
   
   This is not Erlang-specific, please refer to
   [CWE description](https://cwe.mitre.org/data/definitions/94.html) for
   general advice on how to deal with this.
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-94]: https://cwe.mitre.org/data/definitions/94.html
   [](){: #cwe-20 }
1. [CWE-20] - Improper Input Validation
   [`CWE-20`]: #cwe-20
   
   Special care should be taken around atoms as described in the
   [Atom Exhaustion] section. Following [`DSG-003`], [`DSG-011`] and
   [`DSG-007`] may also help.
   
   Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/20.html) for
   general advice on how to deal with this.
   
   [CWE-20]: https://cwe.mitre.org/data/definitions/20.html
   [](){: #cwe-77 }
1. [CWE-77] - Improper Neutralization of Special Elements used in a Command ('Command Injection')
   [`CWE-77`]: #cwe-77
   
   This is a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/77.html) for
   general advice on how to deal with this.
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-77]: https://cwe.mitre.org/data/definitions/77.html
   [](){: #cwe-287 }
1. [CWE-287] - Improper Authentication 
   [`CWE-287`]: #cwe-287
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is
   a general issue.
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/287.html) for
   general advice on how to deal with this.

   [CWE-287]: https://cwe.mitre.org/data/definitions/287.html
   [](){: #cwe-269 }
1. [CWE-269] - Improper Privilege Management 
   [`CWE-269`]: #cwe-269
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/862.html) for
   general advice on how to deal with this.
   
    [CWE-269]: https://cwe.mitre.org/data/definitions/269.html
   [](){: #cwe-502 }
1. [CWE-502] - Deserialization of Untrusted Data
   [`CWE-502`]: #cwe-502
   
   See [`DSG-011`].
   
   [CWE-502]: https://cwe.mitre.org/data/definitions/502.html
   [](){: #cwe-200 }
1. [CWE-200] - Exposure of Sensitive Information to an Unauthorized Actor
   [`CWE-200`]: #cwe-200
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is a general issue.
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/200.html) for
   general advice on how to deal with this.
   
   See also [`MSC-004`] and [`DSG-007`].
   
   [CWE-200]: https://cwe.mitre.org/data/definitions/200.html
   [](){: #cwe-863 }
1. [CWE-863] - Incorrect Authorization 
   [`CWE-863`]: #cwe-863
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is
   a general issue.
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/863.html) for
   general advice on how to deal with this.
   
   [CWE-863]: https://cwe.mitre.org/data/definitions/863.html
   [](){: #cwe-918 }
1. [CWE-918] - Server-Side Request Forgery (SSRF) 
   [`CWE-918`]: #cwe-918
   
   Erlang/OTP does not include any web frameworks, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/918.html) for
   general advice on how to deal with this.
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-918]: https://cwe.mitre.org/data/definitions/918.html
   [](){: #cwe-119 }
1. [CWE-119] - Improper Restriction of Operations within the Bounds of a Memory Buffer
   [`CWE-119`]: #cwe-119
   
   This issue cannot occur because Erlang is memory safe.

   [CWE-119]: https://cwe.mitre.org/data/definitions/119.html
   [](){: #cwe-476 }
1. [CWE-476] - NULL Pointer Dereference
   [`CWE-476`]: #cwe-476
   
   This issue cannot occur because Erlang is memory safe.
   
   [CWE-476]: https://cwe.mitre.org/data/definitions/476.html
   [](){: #cwe-798 }
1. [CWE-798] - Use of Hard-coded Credentials 
   [`CWE-798`]: #cwe-798
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/798.html) for
   general advice on how to deal with this.
   
   [CWE-798]: https://cwe.mitre.org/data/definitions/798.html
   [](){: #cwe-190 }
1. [CWE-190] - Integer Overflow or Wraparound
   [`CWE-190`]: #cwe-190
   
   Erlang uses arbitrary-precision arithmetic rather than wrapping around or
   behaving in an ill-defined manner on overflow. Note that this weakness is
   distinct from throwing an error on overflow, which may still occur if the
   number grows to be several megabits in size. See [system limits].
   
   [CWE-190]: https://cwe.mitre.org/data/definitions/190.html
   [](){: #cwe-400 }
1. [CWE-400] - Uncontrolled Resource Consumption
   [`CWE-400`]: #cwe-400
   
   Aside from the previously mentioned issues regarding [Atom Exhaustion], this
   is a general issue.
   
   Most network-facing OTP applications provide options for controlling
   resource consumption. See [`ssl` Application] and [`ssh` Application] for
   more information.
   
   Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/400.html) for
   general advice on how to deal with this.
   
   [CWE-400]: https://cwe.mitre.org/data/definitions/400.html
   [](){: #cwe-306 }
1. [CWE-306] - Missing Authentication for Critical Function
   [`CWE-306`]: #cwe-306
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is
   a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/306.html) for
   general advice on how to deal with this.
   
   [CWE-306]: https://cwe.mitre.org/data/definitions/306.html
   [](){: #cwe-770 }
1. [CWE-770] - Allocation of Resources Without Limits or Throttling 
   [`CWE-770`]: #cwe-770
   
   See [`DSG-009`]. Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/770.html) for
   general advice on how to deal with this.
   
   [CWE-770]: https://cwe.mitre.org/data/definitions/770.html
   [](){: #cwe-668 }
1. [CWE-668] - Exposure of Resource to Wrong Sphere
   [`CWE-668`]: #cwe-668
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is
   a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/668.html) for
   general advice on how to deal with this.
   
   [CWE-668]: https://cwe.mitre.org/data/definitions/668.html
1. [CWE-74] - Improper Neutralization of Special Elements in Output Used by a
   [](){: #cwe-74 }
   Downstream Component ('Injection')
   [`CWE-74`]: #cwe-74
   
   This is a general issue, and is also just a parent of other previously
   mentioned CWEs such as [CWE-77], [CWE-78], [CWE-79], [CWE-89].
   
   As with other injection attacks, following [`DSG-007`] may also help.
   
   [CWE-74]: https://cwe.mitre.org/data/definitions/74.html
   [](){: #cwe-427 }
1. [CWE-427] - Uncontrolled Search Path Element
   [`CWE-427`]: #cwe-427
   
   See [`DEP-004`].
   
   [CWE-427]: https://cwe.mitre.org/data/definitions/427.html
   [](){: #cwe-639 }
1. [CWE-639] - Authorization Bypass Through User-Controlled Key
   [`CWE-639`]: #cwe-639
   
   This is a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/639.html) for
   general advice on how to deal with this.
   
   [CWE-639]: https://cwe.mitre.org/data/definitions/639.html
   [](){: #cwe-532 }
1. [CWE-532] - Insertion of Sensitive Information into Log File
   [`CWE-532`]: #cwe-532
   
   See [`MSC-004`].
   
   [CWE-532]: https://cwe.mitre.org/data/definitions/532.html
   [](){: #cwe-732 }
1. [CWE-732] - Incorrect Permission Assignment for Critical Resource
   [`CWE-732`]: #cwe-732
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (see [`DEP-001`]), this is a general issue. Please
   refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/732.html) for
   general advice on how to deal with this.
   
   [CWE-732]: https://cwe.mitre.org/data/definitions/732.html
   [](){: #cwe-601 }
1. [CWE-601] - URL Redirection to Untrusted Site ('Open Redirect')
   [`CWE-601`]: #cwe-601
   
   This is a general issue. When acting as a client using `m:httpc`, the
   `{autoredirect, false}` option can be passed to avoid blindly following
   redirects.
   
   Otherwise, please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/601.html) for
   general advice on how to deal with this.
   
   [CWE-601]: https://cwe.mitre.org/data/definitions/601.html
   [](){: #cwe-362 }
1. [CWE-362] - Concurrent Execution using Shared Resource with Improper Synchronization ('Race Condition')
   [`CWE-362`]: #cwe-362

   As Erlang provides message-passing concurrency, race conditions are very
   easily avoided. Wrapping a shared resource in an Erlang process and routing
   all access through it guarantees atomicity regardless of the nature of the
   resource.
   
   Following [`DSG-008`] may also help.
   
   [CWE-362]: https://cwe.mitre.org/data/definitions/362.html
   [](){: #cwe-522 }
1. [CWE-522] - Insufficiently Protected Credentials
   [`CWE-522`]: #cwe-522
   
   This is a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/522.html) for
   general advice on how to deal with this.
   
   [CWE-522]: https://cwe.mitre.org/data/definitions/522.html
   [](){: #cwe-276 }
1. [CWE-276] - Incorrect Default Permissions
   [`CWE-276`]: #cwe-276
   
   Aside from the previously mentioned issues regarding the default Erlang
   distribution protocol (which is effectively wide open by default, see
   [`DEP-001`]), this is a general issue.
   
   Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/276.html) for
   general advice on how to deal with this.
   
   [CWE-276]: https://cwe.mitre.org/data/definitions/276.html
   [](){: #cwe-203 }
1. [CWE-203] - Observable Discrepancy
   [`CWE-203`]: #cwe-203
   
   An attacker can easily bypass the ["cookie" mechanism] of the default
   Erlang distribution protocol by exploiting timing differences. When exposing
   the Erlang distribution protocol on an untrusted network, it _must_ be
   configured to use [TLS with client certificate verification] (see also
   [`DEP-001`]).
   
   Otherwise, this is a user-level issue. Interfaces that deal with sensitive
   data must be inaccessible without prior authentication: without access,
   there can be no leak of information.
   
   It is not practical to attempt to implement a freely accessible interface
   without observable discrepancies, and unfortunately many guides on the
   subject in general provide advice on how to minimize timing differences
   _within_ the interface. This is prohibitively difficult regardless of
   implementation language: even the Intel SGX hardware extensions
   [were broken] through a timing attack, despite Intel being in _full control_
   of all aspects.
   
   The best known way to prevent this issue is through cryptographically secure
   authentication _before_ any further interaction is admitted. Depending on
   the threat model this may also extend to interactions within a system
   where, for example, an administrator may have more access to sensitive data
   than others.
   
   [were broken]: https://dl.acm.org/doi/10.1145/3065913.3065915
   [CWE-203]: https://cwe.mitre.org/data/definitions/203.html
   [](){: #cwe-59 }
1. [CWE-59] - Improper Link Resolution Before File Access ('Link Following') 
   [`CWE-59`]: #cwe-59
   
   See [`MSC-006`].

   [CWE-59]: https://cwe.mitre.org/data/definitions/59.html
   [](){: #cwe-843 }
1. [CWE-843] - Access of Resource Using Incompatible Type ('Type Confusion')
   [`CWE-843`]: #cwe-843
   
   This issue cannot occur because Erlang is memory safe and type coercion is
   not part of the language. As a dynamic language, the offending process will
   crash if incorrect types are used, but this is not a problem in a program
   that follows our [Error Handling] conventions.
   
   Following [`DSG-007`] may also help.
   
   [CWE-843]: https://cwe.mitre.org/data/definitions/843.html
   [](){: #cwe-312 }
1. [CWE-312] - Cleartext Storage of Sensitive Information
   [`CWE-312`]: #cwe-312
   
   This is a general issue. Please refer to the
   [CWE description](https://cwe.mitre.org/data/definitions/312.html) for
   general advice on how to deal with this.
   
   [CWE-312]: https://cwe.mitre.org/data/definitions/312.html

[](){: #h-owasp-top-10 }
## OWASP Top 10
[OWASP Top 10]: #h-owasp-top-10

This section comments on the [top ten security risks] as catalogued by [OWASP].
In general, following the Erlang/OTP conventions and documentation mitigates
these risks.

[top ten security risks]: https://owasp.org/Top10/2025/0x00_2025-Introduction/
[OWASP]: https://owasp.org/

   [](){: #owasp-a01-2025 }
1. [A01:2025] - Broken Access Control
   [`A01:2025`]: #owasp-a01-2025
   
   This risk does not relate to Erlang/OTP per se, other than the previously
   described deficiencies regarding the default distribution protocol, see
   [`DEP-001`].
   
   Otherwise, please refer to the [A01:2025] description for advice on how to
   deal with this.
   
   [A01:2025]: https://owasp.org/Top10/2025/A01_2025-Broken_Access_Control/
   [](){: #owasp-a02-2025 }
1. [A02:2025] - Security Misconfiguration
   [`A02:2025`]: #owasp-a02-2025
   
   See [`A01:2025`] and the [A02:2025] description for advice on how to deal
   with this.
   
   [A02:2025]: https://owasp.org/Top10/2025/A02_2025-Security_Misconfiguration/
   [](){: #owasp-a03-2025 }
1. [A03:2025] - Software Supply Chain Failures
   [`A03:2025`]: #owasp-a03-2025
   
   This risk does not relate to Erlang per se. For what it is worth, Erlang/OTP
   provides a built source Software Bill of Materials (SBoM) for every release.

   Otherwise, please refer to the [A03:2025] description for advice on how to
   deal with this.
   
   [A03:2025]: https://owasp.org/Top10/2025/A03_2025-Software_Supply_Chain_Failures/
   [](){: #owasp-a04-2025 }
1. [A04:2025] - Cryptographic Failures
   [`A04:2025`]: #owasp-a04-2025
   
   This risk does not relate to Erlang/OTP per se, but some risks can be
   mitigated by following the advice in the [`crypto` Application] section.

   Otherwise, please refer to the [A04:2025] description for advice on how to
   deal with this.
   
   [A04:2025]: https://owasp.org/Top10/2025/A04_2025-Cryptographic_Failures/
   [](){: #owasp-a05-2025 }
1. [A05:2025] - Injection
   [`A05:2025`]: #owasp-a05-2025
   
   See [`MSC-004`] and the [A05:2025] description for advice on how to deal
   with this.
   
   [A05:2025]: https://owasp.org/Top10/2025/A05_2025-Injection/
   [](){: #owasp-a06-2025 }
1. [A06:2025] - Insecure Design
   [`A06:2025`]: #owasp-a06-2025
   
   This risk does not relate to Erlang per se, but following this guide, our
   [Design principles], and the [A06:2025] description provides a good
   foundation to build upon.
   
   [Design principles]: https://www.erlang.org/doc/system/design_principles.html
   
   [A06:2025]: https://owasp.org/Top10/2025/A06_2025-Insecure_Design/
   [](){: #owasp-a07-2025 }
1. [A07:2025] - Authentication Failures
   [`A07:2025`]: #owasp-a07-2025
   
   See [`A01:2025`] and the [A07:2025] description for advice on how to deal
   with this.
   
   [A07:2025]: https://owasp.org/Top10/2025/A07_2025-Authentication_Failures/
   [](){: #owasp-a08-2025 }
1. [A08:2025] - Software or Data Integrity Failures
   [`A08:2025`]: #owasp-a08-2025
   
   Erlang/OTP provides various tools to help ensure data integrity, notably
   `public_key:sign/4` and `public_key:verify/5`.
   
   Note, however, that there is _no built-in support_ for software integrity
   checks. All components of the software itself are considered _trusted_ and
   are _only_ subject to checks that protect against some kinds of accidental
   corruption.

   Otherwise, please refer to the [A08:2025] description for advice on how to
   deal with this.
   
   [A08:2025]: https://owasp.org/Top10/2025/A08_2025-Software_or_Data_Integrity_Failures/
   [](){: #owasp-a09-2025 }
1. [A09:2025] - Logging & Alerting Failures
   [`A09:2025`]: #owasp-a09-2025
   
   Erlang/OTP logs many things by default, including processes that terminate
   abnormally ("crash"), which is especially effective when following the
   convention outlined in the [Error Handling] section. These logs are passed
   through the `m:logger` framework and can be configured at will.
   
   It is also important to consider the possibility of leaking sensitive
   information through logs, see [`MSC-004`].

   Otherwise, please refer to the [A09:2025] description for advice on how to
   deal with this.
   
   [A09:2025]: https://owasp.org/Top10/2025/A09_2025-Logging_and_Alerting_Failures/
   [](){: #owasp-a10-2025 }
1. [A10:2025] - Mishandling of Exceptional Conditions
   [`A10:2025`]: #owasp-a10-2025
   
   As mentioned in the [Error Handling] section, program execution should be
   restricted to that which is expected, and unexpected situations should be left
   to the supervision structure. See also [`DSG-001`] and the [A10:2025]
   description for advice on how to deal with this.
   
   [A10:2025]: https://owasp.org/Top10/2025/A10_2025-Mishandling_of_Exceptional_Conditions/

[](){: #owasp-api-top-10 }
## OWASP API Security Top 10
[OWASP API Security Top 10]: #owasp-api-top-10

This section comments on the [top ten API security risks] as catalogued by
[OWASP]. In general, following the Erlang/OTP conventions and documentation
mitigates these risks, but their nature means that there are few
Erlang-specific recommendations.

[top ten API security risks]: https://owasp.org/API-Security/editions/2023/en/0x11-t10/

   [](){: #owasp-api1-2023 }
1. [API1:2023] - Broken Object Level Authorization
   [`API1:2023`]: #owasp-api1-2023
   
   This is a general issue, please refer to the [API1:2023] description for
   advice on how to deal with this.
   
   [API1:2023]: https://owasp.org/API-Security/editions/2023/en/0xa1-broken-object-level-authorization/
   [](){: #owasp-api2-2023 }
1. [API2:2023] - Broken Authentication
   [`API2:2023`]: #owasp-api2-2023
   
   This is a general issue, similar to [`A01:2025`] - Broken Access Control.
   Please refer to the [API2:2023] description for advice on how to deal with
   this.
   
   [API2:2023]: https://owasp.org/API-Security/editions/2023/en/0xa2-broken-authentication/
   [](){: #owasp-api3-2023 }
1. [API3:2023] - Broken Object Property Level Authorization
   [`API3:2023`]: #owasp-api3-2023
   
   This is a general issue, similar to [`API1:2023`] - Broken Object Level
   Authorization. Please refer to the [API3:2023] description for advice on how
   to deal with this.
   
   [API3:2023]: https://owasp.org/API-Security/editions/2023/en/0xa3-broken-object-property-level-authorization/
   [](){: #owasp-api4-2023 }
1. [API4:2023] - Unrestricted Resource Consumption
   [`API4:2023`]: #owasp-api4-2023
   
   Aside from the previously mentioned issues regarding [Atom Exhaustion], this
   is a general issue. Please refer to [`DSG-009`], [`CWE-770`], and the
   [API4:2023] description for advice on how to deal with this.
   
   [API4:2023]: https://owasp.org/API-Security/editions/2023/en/0xa4-unrestricted-resource-consumption/
   [](){: #owasp-api5-2023 }
1. [API5:2023] - Broken Function Level Authorization
   [`API5:2023`]: #owasp-api5-2023
   
   This is a general issue, similar to [`API1:2023`] - Broken Object Level
   Authorization. Please refer to the [API5:2023] description for advice on how
   to deal with this.
   
   [API5:2023]: https://owasp.org/API-Security/editions/2023/en/0xa5-broken-function-level-authorization/
   [](){: #owasp-api6-2023 }
1. [API6:2023] - Unrestricted Access to Sensitive Business Flows
   [`API6:2023`]: #owasp-api6-2023
   
   This is a general issue, similar to [`API1:2023`] - Broken Object Level
   Authorization. Please refer to the [API6:2023] description for advice on how
   to deal with this.
   
   [API6:2023]: https://owasp.org/API-Security/editions/2023/en/0xa6-unrestricted-access-to-sensitive-business-flows/
   [](){: #owasp-api7-2023 }
1. [API7:2023] - Server Side Request Forgery
   [`API7:2023`]: #owasp-api7-2023
   
   This is a general issue, please refer to [`CWE-918`] and the [API7:2023]
   description for advice on how to deal with this.
   
   [API7:2023]: https://owasp.org/API-Security/editions/2023/en/0xa7-server-side-request-forgery/
   [](){: #owasp-api8-2023 }
1. [API8:2023] - Security Misconfiguration
   [`API8:2023`]: #owasp-api8-2023

   This risk does not relate to Erlang/OTP per se, other than the previously
   described deficiencies regarding the default distribution protocol (see
   [`DEP-001`]).
   
   Otherwise, please refer to the [API8:2023] description for advice on how to
   deal with this.
   
   [API8:2023]: https://owasp.org/API-Security/editions/2023/en/0xa8-security-misconfiguration/
   [](){: #owasp-api9-2023 }
1. [API9:2023] - Improper Inventory Management
   [`API9:2023`]: #owasp-api9-2023
   
   This is a general issue, please refer to the [API9:2023] description for
   advice on how to deal with this.
   
   [API9:2023]: https://owasp.org/API-Security/editions/2023/en/0xa9-improper-inventory-management/
   [](){: #owasp-api10-2023 }
1. [API10:2023] - Unsafe Consumption of APIs
   [`API10:2023`]: #owasp-api10-2023
   
   This is a general issue, please refer to the [API10:2023] description for
   advice on how to deal with this.
   
   [API10:2023]: https://owasp.org/API-Security/editions/2023/en/0xaa-unsafe-consumption-of-apis/
