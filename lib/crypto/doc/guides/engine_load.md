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
# Engine Load

[](){: #engine_load } This chapter describes the support for loading encryption
engines in the crypto application.

## Background

OpenSSL exposes an Engine API, which makes it possible to plug in alternative
implementations for some or all of the cryptographic operations implemented by
OpenSSL. When configured appropriately, OpenSSL calls the engine's
implementation of these operations instead of its own.

Typically, OpenSSL engines provide a hardware implementation of specific
cryptographic operations. The hardware implementation usually offers improved
performance over its software-based counterpart, which is known as cryptographic
acceleration.

> #### Note {: .info }
>
> The file name requirement on the engine dynamic library can differ between SSL
> versions.

## Use Cases

### Dynamically load an engine from default directory

If the engine is located in the OpenSSL/LibreSSL installation `engines`
directory.

```erlang
1> {ok, Engine} = crypto:engine_load(<<"otp_test_engine">>, [], []).
 {ok, #Ref}
```

### Load an engine with the dynamic engine

Load an engine with the help of the dynamic engine by giving the path to the
library.

```erlang
 2> {ok, Engine} = crypto:engine_load(<<"dynamic">>,
                                      [{<<"SO_PATH">>,
                                        <<"/some/path/otp_test_engine.so">>},
                                       {<<"ID">>, <<"MD5">>},
                                       <<"LOAD">>],
                                      []).
 {ok, #Ref}
```

### Load an engine and replace some methods

Load an engine with the help of the dynamic engine and just replace some engine
methods.

```erlang
 3> {ok, Engine} = crypto:engine_load(<<"dynamic">>,
                                      [{<<"SO_PATH">>,
                                        <<"/some/path/otp_test_engine.so">>},
                                       {<<"ID">>, <<"MD5">>},
                                       <<"LOAD">>],
                                      []).
{ok, #Ref}
4> ok = crypto:engine_register(Engine, [engine_method_digests]).
ok
```

### Load with the ensure loaded function

This function makes sure the engine is loaded just once and the ID is added to
the internal engine list of OpenSSL. The following calls to the function will
check if the ID is loaded and then just get a new reference to the engine.

```erlang
 5> {ok, Engine} = crypto:ensure_engine_loaded(<<"MD5">>,
                                               <<"/some/path/otp_test_engine.so">>).
 {ok, #Ref}
```

To remove the tag from the OpenSSL engine list use `crypto:engine_remove/1`.

```erlang
 6> crypto:engine_remove(Engine).
 ok
```

To unload it use `crypto:engine_unload/1` which removes the references to the
engine.

```erlang
 6> crypto:engine_unload(Engine).
 ok
```

### List all engines currently loaded

```erlang
 8> crypto:engine_list().
[<<"dynamic">>, <<"MD5">>]
```
