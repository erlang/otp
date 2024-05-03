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
# Inet Configuration

## Introduction

This section describes how the Erlang runtime system is configured for IP
communication. It also explains how you can configure it for your needs by a
configuration file. The information is primarily intended for users with special
configuration needs or problems. There is normally no need for specific settings
for Erlang to function properly on a correctly IP-configured platform.

When Erlang starts up it reads the Kernel variable `inetrc`, which, if defined,
is to specify the location and name of a user configuration file. Example:

```text
% erl -kernel inetrc '"./cfg_files/erl_inetrc"'
```

Notice that the use of an `.inetrc` file, which was supported in earlier
Erlang/OTP versions, is now obsolete.

A second way to specify the configuration file is to set environment variable
`ERL_INETRC` to the full name of the file. Example (bash):

```text
% export ERL_INETRC=./cfg_files/erl_inetrc
```

Notice that the Kernel variable `inetrc` overrides this environment variable.

If no user configuration file is specified and Erlang is started in
non-distributed or short name distributed mode, Erlang uses default
configuration settings and a native lookup method that works correctly under
most circumstances. Erlang reads no information from system `inet` configuration
files (such as `/etc/host.conf` and `/etc/nsswitch.conf`) in these modes, except
for `/etc/resolv.conf` and `/etc/hosts` that is read and monitored for changes
on Unix platforms for the internal DNS client `m:inet_res`.

If Erlang is started in long name distributed mode, it needs to get the domain
name from somewhere and reads system `inet` configuration files for this
information. Any hosts and resolver information found is also recorded, but not
used as long as Erlang is configured for native lookups. The information becomes
useful if the lookup method is changed to `'file'` or `'dns'`, see below.

Native lookup (system calls) is always the default resolver method. This is true
for all platforms, except OSE Delta where `'file'` or `'dns'` is used (in that
priority order).

On Windows platforms, Erlang searches the system registry rather than looks for
configuration files when started in long name distributed mode.

## Configuration Data

Erlang records the following data in a local database if found in system `inet`
configuration files (or system registry):

- Hostnames and host addresses
- Domain name
- Nameservers
- Search domains
- Lookup method

This data can also be specified explicitly in the user configuration file. This
file is to contain lines of configuration parameters (each terminated with a
full stop). Some parameters add data to the configuration (such as host and
nameserver), others overwrite any previous settings (such as domain and lookup).
The user configuration file is always examined last in the configuration
process, making it possible for the user to override any default values or
previously made settings. Call `inet:get_rc()` to view the state of the `inet`
configuration database.

The valid configuration parameters are as follows:

- ```
  {file, Format, File}.
    Format = atom()
    File = string()
  ```

  Specify a system file that Erlang is to read configuration data from. `Format`
  tells the parser how the file is to be interpreted:

  - `resolv` (Unix resolv.conf)
  - `host_conf_freebsd` (FreeBSD host.conf)
  - `host_conf_bsdos` (BSDOS host.conf)
  - `host_conf_linux` (Linux host.conf)
  - `nsswitch_conf` (Unix nsswitch.conf)
  - `hosts` (Unix hosts)

  `File` is to specify the filename with full path.

- ```
  {resolv_conf, File}.
    File = string()
  ```

  Specify a system file that Erlang is to read resolver configuration from for
  the internal DNS client `m:inet_res`, and monitor for changes, even if it does
  not exist. The path must be absolute.

  This can override the configuration parameters `nameserver` and `search`
  depending on the contents of the specified file. They can also change any time
  in the future reflecting the file contents.

  If the file is specified as an empty string `""`, no file is read or monitored
  in the future. This emulates the old behavior of not configuring the DNS
  client when the node is started in short name distributed mode.

  If this parameter is not specified, it defaults to `/etc/resolv.conf` unless
  environment variable `ERL_INET_ETC_DIR` is set, which defines the directory
  for this file to some maybe other than `/etc`.

- ```
  {hosts_file, File}.
    File = string()
  ```

  Specify a system file that Erlang is to read resolver configuration from for
  the internal hosts file resolver, and monitor for changes, even if it does not
  exist. The path must be absolute.

  These host entries are searched after all added with `{file, hosts, File}`
  above or `{host, IP, Aliases}` below when lookup option `file` is used.

  If the file is specified as an empty string `""`, no file is read or monitored
  in the future. This emulates the old behavior of not configuring the DNS
  client when the node is started in short name distributed mode.

  If this parameter is not specified, it defaults to `/etc/hosts` unless
  environment variable `ERL_INET_ETC_DIR` is set, which defines the directory
  for this file to some maybe other than `/etc`.

- ```
  {registry, Type}.
    Type = atom()
  ```

  Specify a system registry that Erlang is to read configuration data from.
  `win32` is the only valid option.

- ```
  {host, IP, Aliases}.
    IP = tuple()
  ```

  `Aliases = [string()]`

  Add host entry to the hosts table.

- ```
  {domain, Domain}.
    Domain = string()
  ```

  Set domain name.

- ```
  {nameserver, IP [,Port]}.
    IP = tuple()
    Port = integer()
  ```

  Add address (and port, if other than default) of the primary nameserver to use
  for `m:inet_res`.

- ```
  {alt_nameserver, IP [,Port]}.
    IP = tuple()
    Port = integer()
  ```

  Add address (and port, if other than default) of the secondary nameserver for
  `m:inet_res`.

- ```
  {search, Domains}.
    Domains = [string()]
  ```

  Add search domains for `m:inet_res`.

- ```
  {lookup, Methods}.
    Methods = [atom()]
  ```

  Specify lookup methods and in which order to try them. The valid methods are
  as follows:

  - `native` (use system calls)
  - `file` (use host data retrieved from system configuration files and/or the
    user configuration file)
  - `dns` (use the Erlang DNS client `m:inet_res` for nameserver queries)

  The lookup method `string` tries to parse the hostname as an IPv4 or IPv6
  string and return the resulting IP address. It is automatically tried first
  when `native` is _not_ in the `Methods` list. To skip it in this case, the
  pseudo lookup method `nostring` can be inserted anywhere in the `Methods`
  list.

- ```
  {cache_size, Size}.
    Size = integer()
  ```

  Set the resolver cache size for `dns` lookups. `native` lookups are not
  cached. Defaults to 100 DNS records.

- ```
  {cache_refresh, Time}.
    Time = integer()
  ```

  Set how often (in milliseconds) the resolver cache for `m:inet_res` is
  refreshed (that is, expired DNS records are deleted). Defaults to 1 hour.

- ```
  {timeout, Time}.
    Time = integer()
  ```

  Set the time to wait until retry (in milliseconds) for DNS queries made by
  `m:inet_res`. Defaults to 2 seconds.

- ```
  {retry, N}.
    N = integer()
  ```

  Set the number of DNS queries `m:inet_res` will try before giving up. Defaults
  to 3.

- ```
  {servfail_retry_timeout, Time}.
    Time = non_neg_integer()
  ```

  After all name servers have been tried, there is a timeout before the name
  servers are tried again. This is to prevent the server from answering the
  query with what's in the servfail cache,
  [`inet_res`](`m:inet_res#servfail_retry_timeout`). Defaults to 1500 milli
  seconds .

- ```
  {inet6, Bool}.
    Bool = true | false
  ```

  Tells the DNS client `m:inet_res` to look up IPv6 addresses. Defaults to
  `false`.

- ```
  {usevc, Bool}.
    Bool = true | false
  ```

  Tells the DNS client `m:inet_res` to use TCP (Virtual Circuit) instead of UDP.
  Defaults to `false`.

- ```
  {edns, Version}.
    Version = false | 0
  ```

  Sets the EDNS version that `m:inet_res` will use. The only allowed version is
  zero. Defaults to `false`, which means not to use EDNS.

- ```
  {udp_payload_size, Size}.
    N = integer()
  ```

  Sets the allowed UDP payload size `m:inet_res` will advertise in EDNS queries.
  Also sets the limit when the DNS query will be deemed too large for UDP
  forcing a TCP query instead; this is not entirely correct, as the advertised
  UDP payload size of the individual nameserver is what is to be used, but this
  simple strategy will do until a more intelligent (probing, caching) algorithm
  needs to be implemented. Default to 1280, which stems from the standard
  Ethernet MTU size.

- ```
  {udp, Module}.
    Module = atom()
  ```

  Tell Erlang to use another primitive UDP module than `inet_udp`.

- ```
  {tcp, Module}.
    Module = atom()
  ```

  Tell Erlang to use another primitive TCP module than `inet_tcp`.

- ```
  clear_hosts.
  ```

  Clear the hosts table.

- ```
  clear_ns.
  ```

  Clear the list of recorded nameservers (primary and secondary).

- ```
  clear_search.
  ```

  Clear the list of search domains.

## User Configuration Example

Assume that a user does not want Erlang to use the native lookup method, but
wants Erlang to read all information necessary from start and use that for
resolving names and addresses. If lookup fails, Erlang is to request the data
from a nameserver (using the Erlang DNS client, set to use EDNS allowing larger
responses). The resolver configuration is updated when its configuration file
changes. Also, DNS records are never to be cached. The user configuration file
(in this example named `erl_inetrc`, stored in directory `./cfg_files`) can then
look as follows (Unix):

```erlang
%% -- ERLANG INET CONFIGURATION FILE --
%% read the hosts file
{file, hosts, "/etc/hosts"}.
%% add a particular host
{host, {134,138,177,105}, ["finwe"]}.
%% do not monitor the hosts file
{hosts_file, ""}.
%% read and monitor nameserver config from here
{resolv_conf, "/usr/local/etc/resolv.conf"}.
%% enable EDNS
{edns,0}.
%% disable caching
{cache_size, 0}.
%% specify lookup method
{lookup, [file, dns]}.
```

And Erlang can, for example, be started as follows:

```text
% erl -sname my_node -kernel inetrc '"./cfg_files/erl_inetrc"'
```
