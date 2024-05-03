### Crypto Old API

The [Old API](`e:crypto:new_api.md#the-old-api`) is deprecated as of OTP 23 and
has been [removed in OTP 24](removed.md#otp-24).

For replacement functions see the [New API](`e:crypto:new_api.md#the-new-api`).

### http_uri

Since OTP 21 the recommended module to handle URIs is `m:uri_string`. The module
http_uri does not provide a implementation that satisfies the RFC.

### ssh

The public key algorithm `'ssh-rsa` is regarded as insecure due to its usage of
SHA1, and is therefore deprecated. It will not be available by default from
OTP-24.

The public key algorithm `'ssh-dss` is regarded as insecure due to its usage of
SHA1 and its short key length, and is therefore deprecated. It is not available
by default from OTP-23.

### Distributed Disk Logs

As of OTP 23, the distributed `m:disk_log` feature has been deprecated and it
has also been [removed in OTP 24](removed.md#otp-24).

### erl_interface registry

As of OTP 23, the `registry` functionality part of `erl_interface` has been
deprecated and it has also been [removed in OTP 24](removed.md#otp-24).

### Functions Deprecated in OTP 23

-   `http_uri:decode/1` (use uri_string:unquote function instead)
-   `http_uri:encode/1` (use uri_string:quote function instead)
-   `httpd:parse_query/1` (use uri_string:dissect_query/1 instead)
