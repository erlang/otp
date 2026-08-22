# ERL-1510 - No crypto support

## Example

```erlang

```

```

```

## Explanation

The current system is not configured with crypto support, but the code is trying to use code from the crypto application.

The error is most likely due to Erlang being installed without SSL support. To fix the issue you should consider re-installing Erlang using the `--with-ssl` option and ensuring OpenSSL is available for your system. Building Erlang without OpenSSL support causes the `crypto`, `ssl` and `ssh` applications not to work correctly and the public_key application to have very limited capabilities.

For further information on how to install Erlang, please refer to the official installation instructions.