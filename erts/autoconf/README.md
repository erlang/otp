All files in this directory except for the README.md files are copies
of primary files located in the `$ERL_TOP/make/autoconf` directory.
Files in this directory are updated automatically when executing
`$ERL_TOP/otp_build update_configure [--no-commit]`.

The files in this directory are only kept here in order not to break
external scripts that might depend on them being here. You typically
want to use the files in the `$ERL_TOP/make/autoconf` directory and
*not* the ones in this directory. The files in this directory will
eventually be removed.
