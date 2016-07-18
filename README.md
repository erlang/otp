##Basho Erlang/OTP

This is the home of [Basho's][basho] version of [Erlang/OTP][erlang], forked from Ericsson's [repository][otp_repo].
You can _(and should!)_ read their [README][otp_readme] file for information on the language and applications.

###Branch Information

####This is not a supported branch, it is informational ONLY!

Basho OTP should be built from one of the following branches or tags.
If you're building for any use approximating production, we ***strongly*** encourage you to use a tagged production release.

|Status|Base|Branch|Release Tag|Stable B/T|
|:-----|:---|:-----|:----------|:---------|
| Production   | R16    | [`basho-otp-16`](http://github.com/basho/otp/tree/basho-otp-16) | [`OTP_R16B02_basho10`](http://github.com/basho/otp/tree/OTP_R16B02_basho10) | [`OTP_R16B02_basho10`](http://github.com/basho/otp/tree/OTP_R16B02_basho10) |
| Retired      | R15    | [`basho-otp-15`](http://github.com/basho/otp/tree/basho-otp-15) | [`basho_OTP_R15B01p`](http://github.com/basho/otp/tree/basho_OTP_R15B01p) | [`OTP_R15B01_basho1`](http://github.com/basho/otp/tree/OTP_R15B01_basho1) |
| Experimental | OTP-17 | [`basho-otp-17`](http://github.com/basho/otp/tree/basho-otp-17) | _n/a_ | [`basho-otp-17`](http://github.com/basho/otp/tree/basho-otp-17) |
| Experimental | OTP-18 | [`basho-otp-18`](http://github.com/basho/otp/tree/basho-otp-18) | _n/a_ | [`basho-otp-18`](http://github.com/basho/otp/tree/basho-otp-18) |
| Active       | OTP-19 | [`basho-otp-19`](http://github.com/basho/otp/tree/basho-otp-19) | _n/a_ | [`basho-otp-19`](http://github.com/basho/otp/tree/basho-otp-19) |

####Branch Conventions

All Basho branches are named with the prefix _basho_ - any other branch name is simply updated periodically, unchanged, from Ericsson's repository.

For any given Basho OTP _**NN**_ version, the current stable branch is `basho-otp-NN` and work-in-progress branches are `basho-otp-NN-some-descriptive-name`.
Updates from the main Erlang repository are merged from the appropriate maintenance branch into `basho-otp-NN` periodically, usually at tagged point releases.

Further information on branches can be found in README and Release Notes files on the branches themselves.

###What's Here

Our modifications to the original distribution generally fall into one or more of the following categories:

* ***Performance***<br />
  Our users care a lot about performance, and we do what we can to get the best out of our products running on Erlang/OTP.
* ***Security***<br />
  In general, we tighten up security in our releases where it makes sense for us to do so.
* ***Stability & Scalability***<br />
  Erlang/OTP is pretty stable and scalable, but when we find an area where we can improve it for running our applications, we do.

####Where it Works

Erlang/OTP is designed to run on a wide array of platforms, while our products are not.
As such, we only qualify our releases on 64-bit operating systems running on x86_64 processors.
Specific versions are listed for our products, but our focus is on relatively current versions of:

* FreeBSD
* Linux
* OS X
* SmartOS
* Solaris

#####Interoperability

Our releases should be fully interoperable with unmodified Erlang/OTP distributions, but not necessarily in their default configurations.
We _DO_ change a few default settings, but generally accept the same configuration options to set them explicitly.

#####YMMV

No, we don't do Windows, and we don't even do all available versions of the systems listed above.
While we do try to keep our changes as portable as the original distributions they're based on, we don't test beyond what our products support.

###Building and Installing

General information on building and installing Erlang/OTP can be found in the [$ERL_TOP/HOWTO/INSTALL.md][install] document.

Specific instructions for building Basho's OTP releases can be found in the README file ***on the branch*** you're building.

####Versions

Our version identifiers correlate to the Erlang/OTP release without the _basho_ suffix, but our changes to individual ERTS components and OTP applications _may not_ always carry distinct versions due to release process issues with older versions.
Our releases are intended to be used as a single cohesive installation, we do _NOT_ support mixing components between our releases and the original distributions.

###Contributing to Erlang/OTP

Unless you want to suggest a patch to our specific Erlang/OTP changes, if you find something you think needs to be changed you'll want to refer to the Erlang instructions for submitting [bug reports][otp_bugs] or [patches][otp_patching].

If your patch pertains specifically to our version, forking and creating a pull request on GitHub is the best way to get us to consider it.
Bear in mind, however, that our releases are tailored to our needs, so if it's not directly pertinent to how our users deploy Erlang/OTP, it may not be of interest to us.

###Copyright and License

Everything in Erlang/OTP, whether part of the original distribution or a contribution of ours, is subject to the terms of the license applied to it in Ericsson's repository.
Through OTP-17, that was the [Erlang Public License][eplicense].
Beginning with OTP-18, the [Apache License, Version 2.0][license] applies.


  [basho]:          http://www.basho.com
  [eplicense]:      http://www.erlang.org/EPLICENSE
  [erlang]:         http://www.erlang.org
  [install]:        HOWTO/INSTALL.md
  [license]:        LICENSE.txt
  [otp_bugs]:       https://github.com/erlang/otp/wiki/Bug-reports
  [otp_patching]:   http://wiki.github.com/erlang/otp/contribution-guidelines
  [otp_readme]:     https://github.com/erlang/otp/blob/maint/README.md
  [otp_repo]:       http://github.com/erlang/otp
