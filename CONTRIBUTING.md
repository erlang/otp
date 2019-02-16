# Contributing to Erlang/OTP

## License

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the 
    best of my knowledge, is covered under an appropriate open 
    source license and I have the right under that license to   
    submit that work with modifications, whether created in whole
    or in part by me, under the same open source license (unless
    I am permitted to submit under a different license), as 
    Indicated in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including 
    all personal information I submit with it, including my
    sign-off) is maintained indefinitely and may be redistributed
    consistent with this project or the open source license(s)
    involved.

Erlang/otp is licensed under the
Apache License 2.0

As stated in: LICENSE.txt 

http://developercertificate.org/

## Reporting a bug

Report bugs at https://bugs.erlang.org. See [Bug reports](https://github.com/erlang/otp/wiki/Bug-reports)
for more information.

## Submitting Pull Requests

You can contribute to Erlang/OTP by opening a Pull Request.

Make sure you create a new branch for your pull request with `git checkout -b new-branch-name`.
Give the branch a short but descriptive name, like `stdlib/lists-length-fix`.
Never do your work directly on `maint` or `master`.

## Fixing a bug

* In most cases, pull requests for bug fixes should be based on the `maint` branch.
There are exceptions, for example corrections to bugs that have been introduced in the `master` branch.

* Include a test case to ensure that the bug is fixed **and that it stays fixed**.

* TIP: Write the test case **before** fixing the bug so that you can know that it catches the bug.

* For applications without a test suite in the git repository, it would be appreciated if you provide a
small code sample in the commit message or email a module that will provoke the failure.

## Adding a new feature

* In most cases, pull requests for new features should be based on the `master` branch.

* It is recommended to discuss new features on
[the erlang-questions mailing list](http://erlang.org/mailman/listinfo/erlang-questions),
especially for major new features or any new features in ERTS, Kernel, or STDLIB.

* It is important to write a good commit message explaining **why** the feature is needed.
We prefer that the information is in the commit message, so that anyone that want to know 
two years later why a particular feature can easily find out. It does no harm to provide
the same information in the pull request (if the pull request consists of a single commit,
the commit message will be added to the pull request automatically).

* With few exceptions, it is mandatory to write a new test case that tests the feature.
The test case is needed to ensure that the features does not stop working in the future.

* Update the [Documentation](https://github.com/erlang/otp/wiki/Documentation) to describe the feature.

* Make sure that the new feature builds and works on all major platforms. Exceptions are features
that only makes sense one some platforms, for example the `win32reg` module for accessing the Windows registry.

* Make sure that your feature does not break backward compatibility. In general, we only break backward
compatibility in major releases and only for a very good reason. Usually we first deprecate the
feature one or two releases beforehand.

* In general, language changes/extensions require an
[EEP (Erlang Enhancement Proposal)](https://github.com/erlang/eep) to be written and approved before they 
can be included in OTP. Major changes or new features in ERTS, Kernel, or STDLIB will need an EEP or at least
a discussion on the mailing list.

## Before you submit your pull request

* Make sure existing test cases don't fail. It is not necessary to run all tests (that would take many hours),
but you should at least run the tests for the application you have changed.
See [Running tests](https://github.com/erlang/otp/wiki/Running-tests).

Make sure that your branch contains clean commits:

* Don't make the first line in the commit message longer than 72 characters.
**Don't end the first line with a period.**

* Follow the guidelines for [Writing good commit messages](https://github.com/erlang/otp/wiki/Writing-good-commit-messages).

* Don't merge `maint` or `master` into your branch. Use `git rebase` if you need to resolve merge
conflicts or include the latest changes.

* To make it possible to use the powerful `git bisect` command, make sure that each commit can be
compiled and that it works.

* Check for unnecessary whitespace before committing with `git diff --check`.
However, do not fix preexisting whitespace errors in otherwise untouched source lines.

Check your coding style:

* Make sure your changes follow the coding and indentation style of the code surrounding your changes.

* Do not commit commented-out code or files that are no longer needed. Remove the code or the files.

* In most code (Erlang and C), indentation is 4 steps. Indentation using only spaces is **strongly recommended**.

### Configuring Emacs

If you use Emacs, use the Erlang mode, and add the following lines to `.emacs`:

    (setq-default indent-tabs-mode nil)
    (setq c-basic-offset 4)

If you want to change the setting only for the Erlang mode, you can use a hook like this:

```
(add-hook 'erlang-mode-hook 'my-erlang-hook)

(defun my-erlang-hook ()
  (setq indent-tabs-mode nil))
```
