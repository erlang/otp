# Deprecate

This HOWTO shows how to deprecate functionality from Erlang/OTP.

When adding a new *deprecation* or *removal* warning you need to add an attribute to
the module in question and update [$ERL_TOP/system/doc/general_info/DEPRECATIONS][1]
with information about when the deprecation was added.

After changing or adding an attribute, and updating the [`DEPRECATIONS`][1],
you need to update the internal state by running:

    $ ./otp_build update_deprecations

This will update the documentation and the central list of deprecated and removed
interfaces ([`otp_internal.erl`][2]).

## Attribute format

To mark a function/type as deprecated or removed, point out its name and arity
together with a suggestion on what the user should do instead:

    -deprecated([{now,0,
                  "see the \"Time and Time Correction in Erlang\" "
                  "chapter of the ERTS User's Guide for more information"}]).
    
    -deprecated([{cmac, 3, "use crypto:mac/4 instead"},
                 {cmac, 4, "use crypto:macN/5 instead"}]).
    
    -removed([{md5_mac,2,"use crypto:hmac/3 instead"},
              {md5_mac_96,2,"use crypto:hmac/4 instead"}]).
    
    -deprecated_type([{gadget,1,"use widget/1 instead"}]).
    
    -removed_type([{column,0,"use erl_anno:column() instead"},
                   {line,0,"use erl_anno:line() instead"},
                   {location,0,"use erl_anno:location() instead"}]).

Wildcards can be used to match all names and/or arities:

    -removed([{rsa_sign,'_',"use crypto:sign/4 instead"},
              {rsa_verify,'_',"use crypto:verify/5 instead"}]).
    
    -deprecated([{next_iv, '_',"see the 'New and Old API' chapter of the CRYPTO User's guide"}]).
    
    -deprecated([{'_','_',"use the 'rand' module instead"}]).
    
    -deprecated_type([{grunka,'_',"use frobnitz/1,2 instead"}]).

You can also use the `Name/Arity` shorthand for all of these variants, e.g.
`-deprecated([f/1])`, which will result in a generic description to the effect of
"see the documentation for details."

Note that it is not possible to warn about a module that no longer exists.
This is to prevent later namespace clashes from raising warnings, like the ones
we had when the long-removed `net` module reappeared in the socket NIF.
"Removed" modules should instead have their contents replaced by
`-removed()` attributes until there's no longer any point in warning
about their use.

## The DEPRECATIONS file

The [$ERL_TOP/system/doc/general_info/DEPRECATIONS][1] file contains additional
information about each deprecated function, namely in what release it was deprecated
and optionally in what release it will be removed. The information in this file will
be used to generate the [Deprecations](http://erlang.org/doc/general_info/deprecations.html),
[Scheduled for Removal](http://erlang.org/doc/general_info/scheduled_for_removal.html),
and [Removed Functionality](http://erlang.org/doc/general_info/removed.html),
pages in the documentation.

Here is how the entry for `erlang:now/0` that was deprecated in OTP 18 looks like:

    erlang:now/0 since=18

Here is an example of a function that was deprecated in OTP 23 and is scheduled for removal in OTP 25:

    filename:safe_relative_path/1 since=23 remove=25

After removing a function it's important to keep its line in the DEPRECATIONS
file for as long as we wish to raise warnings about it. Should a removal be
postponed, the corresponding `remove` attribute must be bumped accordingly.

 [1]: ../system/doc/general_info/DEPRECATIONS
 [2]: ../lib/stdlib/src/otp_internal.erl
