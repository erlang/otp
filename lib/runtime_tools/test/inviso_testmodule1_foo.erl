-module(inviso_testmodule1_foo).

-compile(export_all).

%% The purpose of this module is simply to have a module that is
%% guaranteed not loaded.

foo() ->
    true.
