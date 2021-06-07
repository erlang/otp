-module(remotes1).

-compile(export_all).

-spec foo1(some_unknown_remote:type42()) -> ok.
foo1(ok) -> ok.

-spec foo2(ok) -> some_unknown_remote:type42().
foo2(ok) -> ok.

-spec foo3(some_known_remote:type42()) -> ok.
foo3(ok) -> ok.

-spec foo4(ok) -> some_known_remote:type42().
foo4(ok) -> ok.

-spec foo5(ok|ko) -> ok|ko.
foo5(ok) -> ok.

-spec foo6(ok|ko) -> ok.
foo6(ok) -> ok.

-type local_type_42() :: ok | ko.

-spec foo7(ok) -> local_type_42().
foo7(ok) -> ok.

-spec foo8(local_type_42()) -> ok.
foo8(ok) -> ok.

-type local_and_known_remote_type_42() :: some_known_remote:type42() | ok | ko.

-spec foo9(ok) -> local_and_known_remote_type_42().
foo9(ok) -> ok.

-spec foo10(local_and_known_remote_type_42()) -> ok.
foo10(ok) -> ok.

-type local_and_ok_known_remote_type_42() :: some_known_remote:type42() | ok.

-spec foo11(ok) -> local_and_ok_known_remote_type_42().
foo11(ok) -> ok.

-spec foo12(local_and_ok_known_remote_type_42()) -> ok.
foo12(ok) -> ok.

-type local_and_unknown_remote_type_42() :: some_unknown_remote:type42() | ok | ko.

-spec foo13(ok) -> local_and_unknown_remote_type_42().
foo13(ok) -> ok.

-spec foo14(local_and_unknown_remote_type_42()) -> ok.
foo14(ok) -> ok.

-type local_and_ok_unknown_remote_type_42() :: some_unknown_remote:type42() | ok.

-spec foo15(ok) -> local_and_ok_unknown_remote_type_42().
foo15(ok) -> ok.

-spec foo16(local_and_ok_unknown_remote_type_42()) -> ok.
foo16(ok) -> ok.
