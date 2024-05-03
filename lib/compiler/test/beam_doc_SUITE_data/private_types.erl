-module(private_types).

-export([bar/0, hidden/0, hidden_type_exposed/0, bounded/2]).
-export_type([public_t/0, hidden_export_t/0, complex/1, opaque_t/0]).

-type private_t() :: integer(). %% In chunk because referred to by exported bar/0
-type public_t() :: integer(). %% In chunk because exported
-opaque opaque_t() :: local_t() | private_t(). %% In chunk because exported,
                                               %% but local_t() not in chunk
-type private_cb_t() :: integer(). %% In chunk because referred to by callback
-type local_t() :: integer(). %% Not in chunk because only referred by non-exported function

-doc false.
-type hidden_export_t() :: integer(). %% In chunk because exported

-callback bar(private_cb_t()) -> ok.

-spec bar() -> private_t().
bar() -> baz().

-spec baz() -> local_t().
baz() -> 1.

-type hidden_t() :: integer(). %% Not in chunk because only referred to by hidden function

-doc false.
-spec hidden() -> hidden_t().
hidden() -> 1.

-doc false.
-type private() :: integer().

-spec hidden_type_exposed() -> private().
hidden_type_exposed() -> 1.

-type bounded_arg_t() :: integer().
-type arg_t() :: integer().
-type bounded_ret_t() :: integer().
-spec bounded(A :: arg_t(), B) -> C
              when B :: bounded_arg_t(),
                   C :: bounded_ret_t().
bounded(A, B) -> A + B.

-record(r,{ a :: record_a_t(), f :: record_f_t(), rec :: #r{} }). %% We have a recusive type to make sure we handle that
-type complex(A) ::
        [fun((fun_t()) -> fun_ret_t()) |
         fun((...) -> fun_ret_2_t()) |
         #{ map_key_t() := map_value_t(),
            map_key_2_t() => map_value_2_t() } |
         #r{ f :: record_inline_t() } |
         {tuple_t()} |
         maps:iterator_order(remote_type_t(A)) |
         [] | 1 .. 2
        ].

-type fun_t() :: integer().
-type fun_ret_t() :: integer().
-type fun_ret_2_t() :: integer().
-type map_key_t() :: integer().
-type map_value_t() :: integer().
-type map_key_2_t() :: integer().
-type map_value_2_t() :: integer().
-type record_inline_t() :: integer().
-type record_a_t() :: integer().
-type record_f_t() :: integer(). %% Should not be included as #r{ f } overrides it
-type tuple_t() :: integer().
-type remote_type_t(A) :: A.
