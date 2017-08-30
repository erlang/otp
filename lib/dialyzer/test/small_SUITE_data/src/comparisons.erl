-module(comparisons).

-compile(export_all).

-define(r, get(r)).

integer() -> integer(?r).
integer(X) when is_integer(X) -> X.

mfloat() -> float(?r).
mfloat(X) when is_float(X) -> X.

atom() -> atom(?r).
atom(X) when is_atom(X) -> X.

tuple() -> tuple(?r).
tuple(X) when is_tuple(X) -> X.

list() -> list(?r).
list(X) when is_list(X) -> X.

map() -> map(?r).
map(X) when is_map(X) -> #{}.

bitstring() -> bitstring(?r).
bitstring(X) when is_bitstring(X) -> <<0:63>>.

i() -> integer().
f() -> mfloat().
n() -> case ?r of 1 -> i(); 2 -> f() end.
a() -> atom().
t() -> tuple().
l() -> list().
m() -> map().
b() -> bitstring().
na() -> case ?r of 1 -> n(); 2 -> a() end.
at() -> case ?r of 1 -> t(); 2 -> a() end.
tl() -> case ?r of 1 -> t(); 2 -> l() end.

test_i_ll_i() -> case i()  < i() of true -> maybe; false -> maybe_too end.
test_i_le_i() -> case i() =< i() of true -> maybe; false -> maybe_too end.
test_i_gg_i() -> case i()  > i() of true -> maybe; false -> maybe_too end.
test_i_ge_i() -> case i() >= i() of true -> maybe; false -> maybe_too end.
test_i_ll_f() -> case i()  < f() of true -> maybe; false -> maybe_too end.
test_i_le_f() -> case i() =< f() of true -> maybe; false -> maybe_too end.
test_i_gg_f() -> case i()  > f() of true -> maybe; false -> maybe_too end.
test_i_ge_f() -> case i() >= f() of true -> maybe; false -> maybe_too end.
test_i_ll_n() -> case i()  < n() of true -> maybe; false -> maybe_too end.
test_i_le_n() -> case i() =< n() of true -> maybe; false -> maybe_too end.
test_i_gg_n() -> case i()  > n() of true -> maybe; false -> maybe_too end.
test_i_ge_n() -> case i() >= n() of true -> maybe; false -> maybe_too end.
test_i_ll_a() -> case i()  < a() of true -> always; false -> never end.
test_i_le_a() -> case i() =< a() of true -> always; false -> never end.
test_i_gg_a() -> case i()  > a() of true -> never; false -> always end.
test_i_ge_a() -> case i() >= a() of true -> never; false -> always end.
test_i_ll_t() -> case i()  < t() of true -> always; false -> never end.
test_i_le_t() -> case i() =< t() of true -> always; false -> never end.
test_i_gg_t() -> case i()  > t() of true -> never; false -> always end.
test_i_ge_t() -> case i() >= t() of true -> never; false -> always end.
test_i_ll_l() -> case i()  < l() of true -> always; false -> never end.
test_i_le_l() -> case i() =< l() of true -> always; false -> never end.
test_i_gg_l() -> case i()  > l() of true -> never; false -> always end.
test_i_ge_l() -> case i() >= l() of true -> never; false -> always end.
test_i_ll_m() -> case i()  < m() of true -> always; false -> never end.
test_i_le_m() -> case i() =< m() of true -> always; false -> never end.
test_i_gg_m() -> case i()  > m() of true -> never; false -> always end.
test_i_ge_m() -> case i() >= m() of true -> never; false -> always end.
test_i_ll_b() -> case i()  < b() of true -> always; false -> never end.
test_i_le_b() -> case i() =< b() of true -> always; false -> never end.
test_i_gg_b() -> case i()  > b() of true -> never; false -> always end.
test_i_ge_b() -> case i() >= b() of true -> never; false -> always end.

test_f_ll_i() -> case f()  < i() of true -> maybe; false -> maybe_too end.
test_f_le_i() -> case f() =< i() of true -> maybe; false -> maybe_too end.
test_f_gg_i() -> case f()  > i() of true -> maybe; false -> maybe_too end.
test_f_ge_i() -> case f() >= i() of true -> maybe; false -> maybe_too end.
test_f_ll_f() -> case f()  < f() of true -> maybe; false -> maybe_too end.
test_f_le_f() -> case f() =< f() of true -> maybe; false -> maybe_too end.
test_f_gg_f() -> case f()  > f() of true -> maybe; false -> maybe_too end.
test_f_ge_f() -> case f() >= f() of true -> maybe; false -> maybe_too end.
test_f_ll_n() -> case f()  < n() of true -> maybe; false -> maybe_too end.
test_f_le_n() -> case f() =< n() of true -> maybe; false -> maybe_too end.
test_f_gg_n() -> case f()  > n() of true -> maybe; false -> maybe_too end.
test_f_ge_n() -> case f() >= n() of true -> maybe; false -> maybe_too end.
test_f_ll_a() -> case f()  < a() of true -> always; false -> never end.
test_f_le_a() -> case f() =< a() of true -> always; false -> never end.
test_f_gg_a() -> case f()  > a() of true -> never; false -> always end.
test_f_ge_a() -> case f() >= a() of true -> never; false -> always end.
test_f_ll_t() -> case f()  < t() of true -> always; false -> never end.
test_f_le_t() -> case f() =< t() of true -> always; false -> never end.
test_f_gg_t() -> case f()  > t() of true -> never; false -> always end.
test_f_ge_t() -> case f() >= t() of true -> never; false -> always end.
test_f_ll_l() -> case f()  < l() of true -> always; false -> never end.
test_f_le_l() -> case f() =< l() of true -> always; false -> never end.
test_f_gg_l() -> case f()  > l() of true -> never; false -> always end.
test_f_ge_l() -> case f() >= l() of true -> never; false -> always end.
test_f_ll_m() -> case f()  < m() of true -> always; false -> never end.
test_f_le_m() -> case f() =< m() of true -> always; false -> never end.
test_f_gg_m() -> case f()  > m() of true -> never; false -> always end.
test_f_ge_m() -> case f() >= m() of true -> never; false -> always end.
test_f_ll_b() -> case f()  < b() of true -> always; false -> never end.
test_f_le_b() -> case f() =< b() of true -> always; false -> never end.
test_f_gg_b() -> case f()  > b() of true -> never; false -> always end.
test_f_ge_b() -> case f() >= b() of true -> never; false -> always end.

test_n_ll_i() -> case n()  < i() of true -> maybe; false -> maybe_too end.
test_n_le_i() -> case n() =< i() of true -> maybe; false -> maybe_too end.
test_n_gg_i() -> case n()  > i() of true -> maybe; false -> maybe_too end.
test_n_ge_i() -> case n() >= i() of true -> maybe; false -> maybe_too end.
test_n_ll_f() -> case n()  < f() of true -> maybe; false -> maybe_too end.
test_n_le_f() -> case n() =< f() of true -> maybe; false -> maybe_too end.
test_n_gg_f() -> case n()  > f() of true -> maybe; false -> maybe_too end.
test_n_ge_f() -> case n() >= f() of true -> maybe; false -> maybe_too end.
test_n_ll_n() -> case n()  < n() of true -> maybe; false -> maybe_too end.
test_n_le_n() -> case n() =< n() of true -> maybe; false -> maybe_too end.
test_n_gg_n() -> case n()  > n() of true -> maybe; false -> maybe_too end.
test_n_ge_n() -> case n() >= n() of true -> maybe; false -> maybe_too end.
test_n_ll_a() -> case n()  < a() of true -> always; false -> never end.
test_n_le_a() -> case n() =< a() of true -> always; false -> never end.
test_n_gg_a() -> case n()  > a() of true -> never; false -> always end.
test_n_ge_a() -> case n() >= a() of true -> never; false -> always end.
test_n_ll_t() -> case n()  < t() of true -> always; false -> never end.
test_n_le_t() -> case n() =< t() of true -> always; false -> never end.
test_n_gg_t() -> case n()  > t() of true -> never; false -> always end.
test_n_ge_t() -> case n() >= t() of true -> never; false -> always end.
test_n_ll_l() -> case n()  < l() of true -> always; false -> never end.
test_n_le_l() -> case n() =< l() of true -> always; false -> never end.
test_n_gg_l() -> case n()  > l() of true -> never; false -> always end.
test_n_ge_l() -> case n() >= l() of true -> never; false -> always end.
test_n_ll_m() -> case n()  < m() of true -> always; false -> never end.
test_n_le_m() -> case n() =< m() of true -> always; false -> never end.
test_n_gg_m() -> case n()  > m() of true -> never; false -> always end.
test_n_ge_m() -> case n() >= m() of true -> never; false -> always end.
test_n_ll_b() -> case n()  < b() of true -> always; false -> never end.
test_n_le_b() -> case n() =< b() of true -> always; false -> never end.
test_n_gg_b() -> case n()  > b() of true -> never; false -> always end.
test_n_ge_b() -> case n() >= b() of true -> never; false -> always end.

test_a_ll_i() -> case a()  < i() of true -> never; false -> always end.
test_a_le_i() -> case a() =< i() of true -> never; false -> always end.
test_a_gg_i() -> case a()  > i() of true -> always; false -> never end.
test_a_ge_i() -> case a() >= i() of true -> always; false -> never end.
test_a_ll_f() -> case a()  < f() of true -> never; false -> always end.
test_a_le_f() -> case a() =< f() of true -> never; false -> always end.
test_a_gg_f() -> case a()  > f() of true -> always; false -> never end.
test_a_ge_f() -> case a() >= f() of true -> always; false -> never end.
test_a_ll_n() -> case a()  < n() of true -> never; false -> always end.
test_a_le_n() -> case a() =< n() of true -> never; false -> always end.
test_a_gg_n() -> case a()  > n() of true -> always; false -> never end.
test_a_ge_n() -> case a() >= n() of true -> always; false -> never end.
test_a_ll_a() -> case a()  < a() of true -> maybe; false -> maybe_too end.
test_a_le_a() -> case a() =< a() of true -> maybe; false -> maybe_too end.
test_a_gg_a() -> case a()  > a() of true -> maybe; false -> maybe_too end.
test_a_ge_a() -> case a() >= a() of true -> maybe; false -> maybe_too end.
test_a_ll_t() -> case a()  < t() of true -> always; false -> never end.
test_a_le_t() -> case a() =< t() of true -> always; false -> never end.
test_a_gg_t() -> case a()  > t() of true -> never; false -> always end.
test_a_ge_t() -> case a() >= t() of true -> never; false -> always end.
test_a_ll_l() -> case a()  < l() of true -> always; false -> never end.
test_a_le_l() -> case a() =< l() of true -> always; false -> never end.
test_a_gg_l() -> case a()  > l() of true -> never; false -> always end.
test_a_ge_l() -> case a() >= l() of true -> never; false -> always end.
test_a_ll_m() -> case a()  < m() of true -> always; false -> never end.
test_a_le_m() -> case a() =< m() of true -> always; false -> never end.
test_a_gg_m() -> case a()  > m() of true -> never; false -> always end.
test_a_ge_m() -> case a() >= m() of true -> never; false -> always end.
test_a_ll_b() -> case a()  < b() of true -> always; false -> never end.
test_a_le_b() -> case a() =< b() of true -> always; false -> never end.
test_a_gg_b() -> case a()  > b() of true -> never; false -> always end.
test_a_ge_b() -> case a() >= b() of true -> never; false -> always end.

test_t_ll_i() -> case t()  < i() of true -> never; false -> always end.
test_t_le_i() -> case t() =< i() of true -> never; false -> always end.
test_t_gg_i() -> case t()  > i() of true -> always; false -> never end.
test_t_ge_i() -> case t() >= i() of true -> always; false -> never end.
test_t_ll_f() -> case t()  < f() of true -> never; false -> always end.
test_t_le_f() -> case t() =< f() of true -> never; false -> always end.
test_t_gg_f() -> case t()  > f() of true -> always; false -> never end.
test_t_ge_f() -> case t() >= f() of true -> always; false -> never end.
test_t_ll_n() -> case t()  < n() of true -> never; false -> always end.
test_t_le_n() -> case t() =< n() of true -> never; false -> always end.
test_t_gg_n() -> case t()  > n() of true -> always; false -> never end.
test_t_ge_n() -> case t() >= n() of true -> always; false -> never end.
test_t_ll_a() -> case t()  < a() of true -> never; false -> always end.
test_t_le_a() -> case t() =< a() of true -> never; false -> always end.
test_t_gg_a() -> case t()  > a() of true -> always; false -> never end.
test_t_ge_a() -> case t() >= a() of true -> always; false -> never end.
test_t_ll_t() -> case t()  < t() of true -> maybe; false -> maybe_too end.
test_t_le_t() -> case t() =< t() of true -> maybe; false -> maybe_too end.
test_t_gg_t() -> case t()  > t() of true -> maybe; false -> maybe_too end.
test_t_ge_t() -> case t() >= t() of true -> maybe; false -> maybe_too end.
test_t_ll_l() -> case t()  < l() of true -> always; false -> never end.
test_t_le_l() -> case t() =< l() of true -> always; false -> never end.
test_t_gg_l() -> case t()  > l() of true -> never; false -> always end.
test_t_ge_l() -> case t() >= l() of true -> never; false -> always end.
test_t_ll_m() -> case t()  < m() of true -> always; false -> never end.
test_t_le_m() -> case t() =< m() of true -> always; false -> never end.
test_t_gg_m() -> case t()  > m() of true -> never; false -> always end.
test_t_ge_m() -> case t() >= m() of true -> never; false -> always end.
test_t_ll_b() -> case t()  < b() of true -> always; false -> never end.
test_t_le_b() -> case t() =< b() of true -> always; false -> never end.
test_t_gg_b() -> case t()  > b() of true -> never; false -> always end.
test_t_ge_b() -> case t() >= b() of true -> never; false -> always end.

test_l_ll_i() -> case l()  < i() of true -> never; false -> always end.
test_l_le_i() -> case l() =< i() of true -> never; false -> always end.
test_l_gg_i() -> case l()  > i() of true -> always; false -> never end.
test_l_ge_i() -> case l() >= i() of true -> always; false -> never end.
test_l_ll_f() -> case l()  < f() of true -> never; false -> always end.
test_l_le_f() -> case l() =< f() of true -> never; false -> always end.
test_l_gg_f() -> case l()  > f() of true -> always; false -> never end.
test_l_ge_f() -> case l() >= f() of true -> always; false -> never end.
test_l_ll_n() -> case l()  < n() of true -> never; false -> always end.
test_l_le_n() -> case l() =< n() of true -> never; false -> always end.
test_l_gg_n() -> case l()  > n() of true -> always; false -> never end.
test_l_ge_n() -> case l() >= n() of true -> always; false -> never end.
test_l_ll_a() -> case l()  < a() of true -> never; false -> always end.
test_l_le_a() -> case l() =< a() of true -> never; false -> always end.
test_l_gg_a() -> case l()  > a() of true -> always; false -> never end.
test_l_ge_a() -> case l() >= a() of true -> always; false -> never end.
test_l_ll_t() -> case l()  < t() of true -> never; false -> always end.
test_l_le_t() -> case l() =< t() of true -> never; false -> always end.
test_l_gg_t() -> case l()  > t() of true -> always; false -> never end.
test_l_ge_t() -> case l() >= t() of true -> always; false -> never end.
test_l_ll_l() -> case l()  < l() of true -> maybe; false -> maybe_too end.
test_l_le_l() -> case l() =< l() of true -> maybe; false -> maybe_too end.
test_l_gg_l() -> case l()  > l() of true -> maybe; false -> maybe_too end.
test_l_ge_l() -> case l() >= l() of true -> maybe; false -> maybe_too end.
test_l_ll_m() -> case l()  < m() of true -> never; false -> always end.
test_l_le_m() -> case l() =< m() of true -> never; false -> always end.
test_l_gg_m() -> case l()  > m() of true -> always; false -> never end.
test_l_ge_m() -> case l() >= m() of true -> always; false -> never end.
test_l_ll_b() -> case l()  < b() of true -> always; false -> never end.
test_l_le_b() -> case l() =< b() of true -> always; false -> never end.
test_l_gg_b() -> case l()  > b() of true -> never; false -> always end.
test_l_ge_b() -> case l() >= b() of true -> never; false -> always end.

test_n_ll_na() -> case n()  < na() of true -> maybe; false -> maybe_too end.
test_n_le_na() -> case n() =< na() of true -> maybe; false -> maybe_too end.
test_n_gg_na() -> case n()  > na() of true -> maybe; false -> maybe_too end.
test_n_ge_na() -> case n() >= na() of true -> maybe; false -> maybe_too end.
test_n_ll_at() -> case n()  < at() of true -> always; false -> never end.
test_n_le_at() -> case n() =< at() of true -> always; false -> never end.
test_n_gg_at() -> case n()  > at() of true -> never; false -> always end.
test_n_ge_at() -> case n() >= at() of true -> never; false -> always end.
test_n_ll_tl() -> case n()  < tl() of true -> always; false -> never end.
test_n_le_tl() -> case n() =< tl() of true -> always; false -> never end.
test_n_gg_tl() -> case n()  > tl() of true -> never; false -> always end.
test_n_ge_tl() -> case n() >= tl() of true -> never; false -> always end.

test_a_ll_na() -> case a()  < na() of true -> maybe; false -> maybe_too end.
test_a_le_na() -> case a() =< na() of true -> maybe; false -> maybe_too end.
test_a_gg_na() -> case a()  > na() of true -> maybe; false -> maybe_too end.
test_a_ge_na() -> case a() >= na() of true -> maybe; false -> maybe_too end.
test_a_ll_at() -> case a()  < at() of true -> maybe; false -> maybe_too end.
test_a_le_at() -> case a() =< at() of true -> maybe; false -> maybe_too end.
test_a_gg_at() -> case a()  > at() of true -> maybe; false -> maybe_too end.
test_a_ge_at() -> case a() >= at() of true -> maybe; false -> maybe_too end.
test_a_ll_tl() -> case a()  < tl() of true -> always; false -> never end.
test_a_le_tl() -> case a() =< tl() of true -> always; false -> never end.
test_a_gg_tl() -> case a()  > tl() of true -> never; false -> always end.
test_a_ge_tl() -> case a() >= tl() of true -> never; false -> always end.

test_t_ll_na() -> case t()  < na() of true -> never; false -> always end.
test_t_le_na() -> case t() =< na() of true -> never; false -> always end.
test_t_gg_na() -> case t()  > na() of true -> always; false -> never end.
test_t_ge_na() -> case t() >= na() of true -> always; false -> never end.
test_t_ll_at() -> case t()  < at() of true -> maybe; false -> maybe_too end.
test_t_le_at() -> case t() =< at() of true -> maybe; false -> maybe_too end.
test_t_gg_at() -> case t()  > at() of true -> maybe; false -> maybe_too end.
test_t_ge_at() -> case t() >= at() of true -> maybe; false -> maybe_too end.
test_t_ll_tl() -> case t()  < tl() of true -> maybe; false -> maybe_too end.
test_t_le_tl() -> case t() =< tl() of true -> maybe; false -> maybe_too end.
test_t_gg_tl() -> case t()  > tl() of true -> maybe; false -> maybe_too end.
test_t_ge_tl() -> case t() >= tl() of true -> maybe; false -> maybe_too end.

test_l_ll_na() -> case l()  < na() of true -> never; false -> always end.
test_l_le_na() -> case l() =< na() of true -> never; false -> always end.
test_l_gg_na() -> case l()  > na() of true -> always; false -> never end.
test_l_ge_na() -> case l() >= na() of true -> always; false -> never end.
test_l_ll_at() -> case l()  < at() of true -> never; false -> always end.
test_l_le_at() -> case l() =< at() of true -> never; false -> always end.
test_l_gg_at() -> case l()  > at() of true -> always; false -> never end.
test_l_ge_at() -> case l() >= at() of true -> always; false -> never end.
test_l_ll_tl() -> case l()  < tl() of true -> maybe; false -> maybe_too end.
test_l_le_tl() -> case l() =< tl() of true -> maybe; false -> maybe_too end.
test_l_gg_tl() -> case l()  > tl() of true -> maybe; false -> maybe_too end.
test_l_ge_tl() -> case l() >= tl() of true -> maybe; false -> maybe_too end.

test_na_ll_n() -> case na()  < n() of true -> maybe; false -> maybe_too end.
test_na_le_n() -> case na() =< n() of true -> maybe; false -> maybe_too end.
test_na_gg_n() -> case na()  > n() of true -> maybe; false -> maybe_too end.
test_na_ge_n() -> case na() >= n() of true -> maybe; false -> maybe_too end.
test_na_ll_a() -> case na()  < a() of true -> maybe; false -> maybe_too end.
test_na_le_a() -> case na() =< a() of true -> maybe; false -> maybe_too end.
test_na_gg_a() -> case na()  > a() of true -> maybe; false -> maybe_too end.
test_na_ge_a() -> case na() >= a() of true -> maybe; false -> maybe_too end.
test_na_ll_t() -> case na()  < t() of true -> always; false -> never end.
test_na_le_t() -> case na() =< t() of true -> always; false -> never end.
test_na_gg_t() -> case na()  > t() of true -> never; false -> always end.
test_na_ge_t() -> case na() >= t() of true -> never; false -> always end.
test_na_ll_l() -> case na()  < l() of true -> always; false -> never end.
test_na_le_l() -> case na() =< l() of true -> always; false -> never end.
test_na_gg_l() -> case na()  > l() of true -> never; false -> always end.
test_na_ge_l() -> case na() >= l() of true -> never; false -> always end.

test_at_ll_n() -> case at()  < n() of true -> never; false -> always end.
test_at_le_n() -> case at() =< n() of true -> never; false -> always end.
test_at_gg_n() -> case at()  > n() of true -> always; false -> never end.
test_at_ge_n() -> case at() >= n() of true -> always; false -> never end.
test_at_ll_a() -> case at()  < a() of true -> maybe; false -> maybe_too end.
test_at_le_a() -> case at() =< a() of true -> maybe; false -> maybe_too end.
test_at_gg_a() -> case at()  > a() of true -> maybe; false -> maybe_too end.
test_at_ge_a() -> case at() >= a() of true -> maybe; false -> maybe_too end.
test_at_ll_t() -> case at()  < t() of true -> maybe; false -> maybe_too end.
test_at_le_t() -> case at() =< t() of true -> maybe; false -> maybe_too end.
test_at_gg_t() -> case at()  > t() of true -> maybe; false -> maybe_too end.
test_at_ge_t() -> case at() >= t() of true -> maybe; false -> maybe_too end.
test_at_ll_l() -> case at()  < l() of true -> always; false -> never end.
test_at_le_l() -> case at() =< l() of true -> always; false -> never end.
test_at_gg_l() -> case at()  > l() of true -> never; false -> always end.
test_at_ge_l() -> case at() >= l() of true -> never; false -> always end.

test_tl_ll_n() -> case tl()  < n() of true -> never; false -> always end.
test_tl_le_n() -> case tl() =< n() of true -> never; false -> always end.
test_tl_gg_n() -> case tl()  > n() of true -> always; false -> never end.
test_tl_ge_n() -> case tl() >= n() of true -> always; false -> never end.
test_tl_ll_a() -> case tl()  < a() of true -> never; false -> always end.
test_tl_le_a() -> case tl() =< a() of true -> never; false -> always end.
test_tl_gg_a() -> case tl()  > a() of true -> always; false -> never end.
test_tl_ge_a() -> case tl() >= a() of true -> always; false -> never end.
test_tl_ll_t() -> case tl()  < t() of true -> maybe; false -> maybe_too end.
test_tl_le_t() -> case tl() =< t() of true -> maybe; false -> maybe_too end.
test_tl_gg_t() -> case tl()  > t() of true -> maybe; false -> maybe_too end.
test_tl_ge_t() -> case tl() >= t() of true -> maybe; false -> maybe_too end.
test_tl_ll_l() -> case tl()  < l() of true -> maybe; false -> maybe_too end.
test_tl_le_l() -> case tl() =< l() of true -> maybe; false -> maybe_too end.
test_tl_gg_l() -> case tl()  > l() of true -> maybe; false -> maybe_too end.
test_tl_ge_l() -> case tl() >= l() of true -> maybe; false -> maybe_too end.

test_na_ll_na() -> case na()  < na() of true -> maybe; false -> maybe_too end.
test_na_le_na() -> case na() =< na() of true -> maybe; false -> maybe_too end.
test_na_gg_na() -> case na()  > na() of true -> maybe; false -> maybe_too end.
test_na_ge_na() -> case na() >= na() of true -> maybe; false -> maybe_too end.
test_na_ll_at() -> case na()  < at() of true -> maybe; false -> maybe_too end.
test_na_le_at() -> case na() =< at() of true -> maybe; false -> maybe_too end.
test_na_gg_at() -> case na()  > at() of true -> maybe; false -> maybe_too end.
test_na_ge_at() -> case na() >= at() of true -> maybe; false -> maybe_too end.
test_na_ll_tl() -> case na()  < tl() of true -> always; false -> never end.
test_na_le_tl() -> case na() =< tl() of true -> always; false -> never end.
test_na_gg_tl() -> case na()  > tl() of true -> never; false -> always end.
test_na_ge_tl() -> case na() >= tl() of true -> never; false -> always end.

test_at_ll_na() -> case at()  < na() of true -> maybe; false -> maybe_too end.
test_at_le_na() -> case at() =< na() of true -> maybe; false -> maybe_too end.
test_at_gg_na() -> case at()  > na() of true -> maybe; false -> maybe_too end.
test_at_ge_na() -> case at() >= na() of true -> maybe; false -> maybe_too end.
test_at_ll_at() -> case at()  < at() of true -> maybe; false -> maybe_too end.
test_at_le_at() -> case at() =< at() of true -> maybe; false -> maybe_too end.
test_at_gg_at() -> case at()  > at() of true -> maybe; false -> maybe_too end.
test_at_ge_at() -> case at() >= at() of true -> maybe; false -> maybe_too end.
test_at_ll_tl() -> case at()  < tl() of true -> maybe; false -> maybe_too end.
test_at_le_tl() -> case at() =< tl() of true -> maybe; false -> maybe_too end.
test_at_gg_tl() -> case at()  > tl() of true -> maybe; false -> maybe_too end.
test_at_ge_tl() -> case at() >= tl() of true -> maybe; false -> maybe_too end.

test_tl_ll_na() -> case tl()  < na() of true -> never; false -> always end.
test_tl_le_na() -> case tl() =< na() of true -> never; false -> always end.
test_tl_gg_na() -> case tl()  > na() of true -> always; false -> never end.
test_tl_ge_na() -> case tl() >= na() of true -> always; false -> never end.
test_tl_ll_at() -> case tl()  < at() of true -> maybe; false -> maybe_too end.
test_tl_le_at() -> case tl() =< at() of true -> maybe; false -> maybe_too end.
test_tl_gg_at() -> case tl()  > at() of true -> maybe; false -> maybe_too end.
test_tl_ge_at() -> case tl() >= at() of true -> maybe; false -> maybe_too end.
test_tl_ll_tl() -> case tl()  < tl() of true -> maybe; false -> maybe_too end.
test_tl_le_tl() -> case tl() =< tl() of true -> maybe; false -> maybe_too end.
test_tl_gg_tl() -> case tl()  > tl() of true -> maybe; false -> maybe_too end.
test_tl_ge_tl() -> case tl() >= tl() of true -> maybe; false -> maybe_too end.
