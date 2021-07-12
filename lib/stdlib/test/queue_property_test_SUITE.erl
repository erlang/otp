%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(queue_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [
	new_case,
	is_queue_case,
	list_conversion_case,

	all_case,
	any_case,
	cons_case,
	daeh_case,
	delete_case,
	delete_r_case,
	delete_with_case,
	delete_with_r_case,
	drop_case,
	drop_r_case,
	filter_case,
	filtermap_case,
	fold_case,
	get_case,
	get_r_case,
	head_case,
	in_case,
	in_r_case,
	init_case,
	is_empty_case,
	join_case,
	last_case,
	len_case,
	liat_case,
	member_case,
	out_case,
	out_r_case,
	peek_case,
	peek_r_case,
	reverse_case,
	snoc_case,
	split_case,
	tail_case,

	ops_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

new_case(Config) ->
    do_proptest(prop_new, Config).

is_queue_case(Config) ->
    do_proptest(prop_is_queue, Config).

list_conversion_case(Config) ->
    do_proptest(prop_list_conversion, Config).

all_case(Config) ->
    do_proptest(prop_all, Config).

any_case(Config) ->
    do_proptest(prop_any, Config).

cons_case(Config) ->
    do_proptest(prop_cons, Config).

daeh_case(Config) ->
    do_proptest(prop_daeh, Config).

delete_case(Config) ->
    do_proptest(prop_delete, Config).

delete_r_case(Config) ->
    do_proptest(prop_delete_r, Config).

delete_with_case(Config) ->
    do_proptest(prop_delete_with, Config).

delete_with_r_case(Config) ->
    do_proptest(prop_delete_with_r, Config).

drop_case(Config) ->
    do_proptest(prop_drop, Config).

drop_r_case(Config) ->
    do_proptest(prop_drop_r, Config).

filter_case(Config) ->
    do_proptest(prop_filter, Config).

filtermap_case(Config) ->
    do_proptest(prop_filtermap, Config).

fold_case(Config) ->
    do_proptest(prop_fold, Config).

get_case(Config) ->
    do_proptest(prop_get, Config).

get_r_case(Config) ->
    do_proptest(prop_get_r, Config).

head_case(Config) ->
    do_proptest(prop_head, Config).

in_case(Config) ->
    do_proptest(prop_in, Config).

in_r_case(Config) ->
    do_proptest(prop_in_r, Config).

init_case(Config) ->
    do_proptest(prop_init, Config).

is_empty_case(Config) ->
    do_proptest(prop_is_empty, Config).

join_case(Config) ->
    do_proptest(prop_join, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

len_case(Config) ->
    do_proptest(prop_len, Config).

liat_case(Config) ->
    do_proptest(prop_liat, Config).

member_case(Config) ->
    do_proptest(prop_member, Config).

out_case(Config) ->
    do_proptest(prop_out, Config).

out_r_case(Config) ->
    do_proptest(prop_out_r, Config).

peek_case(Config) ->
    do_proptest(prop_peek, Config).

peek_r_case(Config) ->
    do_proptest(prop_peek_r, Config).

reverse_case(Config) ->
    do_proptest(prop_reverse, Config).

snoc_case(Config) ->
    do_proptest(prop_snoc, Config).

split_case(Config) ->
    do_proptest(prop_split, Config).

tail_case(Config) ->
    do_proptest(prop_tail, Config).

ops_case(Config) ->
    do_proptest(prop_ops, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
	queue_prop:Prop(),
	Config).
