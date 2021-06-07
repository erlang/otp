-module(ct_property_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [prop_sort
         ].

%%% First prepare Config and compile the property tests for the found tool:
init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

%%%================================================================
%%% Test suites
%%%
prop_sort(Config) ->
    ct_property_test:quickcheck(
      ct_prop:prop_sort(),
      Config
     ).
