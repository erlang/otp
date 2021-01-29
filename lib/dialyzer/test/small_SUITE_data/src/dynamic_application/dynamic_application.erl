-module(dynamic_application).

-export([test_detect_bad_application_if_all_modules_have_wrong_type/0]).
-export([test_detect_bad_application_if_all_modules_have_wrong_type_or_missing_function/0]).
-export([test_accept_application_if_some_modules_are_ok/0]).

% This test verifies that function calls on (potentially multiple) dynamically
% provided modules are checked for incorrect argument types or entirely missing
% functions.

test_detect_bad_application_if_all_modules_have_wrong_type() ->
    invoke_all_wrong_type(module_func_wrong_arg_type_list),
    invoke_all_wrong_type(module_func_wrong_arg_type_number).

test_detect_bad_application_if_all_modules_have_wrong_type_or_missing_function() ->
    invoke_all_wrong_type_or_missing(module_func_missing),
    invoke_all_wrong_type_or_missing(module_func_wrong_arg_type_list),
    invoke_all_wrong_type_or_missing(module_func_wrong_arg_type_number).

test_accept_application_if_some_modules_are_ok() ->
    invoke_some_ok(module_func_wrong_arg_type_list),
    invoke_some_ok(module_func_wrong_arg_type_number),
    invoke_some_ok(module_func_ok).

-spec invoke_all_wrong_type_or_missing(module_func_wrong_arg_type_list | module_func_wrong_arg_type_number | module_func_missing) -> term().
invoke_all_wrong_type_or_missing(Module) ->
    Module:func(an_arg).

-spec invoke_all_wrong_type(module_func_wrong_arg_type_list | module_func_wrong_arg_type_number) -> term().
invoke_all_wrong_type(Module) ->
    Module:func(an_arg).

-spec invoke_some_ok(module_func_wrong_arg_type_list | module_func_wrong_arg_type_number | module_func_ok) -> term().
invoke_some_ok(Module) ->
    Module:func(an_arg).
