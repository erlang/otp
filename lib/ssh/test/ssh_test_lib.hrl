%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2016-2025. All Rights Reserved.
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


%%-------------------------------------------------------------------------
%% Default system port
%%-------------------------------------------------------------------------
-ifndef(SSH_DEFAULT_PORT).
-define(SSH_DEFAULT_PORT, 22).
-endif.

%%-------------------------------------------------------------------------
%% Timeout time in ms
%%-------------------------------------------------------------------------
-define(TIMEOUT, 15000).
-define(v(Key, Config), proplists:get_value(Key, Config)).
-define(v(Key, Config, Default), proplists:get_value(Key, Config, Default)).

%%-------------------------------------------------------------------------
%% Check for usable crypto
%%-------------------------------------------------------------------------
-define(CHECK_CRYPTO(UsersInitCode),
	try
            application:start(crypto),
            ssh_test_lib:try_enable_fips_mode()
	of
            ok -> UsersInitCode;
            {skip,_} -> UsersInitCode;
            Other -> Other
	catch
            _:_ -> {skip, "Can't start crypto"}
	end
       ).

%%-------------------------------------------------------------------------
%% Help macro
%%-------------------------------------------------------------------------
-define(wait_match(Pattern, Guard, FunctionCall, Bind, Timeout, Ntries),
	Bind =
	    (fun() ->
		     F = fun(N, F1) ->
				 case FunctionCall of
				     Pattern when Guard -> Bind;
				     _ when N>0 ->
					 ct:log("Must sleep ~p ms at ~p:~p",
                                                [Timeout,?MODULE,?LINE]),
					 timer:sleep(Timeout),
					 F1(N-1, F1);
				     Other ->
					 ct:fail("Unexpected ~p:~p  ~p",
                                                 [?MODULE,?LINE,Other])
				 end
			 end,
		     F(Ntries, F)
	     end)()
       ).
-define(wait_match(Pattern, FunctionCall, Bind, Timeout, Ntries),
        ?wait_match(Pattern, true, FunctionCall, Bind, Timeout, Ntries)).
-define(wait_match(Pattern, FunctionCall, Timeout, Ntries),
        ?wait_match(Pattern, FunctionCall, ok, Timeout, Ntries)).
-define(wait_match(Pattern, FunctionCall, Bind),
        ?wait_match(Pattern, FunctionCall, Bind, 500, 10)).
-define(wait_match(Pattern, FunctionCall),
        ?wait_match(Pattern, FunctionCall, ok)).

-define(ct_log_show_file(File),
        (fun(File__) ->
                {ok,Contents__} = file:read_file(File__),
                ct:log("~p:~p Show file~n~s =~n~s~n",
                       [?MODULE,?LINE,File__, Contents__])
        end)(File)).

-define(SSH_TEST_LIB_FORMAT, "(~s ~p:~p in ~p) ").
-define(SSH_TEST_LIB_ARGS,
        [erlang:pid_to_list(self()), ?MODULE, ?LINE, ?FUNCTION_NAME]).
-define(CT_LOG(F),
        (ct:log(?SSH_TEST_LIB_FORMAT ++ F, ?SSH_TEST_LIB_ARGS, [esc_chars]))).
-define(CT_LOG(F, Args),
        (ct:log(
           ?SSH_TEST_LIB_FORMAT ++ F,
           ?SSH_TEST_LIB_ARGS ++ Args,
           [esc_chars]))).
-define(CT_PAL(F),
        (ct:pal(?SSH_TEST_LIB_FORMAT ++ F, ?SSH_TEST_LIB_ARGS))).
-define(CT_PAL(F, Args),
        (ct:pal(?SSH_TEST_LIB_FORMAT ++ F, ?SSH_TEST_LIB_ARGS ++ Args))).
-define(CT_FAIL(F, Args),
        (ct:fail(?SSH_TEST_LIB_FORMAT ++ F, ?SSH_TEST_LIB_ARGS ++ Args))).
