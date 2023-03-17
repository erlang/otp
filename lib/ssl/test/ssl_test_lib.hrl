-define(SSL_TEST_LIB_FORMAT, "(~s ~p:~p in ~p) ").
-define(SSL_TEST_LIB_ARGS,
        [erlang:pid_to_list(self()), ?MODULE, ?LINE, ?FUNCTION_NAME]).
-define(CT_LOG(F),
        (ct:log(?SSL_TEST_LIB_FORMAT ++ F, ?SSL_TEST_LIB_ARGS, [esc_chars]))).
-define(CT_LOG(F, Args),
        (ct:log(
           ?SSL_TEST_LIB_FORMAT ++ F,
           ?SSL_TEST_LIB_ARGS ++ Args,
           [esc_chars]))).
-define(CT_PAL(F),
        (ct:pal(?SSL_TEST_LIB_FORMAT ++ F, ?SSL_TEST_LIB_ARGS))).
-define(CT_PAL(F, Args),
        (ct:pal(?SSL_TEST_LIB_FORMAT ++ F, ?SSL_TEST_LIB_ARGS ++ Args))).
-define(CT_FAIL(F, Args),
        (ct:fail(?SSL_TEST_LIB_FORMAT ++ F, ?SSL_TEST_LIB_ARGS ++ Args))).
