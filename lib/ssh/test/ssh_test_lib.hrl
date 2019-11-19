%%-------------------------------------------------------------------------
%% Timeout time in ms
%%-------------------------------------------------------------------------
-define(TIMEOUT, 27000).

%%-------------------------------------------------------------------------
%% Check for usable crypt 
%%-------------------------------------------------------------------------
-define(CHECK_CRYPTO(UsersInitCode),
	try
            crypto:start(),
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
					 ct:pal("Must sleep ~p ms at ~p:~p",[Timeout,?MODULE,?LINE]),
					 timer:sleep(Timeout),
					 F1(N-1, F1);
				     Other ->  
					 ct:fail("Unexpected ~p:~p  ~p",[?MODULE,?LINE,Other])
				 end
			 end,
		     F(Ntries, F)
	     end)()
       ).

-define(wait_match(Pattern, FunctionCall, Bind, Timeout, Ntries),
        ?wait_match(Pattern, true, FunctionCall, Bind, Timeout, Ntries)).

-define(wait_match(Pattern, FunctionCall, Timeout, Ntries),  ?wait_match(Pattern, FunctionCall, ok, Timeout, Ntries)).

-define(wait_match(Pattern, FunctionCall, Bind),  ?wait_match(Pattern, FunctionCall, Bind, 500, 10) ).

-define(wait_match(Pattern, FunctionCall),  ?wait_match(Pattern, FunctionCall, ok) ).

