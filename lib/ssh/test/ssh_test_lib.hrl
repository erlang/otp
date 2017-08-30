%%-------------------------------------------------------------------------
%% Check for usable crypt 
%%-------------------------------------------------------------------------
-define(CHECK_CRYPTO(Available),
	try crypto:start() 
	of _ -> Available
	catch _:_ -> {skip, "Can't start crypto"}
	end
       ).

%%-------------------------------------------------------------------------
%% Help macro
%%-------------------------------------------------------------------------
-define(wait_match(Pattern, FunctionCall, Bind, Timeout, Ntries),
	Bind =
	    (fun() -> 
		     F = fun(N, F1) ->
				 case FunctionCall of
				     Pattern -> Bind;
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

-define(wait_match(Pattern, FunctionCall, Timeout, Ntries),  ?wait_match(Pattern, FunctionCall, ok, Timeout, Ntries)).

-define(wait_match(Pattern, FunctionCall, Bind),  ?wait_match(Pattern, FunctionCall, Bind, 500, 10) ).

-define(wait_match(Pattern, FunctionCall),  ?wait_match(Pattern, FunctionCall, ok) ).

