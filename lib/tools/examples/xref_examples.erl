-module(xref_examples).

-export([script/0]).

%% Used at Erlang/OTP for finding undefined functions and unused local
%% functions. Output are the two files ${HOME}/undefined.txt and
%% ${HOME}/unused_locals.txt.
script() ->
    Root = code:root_dir(),
    {ok,[[Dir]]} = init:get_argument(home),
    Server = s,
    xref:start(Server),
    {ok, _Relname} = xref:add_release(Server, code:lib_dir(), {name,otp}),
    %% Exclude undefined functions in some modules...
    Exclude = "(CORBA|Cos|Orber|Puller|Pusher|"
	      "StackModule|oe_Cos|mnesia).*_impl",
    UndefS = "XC || (XU - X - B)",
    Q = io_lib:format("Undef = ~s,"
		      "Excluded = ~p:_/_,"
		      "Undef - Undef || Excluded", 
		      [UndefS, Exclude]),
    {ok, Undef} = xref:q(Server, lists:flatten(Q)),
    {ok, NotCalled} = xref:analyze(Server, locals_not_used),
    dump("%% " ++ Root ++ 
	 "\n%% Undefined external functions." ++ 
	 "\n%% The second MFA is the undefined function." ++
	 "\n%% Functions in modules matching the following "
	      "regular expression have been skipped:" ++ 
	 "\n%% " ++ Exclude,
	 filename:join(Dir, "undefined.txt"), 
	 Undef),
    dump("%% " ++ Root ++ "\n%% Unused local functions.",
	 filename:join(Dir, "unused_locals.txt"), 
	 NotCalled),
    catch xref:stop(Server),
    halt().

dump(H, F, T) ->
    {ok, IoDev} = file:open(F,[write]),
    io:format(IoDev, "~s~n", [H]),
    io:format(IoDev, "~p.~n", [T]),
    file:close(IoDev).
