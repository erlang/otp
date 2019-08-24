-module(maps_guard_receive).
-export([test/0]).

test() ->
    M0  = #{ id => 0 },
    Pid = spawn_link(fun() -> guard_receive_loop() end),
    Big = 36893488147419103229,
    B1  = <<"some text">>,
    B2  = <<"was appended">>,
    B3  = <<B1/binary, B2/binary>>,

    #{id:=1, res:=Big} = M1 = call(Pid, M0#{op=>sub,in=>{1 bsl 65, 3}}),
    #{id:=2, res:=26}  = M2 = call(Pid, M1#{op=>idiv,in=>{53,2}}),
    #{id:=3, res:=832} = M3 = call(Pid, M2#{op=>imul,in=>{26,32}}),
    #{id:=4, res:=4}   = M4 = call(Pid, M3#{op=>add,in=>{1,3}}),
    #{id:=5, res:=Big} = M5 = call(Pid, M4#{op=>sub,in=>{1 bsl 65, 3}}),
    #{id:=6, res:=B3}  = M6 = call(Pid, M5#{op=>"append",in=>{B1,B2}}),
    #{id:=7, res:=4}   = _  = call(Pid, M6#{op=>add,in=>{1,3}}),


    %% update old maps and check id update
    #{id:=2, res:=B3} = call(Pid, M1#{op=>"append",in=>{B1,B2}}),
    #{id:=5, res:=99} = call(Pid, M4#{op=>add,in=>{33, 66}}),

    %% cleanup
    done = call(Pid, done),
    ok.

call(Pid, M) ->
    Pid ! {self(), M}, receive {Pid, Res} -> Res end.

guard_receive_loop() ->
    receive
	{Pid, #{ id:=Id, op:="append", in:={X,Y}}=M} when is_binary(X), is_binary(Y) ->
	    Pid ! {self(), M#{ id=>Id+1, res=><<X/binary,Y/binary>>}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=add, in:={X,Y}}} ->
	    Pid ! {self(), #{ id=>Id+1, res=>X+Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=sub,  in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X-Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=idiv, in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X div Y}},
	    guard_receive_loop();
	{Pid, #{ id:=Id, op:=imul, in:={X,Y}}=M} ->
	    Pid ! {self(), M#{ id=>Id+1, res=>X * Y}},
	    guard_receive_loop();
	{Pid, done} ->
	    Pid ! {self(), done};
	{Pid, Other} ->
	    Pid ! {error, Other},
	    guard_receive_loop()
    end.
