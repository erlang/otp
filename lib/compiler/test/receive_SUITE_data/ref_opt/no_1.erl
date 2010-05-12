-module(no_1).
-compile(export_all).

?MODULE() ->
    ok.

f1(X) ->
    Ref = make_ref(),
    receive
	_ when [X] =:= Ref ->
	    ok
    end.

f2(X, Y) ->
    _Ref = make_ref(),
    receive
	_ when X =:= Y ->
	    ok
    end.

f3(X) ->
    Ref = make_ref(),
    receive
	_ when X =:= Ref ->
	    ok
    end.

f4(X) ->
    Ref = make_ref(),
    receive
	{X,_} when not X =:= Ref ->
	    ok
    end.

f5(X) ->
    Ref = make_ref(),
    receive
	{Y,_} when X =:= Y; Y =:= Ref ->
	    ok
    end.

f6(X) ->
    Ref = make_ref(),
    receive
	{Y,_} when Y =:= Ref; Ref =:= X ->
	    ok
    end.

f7(X) ->
    Ref = make_ref(),
    receive
	{Y,_} when Y =:= Ref; not (X =:= Ref) ->
	    ok
    end.

f8(X) ->
    Ref = make_ref(),
    receive
	{Y,_} when not (X =:= Ref); Y =:= Ref ->
	    ok
    end.

f9(X) ->
    Ref = make_ref(),
    receive
	{Y,_} when (not (X =:= Ref)) or (Y =:= Ref) ->
	    ok
    end.

f10(X, Y) ->
    Ref = make_ref(),
    receive
	{Z,_} when not (X =:= Y andalso Z =:= Ref) ->
	    ok
    end.

f11(X, Y) ->
    Ref = make_ref(),
    receive
	{Z,_} when not ((X =:= Y) and (Z =:= Ref)) ->
	    ok
    end.

f12(X, Y) ->
    Ref = make_ref(),
    receive
	{Z,_} when not ((Z =:= Ref) and (X =:= Y)) ->
	    ok
    end.

f13() ->
    Ref = make_ref(),
    RefCopy = id(Ref),
    receive
	_ when hd([RefCopy]) =:= Ref ->
	    ok
    end.

id(I) -> I.
