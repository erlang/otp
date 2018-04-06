-module(receive_stacked).
-compile([export_all,nowarn_export_all]).

%% Messages may be stored outside any process heap until they
%% have been accepted by the 'remove_message' instruction.
%% When matching of a message fails, it is not allowed to
%% leave references to the message or any part of it in
%% the Y registers. An experimental code generator could
%% do that, causing an emulator crash if there happenened to
%% be a garbage collection.
%%
%% The 'S' file corresponding to this file was compiled with
%% that experimental code generator.

f1() ->
    receive
        X when is_integer(X) ->
            id(42),
            X
    end.

f2() ->
    receive
        [X] ->
            Res = {ok,X},
            id(42),
            {Res,X}
    end.

f3() ->
    receive
        [H|_] when is_integer(H) ->
            Res = {ok,H},
            id(42),
            {Res,H}
    end.

f4() ->
    receive
        [_|T] when is_list(T) ->
            Res = {ok,T},
            id(42),
            {Res,T}
    end.

f5() ->
    receive
        {X} when is_integer(X) ->
            Res = #{key=>X},
            id(42),
            {Res,X}
    end.

f6() ->
    receive
        <<_:8,T/binary>> when byte_size(T) > 8 ->
            id(42),
            T
    end.

f7() ->
    receive
        <<_:8,T/binary>> when is_binary(T) ->
            id(42),
            T
    end.

f8() ->
    receive
        <<_:8,T/binary>> = Bin when is_binary(Bin) ->
            id(42),
            T
    end.

m1() ->
    receive
        #{key:=V} when is_integer(V) ->
            id(42),
            [V]
    end.

m2() ->
    K1 = id(key1),
    K2 = id(key2),
    receive
        #{K1:=V1,K2:=V2} when is_integer(V1), is_integer(V2) ->
            id(42),
            {V1,V2}
    end.

id(I) ->
    I.
