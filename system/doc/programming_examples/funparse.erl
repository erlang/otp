-module(funparse).
-compile(export_all).
-import(lists, [reverse/1]).

%17
%% > hof:parse([a,c]).
%% {ok,{'and',{'or',1,{const,a}},{'or',1,{const,c}}}}
%% > hof:parse([a,d]). 
%% {ok,{'and',{'or',1,{const,a}},{'or',2,{const,d}}}}
%% > hof:parse([b,c]).   
%% {ok,{'and',{'or',2,{const,b}},{'or',1,{const,c}}}}
%% > hof:parse([b,d]). 
%% {ok,{'and',{'or',2,{const,b}},{'or',2,{const,d}}}}
%% > hof:parse([a,b]).   
%% fail
%17

%% Grammar = (a | b) & (c | d)

%12
parse(List) ->
    (grammar())(List).
%12

%13
grammar() ->
    pand(
         por(pconst(a), pconst(b)),
         por(pconst(c), pconst(d))).
%13

%14
pconst(X) ->
    fun (T) ->
       case T of
           [X|T1] -> {ok, {const, X}, T1};
           _      -> fail
       end
    end.
%14

%15
por(P1, P2) ->
    fun (T) ->
        case P1(T) of
            {ok, R, T1} -> 
                {ok, {'or',1,R}, T1};
            fail -> 
                case P2(T) of
                    {ok, R1, T1} ->
                        {ok, {'or',2,R1}, T1};
                    fail ->
                        fail
                end
        end
    end.
%15

%16
pand(P1, P2) ->
    fun (T) ->
        case P1(T) of
            {ok, R1, T1} ->
                case P2(T1) of
                    {ok, R2, T2} ->
                        {ok, {'and', R1, R2}};
                    fail ->
                        fail
                end;
            fail ->
                fail
        end
    end.
%16
