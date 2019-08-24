% file: "smith.erl"

-ifdef(ETOS).
-define(IS_INTEGER(X),is_integer(X)).
-else.
-define(IS_INTEGER(X),integer(X)).
-endif.

-module(smith).
-export([?MODULE/0]).

?MODULE() ->
    Tops = generate_sequences(100,32,1),
    Side = generate_sequence(32,0),
    statistics(runtime),
    R = loop(2,Tops,Side,0),
    {R,R =:= 16}.

max(A,B) when ?IS_INTEGER(A), ?IS_INTEGER(B) ->
  if
    A > B -> A;
    true  -> B
  end.

alpha_beta_penalty(A,B) when ?IS_INTEGER(A), ?IS_INTEGER(B) -> max(A-4,B-1).

generate_sequence(Length,R) when ?IS_INTEGER(Length) ->
  if
    Length == 0 -> [];
    true        -> [R rem 10 | generate_sequence(Length-1,
                                                 (R * 11 + 1237501)
                                                 rem 10067)]
  end.

generate_sequences(0,_,_) -> [];
generate_sequences(N,Length,R) when ?IS_INTEGER(N), ?IS_INTEGER(Length) ->
  [generate_sequence(Length, R) | generate_sequences(N-1,Length,R+1)].

match_entry(Top,Side,UpperLeft,Upper,Left) when ?IS_INTEGER(Top), ?IS_INTEGER(Side) ->
  MeLeft = alpha_beta_penalty(element(3, Left), element(1, Left)),
  MeUpper = alpha_beta_penalty(element(3, Upper), element(2, Upper)),
  MeUpperLeft =
	if
	    Top == Side ->
		max(MeLeft,
		    max(MeUpper,
			max(element(3,UpperLeft)+1,0)));
	    true ->
		max(MeLeft,
		    max(MeUpper,
			max(element(3,UpperLeft),0)))
	end,
  {MeLeft, MeUpper, MeUpperLeft,
   max(MeUpperLeft,
       max(element(4,Left),
           max(element(4,Upper),element(4,UpperLeft))))}.

match_zero_entry(Top,Side,{Left,_,UpperLeft,Max}) when ?IS_INTEGER(Top), ?IS_INTEGER(Side) ->
  ELeft = alpha_beta_penalty(UpperLeft, Left),
  Weight = max(1-abs(Side-Top),0),
  EUpperLeft = max(max(ELeft,max(1-abs(Side-Top),0)),0),
  EMax = max(max(Max,EUpperLeft),0),
  {ELeft, -1, EUpperLeft, EMax}.

match(Tops,Side,Prev,UpperLeft,Left) ->
  match0(Tops, Side, Prev, UpperLeft, Left, [], none).

match0([],_,_,_,_,Acc,Last) -> {Acc,Last};
match0([Top|Tops],Side,[Upper|Prev],UpperLeft,Left,Acc,Last) when
  ?IS_INTEGER(Top), ?IS_INTEGER(Side) ->
  E = match_entry(Top, Side, UpperLeft, Upper, Left),
  match0(Tops, Side, Prev, Upper, E, [E|Acc], E);
match0([Top|Tops],Side,none,UpperLeft,Left,Acc,Last) when
  ?IS_INTEGER(Top), ?IS_INTEGER(Side) ->
  E = match_zero_entry(Top, Side, Left),
  match0(Tops, Side, none, UpperLeft, E, [E|Acc], E).

match_two_seq(Side,Top,Prev) ->
  match_two_seq0(Side, Top, Prev, none).

match_two_seq0([],_,_,Result) -> Result;
match_two_seq0([S|Side],Top,Prev,Acc) when ?IS_INTEGER(S) ->
  {Row,Result} = match(Top,S,Prev,{0,0,0,0},{0,0,0,0}),
  match_two_seq0(Side, Top, Row, Result).

match_sequences(Tops,Side) ->
  match_sequences0(Tops, Side, -9999999).

match_sequences0([],_,MaxResult) -> MaxResult;
match_sequences0([Top|Tops],Side,CrntResult) ->
  Result = element(4, match_two_seq(Top, Side, none)),
  match_sequences0(Tops, Side, max(CrntResult, Result)).

loop(0,Tops,Side,R) -> R;
loop(N,Tops,Side,R) -> loop(N-1,Tops,Side,match_sequences(Tops,Side)).
