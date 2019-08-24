%% -*- Mode: erlang; indent-tabs-mode: nil -*-
%% Copyright Ericsson AB 2017. All Rights Reserved.

%%% indentation of terms contain builtin types

%%% Not everything in these test are set in stone
%%% better indentation rules can be added but by having
%%% these tests we can see what changes in new implementations
%%% and notice when doing unintentional changes


list(1) ->
    [a,
     b,
     c
    ];
list(2) ->
    [ a,
      b, c
    ];
list(3) ->
    [
     a,
     b, c
    ];
list(4) ->
    [ a
    , b
    , c
    ].

tuple(1) ->
    {a,
     b,c
    };
tuple(2) ->
    { a,
      b,c
    };
tuple(3) ->
    {
     a,
     b,c
    };
tuple(4) ->
    { a
    , b
    ,c
    }.

binary(1) ->
    <<1:8,
      2:8
    >>;
binary(2) ->
    <<
      1:8,
      2:8
    >>;
binary(3) ->
    << 1:8,
       2:8
    >>;
binary(4) ->
    <<
      1:8
     ,2:8
    >>;
binary(5) ->
    << 1:8
     , 2:8
    >>.

record(1) ->
    #record{a=1,
            b=2
           };
record(2) ->
    #record{ a=1,
             b=2
           };
record(3) ->
    #record{
       a=1,
       b=2
      };
record(4) ->
    #record{
       a=1
      ,b=2
      };
record(Record) ->
    Record#record{
      a=1
     ,b=2
     }.

map(1) ->
    #{a=>1,
      b=>2
     };
map(2) ->
    #{ a=>1,
       b=>2
     };
map(3) ->
    #{
      a=>1,
      b=>2
     };
map(4) ->
    #{
      a => <<"a">>
     ,b => 2
     };
map(MapVar) ->
    MapVar = #{a :=<<"a">>
              ,b:=1}.

deep(Rec) ->
    Rec#rec{ atom = 'atom',
             map = #{ k1 => {v,
                             1},
                      k2 => [
                             1,
                             2,
                             3
                            ],
                      {key,
                       3}
                      =>
                          <<
                            123:8,
                            255:8
                          >>
                    }
           }.

%% Record indentation
some_function_with_a_very_long_name() ->
    #'a-long-record-name-like-it-sometimes-is-with-asn.1-records'{
       field1=a,
       field2=b},
    case dummy_function_with_a_very_very_long_name(x) of
        #'a-long-record-name-like-it-sometimes-is-with-asn.1-records'{
           field1=a,
           field2=b} ->
            ok;
        Var = #'a-long-record-name-like-it-sometimes-is-with-asn.1-records'{
                 field1=a,
                 field2=b} ->
            Var#'a-long-record-name-like-it-sometimes-is-with-asn.1-records'{
              field1=a,
              field2=b};
        #xyz{
           a=1,
           b=2} ->
            ok
    end.

some_function_name_xyz(xyzzy, #some_record{
                                 field1=Field1,
                                 field2=Field2}) ->
    SomeVariable = f(#'Some-long-record-name'{
                        field_a = 1,
                        'inter-xyz-parameters' =
                            #'Some-other-very-long-record-name'{
                               field2 = Field1,
                               field2 = Field2}}),
    {ok, SomeVariable}.

foo() ->
    [#foo{
        foo = foo}].
