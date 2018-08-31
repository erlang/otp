-module(left_assoc).

%% As pointed out in ERL-680, analyzing guards with short circuit
%% operators becomes very slow as the number of left associations
%% grows.

-spec from_iso8601('Elixir.String':t(), 'Elixir.Calendar':calendar()) ->
                          {ok, t()} | {error, atom()}.

-export_type([t/0]).

-type t() ::
        #{'__struct__' := 'Elixir.Date',
          calendar := 'Elixir.Calendar':calendar(),
          day := 'Elixir.Calendar':day(),
          month := 'Elixir.Calendar':month(),
          year := 'Elixir.Calendar':year()}.

-export([from_iso8601/1,
         from_iso8601/2]).

from_iso8601(__@1) ->
    from_iso8601(__@1, 'Elixir.Calendar.ISO').

from_iso8601(<<45/integer,_rest@1/binary>>, _calendar@1) ->
    case raw_from_iso8601(_rest@1, _calendar@1) of
        {ok,#{year := _year@1} = _date@1} ->
            {ok,_date@1#{year := - _year@1}};
        __@1 ->
            __@1
    end;
from_iso8601(<<_rest@1/binary>>, _calendar@1) ->
    raw_from_iso8601(_rest@1, _calendar@1).

raw_from_iso8601(_string@1, _calendar@1) ->
    case _string@1 of
        <<_y1@1/integer,
          _y2@1/integer,
          _y3@1/integer,
          _y4@1/integer,
          45/integer,
          _m1@1/integer,
          _m2@1/integer,
          45/integer,
          _d1@1/integer,
          _d2@1/integer>>
          when
              ((((((((((((((_y1@1 >= 48
                            andalso
                            _y1@1 =< 57)
                           andalso
                           _y2@1 >= 48)
                          andalso
                          _y2@1 =< 57)
                         andalso
                         _y3@1 >= 48)
                        andalso
                        _y3@1 =< 57)
                       andalso
                       _y4@1 >= 48)
                      andalso
                      _y4@1 =< 57)
                     andalso
                     _m1@1 >= 48)
                    andalso
                    _m1@1 =< 57)
                   andalso
                   _m2@1 >= 48)
                  andalso
                  _m2@1 =< 57)
                 andalso
                 _d1@1 >= 48)
                andalso
                _d1@1 =< 57)
               andalso
               _d2@1 >= 48)
              andalso
              _d2@1 =< 57 ->
            {ok,
             #{year => (_y1@1 - 48) * 1000 + (_y2@1 - 48) * 100
               +
                   (_y3@1 - 48) * 10
               +
                   (_y4@1 - 48),
               month => (_m1@1 - 48) * 10 + (_m2@1 - 48),
               day => (_d1@1 - 48) * 10 + (_d2@1 - 48),
               calendar => _calendar@1,
               '__struct__' => 'Elixir.Date'}};
        __@1 ->
            case __@1 of
                _ ->
                    {error,invalid_format};
                __@2 ->
                    error({with_clause,__@2})
            end
    end.
