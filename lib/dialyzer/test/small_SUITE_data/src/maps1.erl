%%
%% File:    maps1.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-01-17
%%

-module(maps1).

-compile([export_all]).


-export([recv/3, decode/1]).

%-record(can_pkt, {id, data :: binary(), timestamp}).

-type can_pkt() :: #{ id => term(), data => binary(), timestamp => term() }.
-type channel() :: atom() | pid() | {atom(),_}.

-spec recv(<<_:64,_:_*8>>, fun((can_pkt()) -> R), channel()) -> R.
recv(Packet, Fun, Chan) ->
  #{id := Can_id, data := Can_data} = P = decode(Packet),
  Fun(P).

-spec decode(<<_:64,_:_*8>>) -> #{id => <<_:11>>,timestamp => char(),_ => _}.
decode(<<_:12, Len:4, Timestamp:16, 0:3, Id:11/bitstring, 0:18,
	 Data:Len/binary, _/binary>>) ->
  #{id => Id, data => Data, timestamp => Timestamp}.



t1() ->
    #{bar=>fun t2/0}.

t2() -> ok.

-type map_state() :: #{ id => integer(), val => term() }.

-spec update(map_state(), term()) -> map_state().

update(#{ id := Id, val := Val } = M, X) when is_integer(Id) ->
    M#{ val := [Val,X] }.

t3() ->
    foo(#{greger => 3, #{arne=>anka} => 45}, 1).

foo(#{} = M, b) -> %% Error
    M#{alfa => 42, beta := 1337}.

t4() ->
    case #{} of
        #{} -> ok;
        Mod -> Mod:function(#{literal => map}, another_arg) %% Error
    end.
