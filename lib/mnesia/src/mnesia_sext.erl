%% -*- erlang-indent-level: 4; indent-tabs-mode: nil
%%==============================================================================
%% Copyright 2014-2015 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
%%
%% @author Ulf Wiger <ulf@wiger.net>
%% @doc Sortable serialization library
%% @end
-module(mnesia_sext).

-export([encode/1, encode/2, decode/1, decode_next/1]).
-export([encode_hex/1, decode_hex/1]).
-export([encode_sb32/1, decode_sb32/1]).
-export([prefix/1,
         partial_decode/1]).
-export([prefix_hex/1]).
-export([prefix_sb32/1]).
-export([to_sb32/1, from_sb32/1]).
-export([to_hex/1, from_hex/1]).


-define(negbig   , 8).
-define(neg4     , 9).
-define(pos4     , 10).
-define(posbig   , 11).
-define(atom     , 12).
-define(reference, 13).
-define(port     , 14).
-define(pid      , 15).
-define(tuple    , 16).
-define(list     , 17).
-define(binary   , 18).
-define(bin_tail , 19).

-define(is_sext(X),
        X==?negbig;
            X==?neg4;
            X==?pos4;
            X==?posbig;
            X==?atom;
            X==?reference;
            X==?port;
            X==?pid;
            X==?tuple;
            X==?list;
            X==?binary;
            X==?bin_tail).

-define(IMAX1, 16#ffffFFFFffffFFFF).

-ifdef(DEBUG).
-define(dbg(Fmt,Args),
        case get(dbg) of
            true -> io:fwrite("~p: " ++ Fmt, [?LINE|Args]);
            _ -> ok
        end).
-else.
-define(dbg(F,A), ok).
-endif.

%% @spec encode(T::term()) -> binary()
%% @doc Encodes any Erlang term into a binary.
%% The lexical sorting properties of the encoded binary match those of the
%% original Erlang term. That is, encoded terms sort the same way as the
%% original terms would.
%% @end
%%
encode(X) -> encode(X, false).

%% @spec encode(T::term(), Legacy::boolean()) -> binary()
%% @doc Encodes an Erlang term using legacy bignum encoding.
%% On March 4 2013, Basho noticed that encoded bignums didn't always sort
%% properly. This bug has been fixed, but the encoding of bignums necessarily
%% changed in an incompatible way.
%%
%% The new decode/1 version can read the old bignum format, but the old
%% version obviously cannot read the new. Using `encode(Term, true)', the term
%% will be encoded using the old format.
%%
%% Use only as transition support. This function will be deprecated in time.
%% @end
encode(X, Legacy) when is_tuple(X)  -> encode_tuple(X, Legacy);
encode(X, Legacy) when is_list(X)   -> encode_list(X, Legacy);
encode(X, _) when is_pid(X)         -> encode_pid(X);
encode(X, _) when is_port(X)        -> encode_port(X);
encode(X, _) when is_reference(X)   -> encode_ref(X);
encode(X, Legacy) when is_number(X) -> encode_number(X, Legacy);
encode(X, _) when is_binary(X)      -> encode_binary(X);
encode(X, _) when is_bitstring(X)   -> encode_bitstring(X);
encode(X, _) when is_atom(X)        -> encode_atom(X).

%% @spec encode_sb32(Term::any()) -> binary()
%% @doc Encodes any Erlang term into an sb32-encoded binary.
%% This is similar to {@link encode/1}, but produces an octet string that
%% can be used without escaping in file names (containing only the characters
%% 0..9, A..V and '-'). The sorting properties are preserved.
%%
%% Note: The encoding used is inspired by the base32 encoding described in
%% RFC3548, but uses a different alphabet in order to preserve the sort order.
%% @end
%%
encode_sb32(Term) ->
    to_sb32(encode(Term)).

%% @spec encode_hex(Term::any()) -> binary()
%% @doc Encodes any Erlang term into a hex-encoded binary.
%% This is similar to {@link encode/1}, but produces an octet string that
%% can be used without escaping in file names (containing only the characters
%% 0..9 and A..F). The sorting properties are preserved.
%%
%% Note: The encoding used is regular hex-encoding, with the proviso that only
%% capital letters are used (mixing upper- and lowercase characters would break
%% the sorting property).
%% @end
%%
encode_hex(Term) ->
    to_hex(encode(Term)).

%% @spec prefix(X::term()) -> binary()
%% @doc Encodes a binary for prefix matching of similar encoded terms.
%% Lists and tuples can be prefixed by using the <code>'_'</code> marker,
%% similarly to Erlang match specifications. For example:
%% <ul>
%%  <li><code>prefix({1,2,'_','_'})</code> will result in a binary that is
%%    the same as the first part of any encoded 4-tuple with the first two
%%    elements being 1 and 2. The prefix algorithm will search for the
%%    first <code>'_'</code>, and treat all following elements as if they
%%    were <code>'_'</code>.</li>
%%  <li><code>prefix([1,2|'_'])</code> will result in a binary that is the
%%    same as the first part of any encoded list where the first two elements
%%    are 1 and 2. <code>prefix([1,2,'_'])</code> will give the same result,
%%    as the prefix pattern is the same for all lists starting with
%%    `[1,2|...]'.</li>
%%  <li>`prefix(Binary)' will result in a binary that is the same as the
%%    encoded version of Binary, except that, instead of padding and
%%    terminating, the encoded binary is truncated to the longest byte-aligned
%%    binary. The same is done for bitstrings.</li>
%%  <li><code>prefix({1,[1,2|'_'],'_'})</code> will prefix-encode the second
%%    element, and let it end the resulting binary. This prefix will match
%%    any 3-tuple where the first element is 1 and the second element is a
%%    list where the first two elements are 1 and 2.</li>
%%  <li><code>prefix([1,[1|'_']|'_'])</code> will result in a prefix that
%%    matches all lists where the first element is 1 and the second element is
%%    a list where the first element is 1.</li>
%%  <li>For all other data types, the prefix is the same as the encoded term.
%%    </li>
%% </ul>
%% @end
%%
prefix(X) ->
    {_, P} = enc_prefix(X),
    P.

enc_prefix(X) when is_tuple(X)     -> prefix_tuple(X);
enc_prefix(X) when is_list(X)      -> prefix_list(X);
enc_prefix(X) when is_pid(X)       -> {false, encode_pid(X)};
enc_prefix(X) when is_port(X)      -> {false, encode_port(X)};
enc_prefix(X) when is_reference(X) -> {false, encode_ref(X)};
enc_prefix(X) when is_number(X)    -> {false, encode_number(X)};
enc_prefix(X) when is_binary(X)    -> prefix_binary(X);
enc_prefix(X) when is_bitstring(X) -> prefix_bitstring(X);
enc_prefix(X) when is_atom(X) ->
    case is_wild(X) of
        true ->
            {true, <<>>};
        false ->
            {false, encode_atom(X)}
    end.

%% @spec prefix_sb32(X::term()) -> binary()
%% @doc Generates an sb32-encoded binary for prefix matching.
%% This is similar to {@link prefix/1}, but generates a prefix for binaries
%% encoded with {@link encode_sb32/1}, rather than {@link encode/1}.
%% @end
%%
prefix_sb32(X) ->
    chop_prefix_tail(to_sb32(prefix(X))).

%% @spec prefix_hex(X::term()) -> binary()
%% @doc Generates a hex-encoded binary for prefix matching.
%% This is similar to {@link prefix/1}, but generates a prefix for binaries
%% encoded with {@link encode_hex/1}, rather than {@link encode/1}.
%% @end
%%
prefix_hex(X) ->
    to_hex(prefix(X)).

%% Must chop of the pad character and the last encoded unit (which, if pad
%% characters are present, is not a whole byte)
%%
chop_prefix_tail(Bin) ->
    Sz = byte_size(Bin),
    Sz6 = Sz-7, Sz4 = Sz - 5, Sz3 = Sz - 4, Sz1 = Sz - 2,
    case Bin of
        << P:Sz6/binary, _, "------" >> -> P;
        << P:Sz4/binary, _, "----"   >> -> P;
        << P:Sz3/binary, _, "---"    >> -> P;
        << P:Sz1/binary, _, "-"      >> -> P;
        _ -> Bin
    end.

%% @spec decode(B::binary()) -> term()
%% @doc Decodes a binary generated using the function {@link sext:encode/1}.
%% @end
%%
decode(Elems) ->
    case decode_next(Elems) of
        {Term, <<>>} -> Term;
        Other -> erlang:error(badarg, Other)
    end.

%% spec decode_sb32(B::binary()) -> term()
%% @doc Decodes a binary generated using the function {@link encode_sb32/1}.
%% @end
%%
decode_sb32(Data) ->
    decode(from_sb32(Data)).

decode_hex(Data) ->
    decode(from_hex(Data)).

encode_tuple(T, Legacy) ->
    Sz = size(T),
    encode_tuple_elems(1, Sz, T, <<?tuple, Sz:32>>, Legacy).

prefix_tuple(T) ->
    Sz = size(T),
    Elems = tuple_to_list(T),
    prefix_tuple_elems(Elems, <<?tuple, Sz:32>>).

%% It's easier to iterate over a tuple by converting it to a list, but
%% since the tuple /can/ be huge, let's do it this way.
encode_tuple_elems(P, Sz, T, Acc, Legacy) when P =< Sz ->
    E = encode(element(P,T), Legacy),
    encode_tuple_elems(P+1, Sz, T, <<Acc/binary, E/binary>>, Legacy);
encode_tuple_elems(_, _, _, Acc, _) ->
    Acc.

prefix_tuple_elems([A|T], Acc) when is_atom(A) ->
    case is_wild(A) of
        true ->
            {true, Acc};
        false ->
            E = encode(A),
            prefix_tuple_elems(T, <<Acc/binary, E/binary>>)
    end;
prefix_tuple_elems([H|T], Acc) ->
    case enc_prefix(H) of
        {true, P} ->
            {true, <<Acc/binary, P/binary>>};
        {false, E} ->
            prefix_tuple_elems(T, <<Acc/binary, E/binary>>)
    end;
prefix_tuple_elems([], Acc) ->
    {false, Acc}.

encode_list(L, Legacy) ->
    encode_list_elems(L, <<?list>>, Legacy).

prefix_list(L) ->
    prefix_list_elems(L, <<?list>>).

encode_binary(B)    ->
    Enc = encode_bin_elems(B),
    <<?binary:8, Enc/binary>>.

prefix_binary(B) ->
    Enc = encode_bin_elems(B),
    {false, <<?binary:8, Enc/binary>>}.

encode_bitstring(B) ->
    Enc = encode_bits_elems(B),
    <<?binary:8, Enc/binary>>.

prefix_bitstring(B) ->
    Enc = encode_bits_elems(B),
    {false, <<?binary:8, Enc/binary>>}.

encode_pid(P) ->
    PBin = term_to_binary(P),
    <<131,103,100,ALen:16,Name:ALen/binary,Rest:9/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?pid, NameEnc/binary, Rest/binary>>.

encode_port(P) ->
    PBin = term_to_binary(P),
    <<131,102,100,ALen:16,Name:ALen/binary,Rest:5/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?port, NameEnc/binary, Rest/binary>>.

encode_ref(R) ->
    RBin = term_to_binary(R),
    <<131,114,_Len:16,100,NLen:16,Name:NLen/binary,Rest/binary>> = RBin,
    NameEnc = encode_bin_elems(Name),
    RestEnc = encode_bin_elems(Rest),
    <<?reference, NameEnc/binary, RestEnc/binary>>.

encode_atom(A) ->
    Bin = list_to_binary(atom_to_list(A)),
    Enc = encode_bin_elems(Bin),
    <<?atom, Enc/binary>>.

encode_number(N) ->
    encode_number(N, false).

encode_number(N, Legacy) when is_integer(N) ->
    encode_int(N, none, Legacy);
encode_number(F, _Legacy) when is_float(F) ->
    encode_float(F).

%%
%% IEEE 764 Binary 64 standard representation
%% http://en.wikipedia.org/wiki/Double_precision_floating-point_format
%%
%% |12345678 12345678 12345678 12345678 12345678 12345678 12345678 12345678
%% |iEEEEEEE EEEEffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff|
%%
%% i: sign bit
%% E: Exponent, 11 bits
%% f: fraction, 52 bits
%%
%% We perform the following operations:
%% - if E < 1023 (see Exponent bias), the integer part is 0
%%
encode_float(F) ->
    <<Sign:1, Exp0:11, Frac:52>> = <<F/float>>,
    ?dbg("F = ~p | Exp0 = ~p | Frac = ~p~n", [cF, Exp0, Frac]),
    {Int0, Fraction} =
        case Exp0 - 1023 of
            NegExp when NegExp < 0 ->
                Offs = -NegExp,
                ?dbg("NegExp = ~p, Offs = ~p~n"
                     "Frac = ~p~n", [NegExp, Offs, Frac]),
                {0, << 0:Offs, 1:1,Frac:52 >>};
            Exp1 ->
                ?dbg("Exp1 = ~p~n", [Exp1]),
                if Exp1 >= 52 ->
                        %% Decimal part will be zero
                        {trunc(F), <<0:52>>};
                   true ->
                        R = 52-Exp1,
                        ?dbg("R = ~p~n", [R]),
                        Exp2 = Exp1 + 1,        % add the leading 1-bit
                        ?dbg("Exp2 = ~p~n", [Exp2]),
                        <<I:Exp2, Frac1:R>> = <<1:1, Frac:52>>,
                        ?dbg("I = ~p, Frac1 = ~p~n", [I,Frac1]),
                        {I, <<Frac1:R>>}
                end
        end,
    if Sign == 1 ->
            %% explicitly encode a negative int, since Int0 can be zero.
            Int = if Int0 >= 0 -> -Int0;
                     true -> Int0
                  end,
            encode_neg_int(Int, Fraction);
       Sign == 0 ->
            encode_int(Int0, Fraction)
    end.


encode_int(I, R) ->
    encode_int(I, R, false).

encode_int(I,R, _Legacy) when I >= 0, I =< 16#7fffffff ->
    ?dbg("encode_int(~p, ~p)~n", [I,R]),
    if R == none ->
            << ?pos4, I:31, 0:1 >>;
       true ->
            RSz = bit_size(R),
            <<Fraction:RSz>> = R,
            ?dbg("Fraction = ~p~n", [Fraction]),
            if Fraction == 0 ->
                    << ?pos4, I:31, 1:1, 8:8 >>;
               true ->
                    Rbits = encode_bits_elems(R),
                    << ?pos4, I:31, 1:1, Rbits/binary >>
               end
    end;
encode_int(I,R, Legacy) when I > 16#7fffffff ->
    ?dbg("encode_int(~p, ~p)~n", [I,R]),
    Bytes = encode_big(I, Legacy),
    if R == none ->
            <<?posbig, Bytes/binary, 0:8>>;
       true ->
            RSz = bit_size(R),
            <<Fraction:RSz>> = R,
            ?dbg("Fraction = ~p~n", [Fraction]),
            if Fraction == 0 ->
                    << ?posbig, Bytes/binary, 1:8, 8:8 >>;
               true ->
                    Rbits = encode_bits_elems(R),
                    <<?posbig, Bytes/binary, 1:8, Rbits/binary>>
            end
    end;
encode_int(I, R, _Legacy) when I < 0 ->
    encode_neg_int(I, R).

encode_neg_int(I,R) when I =< 0, I >= -16#7fffffff ->
    ?dbg("encode_neg_int(~p, ~p [sz: ~p])~n", [I,pp(R), try bit_size(R) catch error:_ -> "***" end]),
    Adj = max_value(31) + I,    % keep in mind that I < 0
    ?dbg("Adj = ~p~n", [erlang:integer_to_list(Adj,2)]),
    if R == none ->
            << ?neg4, Adj:31, 1:1 >>;
       true ->
            Rbits = encode_neg_bits(R),
            ?dbg("R = ~p -> RBits = ~p~n", [pp(R), pp(Rbits)]),
            << ?neg4, Adj:31, 0:1, Rbits/binary >>
    end;
encode_neg_int(I,R) when I < -16#7fFFffFF ->
    ?dbg("encode_neg_int(BIG ~p)~n", [I]),
    Bytes = encode_big_neg(I),
    ?dbg("Bytes = ~p~n", [Bytes]),
    if R == none ->
            <<?negbig, Bytes/binary, 16#ff:8>>;
       true ->
            Rbits = encode_neg_bits(R),
            ?dbg("R = ~p -> RBits = ~p~n", [pp(R), pp(Rbits)]),
            <<?negbig, Bytes/binary, 0, Rbits/binary>>
    end.

encode_big(I, Legacy) ->
    Bl = encode_big1(I),
    ?dbg("Bl = ~p~n", [Bl]),
    Bb = case Legacy of
             false ->
                 prepend_size(list_to_binary(Bl));
             true ->
                 list_to_binary(Bl)
         end,
    ?dbg("Bb = ~p~n", [Bb]),
    encode_bin_elems(Bb).

prepend_size(B) ->
    Sz = byte_size(B),
    <<255, (encode_size(Sz))/binary, B/binary>>.

remove_size_bits(<<255, T/binary>>) ->
    {_, Rest} = untag_7bits(T, <<>>),
    Rest;
remove_size_bits(B) ->
    %% legacy bignum
    B.

encode_size(I) when I > 127 ->
    B = int_to_binary(I),
    tag_7bits(B);
encode_size(I) ->
    <<I>>.

tag_7bits(B) when bit_size(B) > 7 ->
    <<H:7, T/bitstring>> = B,
    <<1:1, H:7, (tag_7bits(T))/binary>>;
tag_7bits(B) ->
    Sz = bit_size(B),
    <<I:Sz>> = B,
    <<0:1, I:7>>.

untag_7bits(<<1:1, H:7, T/binary>>, Acc) ->
    untag_7bits(T, <<Acc/bitstring, H:7>>);
untag_7bits(<<0:1, H:7, T/binary>>, Acc) ->
    AccBits = bit_size(Acc),
    HBits = 8 - (AccBits rem 8),
    {<<Acc/bitstring, H:HBits>>, T}.

int_to_binary(I) when I =< 16#ff -> <<I:8>>;
int_to_binary(I) when I =< 16#ffff -> <<I:16>>;
int_to_binary(I) when I =< 16#ffffff -> <<I:24>>;
int_to_binary(I) when I =< 16#ffffffff -> <<I:32>>;
int_to_binary(I) when I =< 16#ffffffffff -> <<I:40>>;
int_to_binary(I) when I =< 16#ffffffffffff -> <<I:48>>;
int_to_binary(I) when I =< 16#ffffffffffffff -> <<I:56>>;
int_to_binary(I) when I =< 16#ffffffffffffffff -> <<I:64>>;
int_to_binary(I) ->
    %% Realm of the ridiculous
    list_to_binary(
      lists:dropwhile(fun(X) -> X==0 end, binary_to_list(<<I:256>>))).

%% This function exists for documentation, but not used right now.
%% It's the reverse of encode_size/1, used for encoding bignums.
%%
%% decode_size(<<1:1, _/bitstring>> = T) ->
%%     {SzBin, Rest} = untag_7bits(T, <<>>),
%%     Bits = bit_size(SzBin),
%%     <<Sz:Bits>> = SzBin,
%%     {Sz, Rest};
%% decode_size(<<0:1, H:7, T/binary>>) ->
%%     {H, T}.

encode_big_neg(I) ->
    {Words, Max} = get_max(-I),
    ?dbg("Words = ~p | Max = ~p~n", [Words,Max]),
    Iadj = Max + I,             % keep in mind that I < 0
    ?dbg("IAdj = ~p~n", [Iadj]),
    Bin = encode_bin_elems(list_to_binary(encode_big1(Iadj))),
    ?dbg("Bin = ~p~n", [Bin]),
    WordsAdj = 16#ffffFFFF - Words,
    ?dbg("WordsAdj = ~p~n", [WordsAdj]),
    <<WordsAdj:32, Bin/binary>>.

encode_big1(I) ->
    encode_big1(I, []).

encode_big1(I, Acc) when I < 16#ff ->
    [I|Acc];
encode_big1(I, Acc) ->
    encode_big1(I bsr 8, [I band 16#ff | Acc]).

encode_list_elems([], Acc, _) ->
    <<Acc/binary, 2>>;
encode_list_elems(B, Acc, Legacy) when is_bitstring(B) ->
    %% improper list
    <<Acc/binary, ?bin_tail, (encode(B, Legacy))/binary>>;
encode_list_elems(E, Acc, Legacy) when not(is_list(E)) ->
    %% improper list
    <<Acc/binary, 1, (encode(E, Legacy))/binary>>;
encode_list_elems([H|T], Acc, Legacy) ->
    Enc = encode(H,Legacy),
    encode_list_elems(T, <<Acc/binary, Enc/binary>>, Legacy).

prefix_list_elems([], Acc) ->
    {false, <<Acc/binary, 2>>};
prefix_list_elems(E, Acc) when not(is_list(E)) ->
    case is_wild(E) of
        true ->
            {true, Acc};
        false ->
            Marker = if is_bitstring(E) -> ?bin_tail;
                        true -> 1
                     end,
            {Bool, P} = enc_prefix(E),
            {Bool, <<Acc/binary, Marker, P/binary>>}
    end;
prefix_list_elems([H|T], Acc) ->
    case enc_prefix(H) of
        {true, P} ->
            {true, <<Acc/binary, P/binary>>};
        {false, E} ->
            prefix_list_elems(T, <<Acc/binary, E/binary>>)
    end.

is_wild('_') ->
    true;
is_wild(A) when is_atom(A) ->
    case atom_to_list(A) of
        "\$" ++ S ->
            try begin
                    _ = list_to_integer(S),
                    true
                end
            catch
                error:_ ->
                    false
            end;
        _ ->
            false
    end;
is_wild(_) ->
    false.

encode_bin_elems(<<>>) ->
    <<8>>;
encode_bin_elems(B) ->
    Pad = 8 - (size(B) rem 8),
    << (<< <<1:1, B1:8>> || <<B1>> <= B >>)/bitstring, 0:Pad, 8 >>.

encode_neg_bits(<<>>) ->
    <<247>>;
encode_neg_bits(B) ->
    {Padded, TailBits} = pad_neg_bytes(B),
    ?dbg("TailBits = ~p~n", [TailBits]),
    TailSz0 = bit_size(TailBits),
    TailSz = 16#ff - TailSz0,
    if TailSz0 == 0 ->
            Pad = 8 - (bit_size(Padded) rem 8),
            Ip = max_value(Pad), % e.g. max_value(3) -> 2#111
            <<Padded/bitstring, Ip:Pad, TailSz:8>>;
       true ->
            ?dbg("TailSz0 = ~p~n", [TailSz0]),
            TailPad = 8 - TailSz0,
            ?dbg("TailPad = ~p~n", [TailPad]),
            Itp = (1 bsl TailPad)-1,
            ?dbg("Itp = ~p~n", [Itp]),
            Pad = 8 - ((bit_size(Padded) + 1) rem 8),
            ?dbg("Pad = ~p~n", [Pad]),
            Ip = max_value(Pad),
            ?dbg("Ip = ~p~n", [Ip]),
            ?dbg("Pad = ~p~n", [Pad]),
            ?dbg("TailSz = ~p~n", [TailSz]),
            <<Padded/bitstring, 0:1, TailBits/bitstring,
             Itp:TailPad, Ip:Pad, TailSz:8>>
    end.

pad_neg_bytes(Bin) ->
    pad_neg_bytes(Bin, <<>>).

pad_neg_bytes(<<H:8, T/bitstring>>, Acc) ->
    H1 = 16#ff - H,
    pad_neg_bytes(T, <<Acc/bitstring, 0:1, H1>>);
pad_neg_bytes(Bits, Acc) when is_bitstring(Bits) ->
    Sz = bit_size(Bits),
    Max = (1 bsl Sz) - 1,
    <<I0:Sz>> = Bits,
    I1 = Max - I0,
    {Acc, <<I1:Sz>>}.

encode_bits_elems(B) ->
    {Padded, TailBits} = pad_bytes(B),
    TailSz = bit_size(TailBits),
    TailPad = 8-TailSz,
    Pad = 8 - ((TailSz + TailPad + bit_size(Padded) + 1) rem 8),
    <<Padded/bitstring, 1:1, TailBits/bitstring, 0:TailPad, 0:Pad, TailSz:8>>.

pad_bytes(Bin) ->
    pad_bytes(Bin, <<>>).

pad_bytes(<<H:8, T/bitstring>>, Acc) ->
    pad_bytes(T, <<Acc/bitstring, 1:1, H>>);
pad_bytes(Bits, Acc) when is_bitstring(Bits) ->
    {Acc, Bits}.


%% ------------------------------------------------------
%% Decoding routines

-spec decode_next(binary()) -> {any(), binary()}.
%% @spec decode_next(Bin) -> {N, Rest}
%% @doc Decode a binary stream, returning the next decoded term and the
%% stream remainder
%%
%% This function will raise an exception if the beginning of `Bin' is not
%% a valid sext-encoded term.
%% @end
decode_next(<<?atom,Rest/binary>>) -> decode_atom(Rest);
decode_next(<<?pid, Rest/binary>>) -> decode_pid(Rest);
decode_next(<<?port, Rest/binary>>) -> decode_port(Rest);
decode_next(<<?reference,Rest/binary>>) -> decode_ref(Rest);
decode_next(<<?tuple,Sz:32, Rest/binary>>) -> decode_tuple(Sz,Rest);
decode_next(<<?list, Rest/binary>>) -> decode_list(Rest);
decode_next(<<?negbig, Rest/binary>>) -> decode_neg_big(Rest);
decode_next(<<?posbig, Rest/binary>>) -> decode_pos_big(Rest);
decode_next(<<?neg4, I:31, F:1, Rest/binary>>) -> decode_neg(I,F,Rest);
decode_next(<<?pos4, I:31, F:1, Rest/binary>>) -> decode_pos(I,F,Rest);
decode_next(<<?binary, Rest/binary>>) -> decode_binary(Rest).

-spec partial_decode(binary()) -> {full | partial, any(), binary()}.
%% @spec partial_decode(Bytes) -> {full | partial, DecodedTerm, Rest}
%% @doc Decode a sext-encoded term or prefix embedded in a byte stream.
%%
%% Example:
%% ```
%% 1&gt; T = sext:encode({a,b,c}).
%% &lt;&lt;16,0,0,0,3,12,176,128,8,12,177,0,8,12,177,128,8&gt;&gt;
%% 2&gt; sext:partial_decode(&lt;&lt;T/binary, "tail"&gt;&gt;).
%% {full,{a,b,c},&lt;&lt;"tail"&gt;&gt;}
%% 3&gt; P = sext:prefix({a,b,'_'}).
%% &lt;&lt;16,0,0,0,3,12,176,128,8,12,177,0,8&gt;&gt;
%% 4&gt; sext:partial_decode(&lt;&lt;P/binary, "tail"&gt;&gt;).
%% {partial,{a,b,'_'},&lt;&lt;"tail"&gt;&gt;}
%% '''
%%
%% Note that a decoded prefix may not be exactly like the encoded prefix.
%% For example, <code>['_']</code> will be encoded as
%% <code>&lt;&lt;17&gt;&gt;</code>, i.e. only the 'list' opcode. The
%% decoded prefix will be <code>'_'</code>, since the encoded prefix would
%% also match the empty list. The decoded prefix will always be a prefix to
%% anything to which the original prefix is a prefix.
%%
%% For tuples, <code>{1,'_',3}</code> encoded and decoded, will result in
%% <code>{1,'_','_'}</code>, i.e. the tuple size is kept, but the elements
%% after the first wildcard are replaced with wildcards.
%% @end
partial_decode(<<?tuple, Sz:32, Rest/binary>>) ->
    partial_decode_tuple(Sz, Rest);
partial_decode(<<?list, Rest/binary>>) ->
    partial_decode_list(Rest);
partial_decode(Other) ->
    try decode_next(Other) of
        {Dec, Rest} ->
            {full, Dec, Rest}
    catch
        error:function_clause ->
            {partial, '_', Other}
    end.

decode_atom(B) ->
    {Bin, Rest} = decode_binary(B),
    {list_to_atom(binary_to_list(Bin)), Rest}.

decode_tuple(Sz, Elems) ->
    decode_tuple(Sz,Elems,[]).

decode_tuple(0, Rest, Acc) ->
    {list_to_tuple(lists:reverse(Acc)), Rest};
decode_tuple(N, Elems, Acc) ->
    {Term, Rest} = decode_next(Elems),
    decode_tuple(N-1, Rest, [Term|Acc]).

partial_decode_tuple(Sz, Elems) ->
    partial_decode_tuple(Sz, Elems, []).

partial_decode_tuple(0, Rest, Acc) ->
    {full, list_to_tuple(lists:reverse(Acc)), Rest};
partial_decode_tuple(N, Elems, Acc) ->
    case partial_decode(Elems) of
        {partial, Term, Rest} ->
            {partial, list_to_tuple(
                        lists:reverse([Term|Acc]) ++ pad_(N-1)), Rest};
        {full, Dec, Rest} ->
            partial_decode_tuple(N-1, Rest, [Dec|Acc])
    end.

pad_(0) ->
    [];
pad_(N) when N > 0 ->
    ['_'|pad_(N-1)].

partial_decode_list(Elems) ->
    partial_decode_list(Elems, []).

partial_decode_list(<<>>, Acc) ->
    {partial, lists:reverse(Acc) ++ '_', <<>>};
partial_decode_list(<<2, Rest/binary>>, Acc) ->
    {full, lists:reverse(Acc), Rest};
partial_decode_list(<<?bin_tail, Next/binary>>, Acc) ->
    %% improper list, binary tail
    {Term, Rest} = decode_next(Next),
    {full, lists:reverse(Acc) ++ Term, Rest};
partial_decode_list(<<1, Next/binary>>, Acc) ->
    {Result, Term, Rest} = partial_decode(Next),
    {Result, lists:reverse(Acc) ++ Term, Rest};
partial_decode_list(<<X,_/binary>> = Next, Acc) when ?is_sext(X) ->
    case partial_decode(Next) of
        {full, Term, Rest} ->
            partial_decode_list(Rest, [Term|Acc]);
        {partial, Term, Rest} ->
            {partial, lists:reverse([Term|Acc]) ++ '_', Rest}
    end;
partial_decode_list(Rest, Acc) ->
    {partial, lists:reverse(Acc) ++ '_', Rest}.

decode_list(Elems) ->
    decode_list(Elems, []).

decode_list(<<2, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_list(<<?bin_tail, Next/binary>>, Acc) ->
    %% improper list, binary tail
    {Term, Rest} = decode_next(Next),
    {lists:reverse(Acc) ++ Term, Rest};
decode_list(<<1, Next/binary>>, Acc) ->
    %% improper list, non-binary tail
    {Term, Rest} = decode_next(Next),
    {lists:reverse(Acc) ++ Term, Rest};
decode_list(Elems, Acc) ->
    {Term, Rest} = decode_next(Elems),
    decode_list(Rest, [Term|Acc]).

decode_pid(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    <<Tail:9/binary, Rest1/binary>> = Rest,
    NameSz = size(Name),
    {binary_to_term(<<131,103,100,NameSz:16,Name/binary,Tail/binary>>), Rest1}.

decode_port(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    <<Tail:5/binary, Rest1/binary>> = Rest,
    NameSz = size(Name),
    {binary_to_term(<<131,102,100,NameSz:16,Name/binary,Tail/binary>>), Rest1}.

decode_ref(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    {Tail, Rest1} = decode_binary(Rest),
    NLen = size(Name),
    Len = (size(Tail)-1) div 4,
    RefBin = <<131,114,Len:16,100,NLen:16,Name/binary,Tail/binary>>,
    {binary_to_term(RefBin), Rest1}.

decode_neg(I, 1, Rest) ->
    {(I - 16#7fffFFFF), Rest};
decode_neg(I0, 0, Bin) ->  % for negative numbers, 0 means that it's a float
    I = 16#7fffFFFF - I0,
    ?dbg("decode_neg()... I = ~p | Bin = ~p~n", [I, Bin]),
    decode_neg_float(I, Bin).

decode_neg_float(0, Bin) ->
    {R, Rest} = decode_neg_binary(Bin),
    ?dbg("Bin = ~p~n", [pp(Bin)]),
    ?dbg("R = ~p | Rest = ~p~n", [pp(R), Rest]),
    Sz = bit_size(R),
    Offs = Sz - 53,
    ?dbg("Offs = ~p | Sz - ~p~n", [Offs, Sz]),
    <<_:Offs, 1:1, I:52>> = R,
    Exp = 1023 - Offs,
    <<F/float>> = <<1:1, Exp:11, I:52>>,
    {F, Rest};
decode_neg_float(I, Bin) ->
    {R, Rest} = decode_neg_binary(Bin),
    ?dbg("decode_neg_float: I = ~p | R = ~p~n", [I, R]),
    Sz = bit_size(R),
    ?dbg("Sz = ~p~n", [Sz]),
    <<Ri:Sz>> = R,
    ?dbg("Ri = ~p~n", [Ri]),
    if Ri == 0 ->
            %% special case
            {0.0-I, Rest};
       true ->
            IBits = strip_first_one(I),
            ?dbg("IBits = ~p~n", [pp(IBits)]),
            Bits = <<IBits/bitstring, Ri:Sz>>,
            ?dbg("Bits = ~p (Sz: ~p)~n", [pp(Bits), bit_size(Bits)]),
            Exp = bit_size(IBits) + 1023,
            ?dbg("Exp = ~p~n", [Exp]),
            <<Frac:52, _/bitstring>> = <<Bits/bitstring, 0:52>>,
            ?dbg("Frac = ~p~n", [Frac]),
            <<F/float>> = <<1:1, Exp:11, Frac:52>>,
            {F, Rest}
    end.

decode_pos(I, 0, Rest) ->
    {I, Rest};
decode_pos(0, 1, Bin) ->
    {Real, Rest} = decode_binary(Bin),
    Offs = bit_size(Real) - 53,
    <<0:Offs, 1:1, Frac:52>> = Real,
    Exp = 1023 - Offs,
    <<F/float>> = <<0:1, Exp:11, Frac:52>>,
    {F, Rest};
decode_pos(I, 1, Bin) ->        % float > 1
    ?dbg("decode_pos(~p, 1, ~p)~n", [I, Bin]),
    {Real, Rest} = decode_binary(Bin),
    case decode_binary(Bin) of
        {<<>>, Rest} ->
            <<F/float>> = <<I/float>>,
            {F, Rest};
        {Real, Rest} ->
            ?dbg("Real = ~p~n", [Real]),
            Exp = 52 - bit_size(Real) + 1023,
            ?dbg("Exp = ~p~n", [Exp]),
            Bits0 = <<I:31, Real/bitstring>>,
            ?dbg("Bits0 = ~p~n", [Bits0]),
            Bits = strip_one(Bits0),
            <<Frac:52>> = Bits,
            <<F/float>> = <<0:1, Exp:11, Frac:52>>,
            {F, Rest}
    end.

decode_pos_big(Bin) ->
    ?dbg("decode_pos_big(~p)~n", [Bin]),
    {Ib0, Rest} = decode_binary(Bin),
    Ib = remove_size_bits(Ib0),
    ?dbg("Ib = ~p~n", [Ib]),
    ISz = size(Ib) * 8,
    ?dbg("ISz = ~p~n", [ISz]),
    <<I:ISz>> = Ib,
    ?dbg("I = ~p~n", [I]),
    <<F:8, Rest1/binary>> = Rest,
    ?dbg("Rest1 = ~p~n", [Rest1]),
    decode_pos(I, F, Rest1).

decode_neg_big(Bin) ->
    ?dbg("decode_neg_big(~p)~n", [Bin]),
    <<WordsAdj:32, Rest/binary>> = Bin,
    Words = 16#ffffFFFF - WordsAdj,
    ?dbg("Words = ~p~n", [Words]),
    {Ib, Rest1} = decode_binary(Rest),
    ?dbg("Ib = ~p | Rest1 = ~p~n", [Ib, Rest1]),
    ISz = size(Ib) * 8,
    <<I0:ISz>> = Ib,
    ?dbg("I0 = ~p~n", [I0]),
    Max = imax(Words),
    ?dbg("Max = ~p~n", [Max]),
    I = Max - I0,
    ?dbg("I = ~p~n", [I]),
    <<F:8, Rest2/binary>> = Rest1,
    ?dbg("F = ~p | Rest2 = ~p~n", [F, Rest2]),
    if F == 0 ->
            decode_neg_float(I, Rest2);
       F == 16#ff ->
            {-I, Rest2}
    end.

%% optimization - no need to loop through a very large number of zeros.
strip_first_one(I) ->
    Sz = if I < 16#ff -> 8;
            I < 16#ffff -> 16;
            I < 16#ffffff -> 24;
            I < 16#ffffffff -> 32;
            true -> 52
         end,
    strip_one(<<I:Sz>>).

strip_one(<<0:1, Rest/bitstring>>) -> strip_one(Rest);
strip_one(<<1:1, Rest/bitstring>>) -> Rest.


decode_binary(<<8, Rest/binary>>) ->  {<<>>, Rest};
decode_binary(B)     ->  decode_binary(B, 0, <<>>).

decode_binary(<<1:1,H:8,Rest/bitstring>>, N, Acc) ->
    case Rest of
        <<1:1,_/bitstring>> ->
            decode_binary(Rest, N+9, << Acc/binary, H >>);
        _ ->
            Pad = 8 - ((N+9) rem 8),
            <<0:Pad,EndBits,Rest1/binary>> = Rest,
            TailPad = 8-EndBits,
            <<Tail:EndBits,0:TailPad>> = <<H>>,
            {<< Acc/binary, Tail:EndBits >>, Rest1}
    end.

decode_neg_binary(<<247, Rest/binary>>) ->  {<<>>, Rest};  % 16#ff - 8
decode_neg_binary(B)     ->  decode_neg_binary(B, 0, <<>>).

decode_neg_binary(<<0:1,H:8,Rest/bitstring>>, N, Acc) ->
    case Rest of
        <<0:1,_/bitstring>> ->
            decode_neg_binary(Rest, N+9, << Acc/binary, (16#ff - H) >>);
        _ ->
            Pad = 8 - ((N+9) rem 8),
            ?dbg("Pad = ~p~n", [Pad]),
            IPad = (1 bsl Pad) - 1,
            <<IPad:Pad,EndBits0,Rest1/binary>> = Rest,
            ?dbg("EndBits0 = ~p~n", [EndBits0]),
            EndBits = 16#ff - EndBits0,
            ?dbg("EndBits = ~p~n", [EndBits]),
            if EndBits == 0 ->
                    {<< Acc/binary, (16#ff - H)>>, Rest1};
               true ->
                    <<Tail:EndBits,_/bitstring>> = <<(16#ff - H)>>,
                    ?dbg("Tail = ~p~n", [Tail]),
                    {<< Acc/binary, Tail:EndBits >>, Rest1}
            end
    end.

%% The largest value that fits in Sz bits
max_value(Sz) ->
    (1 bsl Sz) - 1.

%% The largest value that fits in Words*64 bits.
imax(1) -> max_value(64);
imax(2) -> max_value(128);
imax(Words) -> max_value(Words*64).

%% Get the smallest imax/1 value that's larger than I.
get_max(I) -> get_max(I, 1, imax(1)).
get_max(I, W, Max) when I > Max ->
    get_max(I, W+1, (Max bsl 64) bor ?IMAX1);
get_max(_, W, Max) ->
    {W, Max}.

%% @spec to_sb32(Bits::bitstring()) -> binary()
%% @doc Converts a bitstring into an sb-encoded bitstring
%%
%% sb32 (Sortable base32) is a variant of RFC3548, slightly rearranged to
%% preserve the lexical sorting properties. Base32 was chosen to avoid
%% filename-unfriendly characters. Also important is that the padding
%% character be less than any character in the alphabet
%%
%% sb32 alphabet:
%% <pre>
%% 0 0     6 6     12 C     18 I     24 O     30 U
%% 1 1     7 7     13 D     19 J     25 P     31 V
%% 2 2     8 8     14 E     20 K     26 Q  (pad) -
%% 3 3     9 9     15 F     21 L     27 R
%% 4 4    10 A     16 G     22 M     28 S
%% 5 5    11 B     17 H     23 N     29 T
%% </pre>
%% @end
%%
to_sb32(Bits) when is_bitstring(Bits) ->
    Sz = bit_size(Bits),
    {Chunk, Rest, Pad} =
        case Sz rem 5 of
            0 -> {Bits, <<>>, <<>>};
            R -> sb32_encode_chunks(Sz, R, Bits)
        end,
    Enc = << << (c2sb32(C1)) >> ||
              <<C1:5>> <= Chunk >>,
    if Rest == << >> ->
            Enc;
       true ->
            << Enc/bitstring, (c2sb32(Rest)):8, Pad/binary >>
    end.

sb32_encode_chunks(Sz, Rem, Bits) ->
    ChunkSz = Sz - Rem,
    << C:ChunkSz/bitstring, Rest:Rem >> = Bits,
    Pad = encode_pad(Rem),
    {C, Rest, Pad}.

encode_pad(3) -> <<"------">>;
encode_pad(1) -> <<"----">>;
encode_pad(4) -> <<"---">>;
encode_pad(2) -> <<"-">>.

%% @spec from_sb32(Bits::bitstring()) -> bitstring()
%% @doc Converts from an sb32-encoded bitstring into a 'normal' bitstring
%%
%% This function is the reverse of {@link to_sb32/1}.
%% @end
%%
from_sb32(<< C:8, "------" >>) -> << (sb322c(C)):3 >>;
from_sb32(<< C:8, "----" >>  ) -> << (sb322c(C)):1 >>;
from_sb32(<< C:8, "---" >>   ) -> << (sb322c(C)):4 >>;
from_sb32(<< C:8, "-" >>     ) -> << (sb322c(C)):2 >>;
from_sb32(<< C:8, Rest/bitstring >>) ->
    << (sb322c(C)):5, (from_sb32(Rest))/bitstring >>;
from_sb32(<< >>) ->
    << >>.

c2sb32(I) when 0  =< I, I =< 9  -> $0 + I;
c2sb32(I) when 10 =< I, I =< 31 -> $A + I - 10.

sb322c(I) when $0 =< I, I =< $9 -> I - $0;
sb322c(I) when $A =< I, I =< $V -> I - $A + 10.

%% @spec to_hex(Bin::binary()) -> binary()
%% @doc Converts a binary into a hex-encoded binary
%% This is conventional hex encoding, with the proviso that
%% only capital letters are used, e.g. `0..9A..F'.
%% @end
to_hex(Bin) ->
    << << (nib2hex(N)):8 >> || <<N:4>> <= Bin >>.

%% @spec from_hex(Bin::binary()) -> binary()
%% @doc Converts from a hex-encoded binary into a 'normal' binary
%%
%% This function is the reverse of {@link to_hex/1}.
%%
from_hex(Bin) ->
    << << (hex2nib(H)):4 >> || <<H:8>> <= Bin >>.

nib2hex(N) when  0 =< N, N =< 9 -> $0 + N;
nib2hex(N) when 10 =< N, N =< 15-> $A + N - 10.

hex2nib(C) when $0 =< C, C =< $9 -> C - $0;
hex2nib(C) when $A =< C, C =< $F -> C - $A + 10.

-ifdef(DEBUG).
pp(none) -> "<none>";
pp(B) when is_bitstring(B) ->
    [ $0 + I || <<I:1>> <= B ].
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    L = test_list(),
    [{I,I} = {I,catch decode(encode(I))} || I <- L].

test_list() ->
    [-456453453477456464.45456,
     -5.23423564,
     -1.234234,
     -1.23423,
     -0.345,
     -0.34567,
     -0.0034567,
     0,
     0.00012345,
     0.12345,
     1.2345,
     123.45,
     456453453477456464.45456,
     a,
     aaa,
     {},
     {1},
     {1,2},
     {"","123"},
     {"1","234"},
     <<>>,
     <<1>>,
     <<1,5:3>>,
     <<1,5:4>>,
     [1,2,3],
     [],
     self(),
     spawn(fun() -> ok end),
     make_ref(),
     make_ref()|
     lists:sublist(erlang:ports(),1,2)].

-endif.
