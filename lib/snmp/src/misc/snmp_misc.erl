%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%% 

-module(snmp_misc).

%% need definition of mib record
-include("snmp_types.hrl").
-include("snmpc_misc.hrl").

-define(VMODULE,"MISC").
-include("snmp_verbosity.hrl").


-export([assq/2,
	 bits_to_int/2,
	 diff/2,
	 ensure_trailing_dir_delimiter/1,
	 foreach/3, 
	 format_pdu/2,
	 format_val/4,
	 format_vb/2,
	 format_vbs/2,
	 format/3,
	 get_option/2,
	 get_option/3,
	 get_sec_level/1,
	 ip/1, ip/2, 
	 is_auth/1,
	 is_BitString/1,
	 is_crypto_supported/1, 
	 is_oid/1,
	 is_priv/1,
	 is_reportable/1,
	 is_reportable_pdu/1,
	 is_string/1,
	 is_tag_member/2,
	 is_tmask_match/3,
	 keyreplaceadd/4,
	 mem_size/1,
	 mk_msg_flags/2,
	 multi_map/2,
	 %% now/0,
	 now/1,
	 read_mib/1,
	 set_option/3,
	 sleep/1,
	 strip_extension_from_filename/2, 
	 str_xor/2,
	 time/3,

	 verify_behaviour/2,

	 %% These are used both for debugging (verbosity printouts)
         %% and other such "utility" operations.
         format_timestamp/1, format_timestamp/2, 
         format_short_timestamp/1, format_short_timestamp/2, 
         format_long_timestamp/1, format_long_timestamp/2,
         formated_timestamp/0, 
         formated_short_timestamp/0, 
         formated_long_timestamp/0

	]).


verify_behaviour(Behaviour, UserMod) 
  when is_atom(Behaviour) andalso is_atom(UserMod) ->
    case (catch UserMod:module_info(exports)) of
        Exps when is_list(Exps) ->
            Callbacks = Behaviour:behaviour_info(callbacks),
            (catch verify_behaviour2(Callbacks, Exps));
        _ ->
            {error, {bad_module, UserMod}}
    end;
verify_behaviour(_, BadModule) ->
    {error, {bad_module, BadModule}}.

verify_behaviour2([], _) ->
    ok;
verify_behaviour2([{Func, Arity} = FuncArity|Callbacks], Exps) ->
    case lists:member(FuncArity, Exps) of
        true ->
            verify_behaviour2(Callbacks, Exps);
        false ->
            throw({error, {bad_module, {function, Func, Arity}}})
    end.


sleep(Time) ->
    receive
	after Time ->
		true
    end.


%% Returns time in ms = sec/1000
% now() -> now(ms).
now(ms) ->
    erlang:monotonic_time(milli_seconds);

%% Returns time in cs = sec/100
now(cs) ->
    erlang:monotonic_time(100);

now(sec) ->
    erlang:monotonic_time(seconds).
    


%% ---------------------------------------------------------------------------
%% # formated_timstamp/0,     formated_timstamp/1
%% # format_short_timstamp/0, format_short_timstamp/1
%% # format_long_timstamp/0,  format_long_timstamp/1
%% 
%% Create a formatted timestamp. Short means that it will not include 
%% the date in the formatted timestamp. Also it will only include millis.
%% ---------------------------------------------------------------------------

formated_timestamp() ->
    formated_long_timestamp().

formated_short_timestamp() ->
    format_short_timestamp(os:timestamp()).

formated_long_timestamp() ->
    format_long_timestamp(os:timestamp()).


%% ---------------------------------------------------------------------------
%% # format_timstamp/1, format_timstamp/2
%% # format_short_timstamp/1, format_short_timstamp/2
%% # format_long_timstamp/1, format_long_timstamp/2
%% 
%% Formats the provided timestamp. Short means that it will not include 
%% the date in the formatted timestamp.
%% ---------------------------------------------------------------------------

-spec format_timestamp(Now :: erlang:timestamp()) ->
    string().

format_timestamp(Now) ->
    format_long_timestamp(Now).

-spec format_short_timestamp(Now :: erlang:timestamp()) ->
    string().

format_short_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(short, Now, N2T).

-spec format_long_timestamp(Now :: erlang:timestamp()) ->
    string().

format_long_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(long, Now, N2T).

-spec format_timestamp(Now :: erlang:timestamp(), 
                       N2T :: function()) ->
    string().

format_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_long_timestamp(Now, N2T).

-spec format_short_timestamp(Now :: erlang:timestamp(), 
                             N2T :: function()) ->
    string().

format_short_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(short, Now, N2T).

-spec format_long_timestamp(Now :: erlang:timestamp(), 
                            N2T :: function()) ->
    string().

format_long_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(long, Now, N2T).

format_timestamp(Format, {_N1, _N2, N3} = Now, N2T) ->
    {Date, Time} = N2T(Now),
    do_format_timestamp(Format, Date, Time, N3).

do_format_timestamp(short, _Date, Time, N3) ->
    do_format_short_timestamp(Time, N3);
do_format_timestamp(long, Date, Time, N3) ->
    do_format_long_timestamp(Date, Time, N3).
    
do_format_long_timestamp(Date, Time, N3) ->
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                      [YYYY, MM, DD, Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).

do_format_short_timestamp(Time, N3) ->
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w", 
                      [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).



is_crypto_supported(Alg) ->
    %% The 'try catch' handles the case when 'crypto' is
    %% not present in the system (or not started).
    try
	begin
	    Supported = crypto:supports(),
	    Hashs     = proplists:get_value(hashs,   Supported),
	    Ciphers   = proplists:get_value(ciphers, Supported),
	    lists:member(Alg, Hashs ++ Ciphers)
	end
    catch
	_:_ ->
	    false
    end.

is_string([]) -> true;
is_string([Tkn | Str]) 
  when is_integer(Tkn) andalso (Tkn >= 0) andalso (Tkn =< 255) ->
    is_string(Str);
is_string(_) -> false.

is_oid([E1, E2| Rest]) 
  when (length(Rest) =< 126) andalso (E1 *40 + E2 =< 255) ->
    is_oid2(Rest);
is_oid([E1]) when E1 =< 2 ->
    true;
is_oid(_) -> false.

is_oid2([]) -> true;
is_oid2([Nbr | RestOid]) 
  when is_integer(Nbr) andalso (0 =< Nbr) andalso (Nbr =< 2147483647) ->
    is_oid2(RestOid);
is_oid2(_) -> false.
    
is_BitString([]) -> true;
is_BitString([Nbr | RestBitstring]) 
  when is_integer(Nbr) andalso (Nbr >= 0) andalso (Nbr =< 1) ->
    is_BitString(RestBitstring);
is_BitString(_) -> false.
    

%% Check if a Tag is a member in a TagList.  Tags and TagLists are defined
%% in SNMP-TARGET-MIB
is_tag_member(Tag, TagList) ->
    check_tag_list(TagList, [], lists:reverse(Tag)).

check_tag_list([32 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([9 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([13 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([11 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([Char | T], Res, Gat) ->
    check_tag_list(T, [Char | Res], Gat);
check_tag_list([], Res, Gat) ->
    tag_delimiter_found(Res, Gat, []).

tag_delimiter_found(Gat, Gat, _T) ->
    true;
tag_delimiter_found(_Res, _Gat, []) ->
    false;
tag_delimiter_found(_Res, Gat, T) ->
    check_tag_list(T, [], Gat).
    

%% Pre: length(TAddr1) == length(TAddr2)
%%      length(TMask) == 0 | length(TAddr1)
is_tmask_match(_TAddr1, _TAddr2, []) ->
    true;
is_tmask_match([H1 | T1], [H2 | T2], [M1 | M2]) ->
    if
	(H1 band M1) == (H2 band M1) ->
	    is_tmask_match(T1, T2, M2);
	true ->
	    false
    end.
    

%%--------------------------------------------------
%% Not a real assq, but what the heck, it's useful.
%%--------------------------------------------------
assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> {value, Val};
	_ -> false
    end.
    
get_option(Key, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> 
	    Value;
	_ ->
	    throw({error, {not_found, Key}})
    end.

get_option(Key, Options, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> 
	    Value;
	_ -> 
	    Default
    end.

set_option(Key, Val, Opts) ->
    keyreplaceadd(Key, 1, Opts, {Key, Val}).

keyreplaceadd(Key, Pos, List, New) ->
    case lists:keysearch(Key, Pos, List) of
	{value, _} -> lists:keyreplace(Key, Pos, List, New);
	_ -> [New | List]
    end.

is_auth(SecLevel) ->
    1 == (SecLevel band 1).

is_priv(SecLevel) ->
    2 == (SecLevel band 2).

is_reportable([MsgFlag]) ->
    4 == (MsgFlag band 4).

%% [OTP-3416] 
%% [RFC 2571] Confirmed Class: GetRequest-PDU, GetNextRequest-PDU, 
%% GetBulkRequest-PDU, SetRequest-PDU, and InformRequest-PDU. 
%% Unconfirmed Class: Report-PDU, Trapv2-PDU, and GetResponse-PDU. 
%% [RFC 2572] The reportableFlag MUST always be zero when the message 
%% contains a PDU from the Unconfirmed Class; it MUST always be one 
%% for a PDU from the Confirmed Class, 
%%
is_reportable_pdu('get-request') -> true;
is_reportable_pdu('get-next-request') -> true;
is_reportable_pdu('get-bulk-request') -> true;
is_reportable_pdu('set-request') -> true;
is_reportable_pdu('inform-request') -> true;
is_reportable_pdu(_) -> false.

mk_msg_flags(PduType, SecLevel) ->
    Flags1 = case is_reportable_pdu(PduType) of
		 true -> 4;
		 false -> 0
	     end,
    [Flags1 bor SecLevel].

get_sec_level([Flag]) ->
    SecLevel = Flag band 3,
    case {is_auth(SecLevel), is_priv(SecLevel)} of
	{false, false} -> noAuthNoPriv;
	{true, false} -> authNoPriv;
	{true, true} -> authPriv
    end.


%% diff(L1, L2) -> L1 - L2.
%% Ex. [1, 2, 3, 4] - [1, 3, 4] = [2, 3, 4]
diff(L1, []) -> L1;
diff([H | T1], [H | T2]) -> diff(T1, T2);
diff(L1, _) -> L1.

foreach(Function, ExtraArgs, [H | T]) ->
    apply(Function, [H | ExtraArgs]),
    foreach(Function, ExtraArgs, T);
foreach(_Function, _ExtraArgs, []) -> true.

str_xor([H1|T1], [H2|T2]) ->
    [H1 bxor H2 | str_xor(T1, T2)];
str_xor([], []) ->
    [].


%%-----------------------------------------------------------------
%% Pre: ListOfLists is a list of N lists, each of length M.
%%      Func is a function of arity N.
%% Returns: A list of length M where element Y is the result of
%%          applying Func on [Elem(Y, List1), ..., Elem(Y, ListN)].
%%-----------------------------------------------------------------
multi_map(_Func, [[] | _ListOfLists]) -> 
    [];
multi_map(Func, ListOfLists) ->
    HD = [hd(L) || L <- ListOfLists], 
    TL = [tl(L) || L <- ListOfLists],
%%     io:format("multi_map -> "
%% 	      "~n   HD: ~p"
%% 	      "~n   TL: ~p", [HD, TL]),
    [
     apply(Func, HD) | multi_map(Func, TL)
    ].

%% Primitive performance analysis.
time(M,F,A) ->
    statistics(runtime),
    R = apply(M, F, A),
    {R, statistics(runtime)}.

%% How much memory is allocated for X? At least some kind of upper estimation...
mem_size(X) ->
    E = ets:new(tmp, [set, protected]),
    M1 = ets:info(E, memory),
    ets:insert(E, {make_ref(), X}),
    M2 = ets:info(E, memory),
    ets:delete(E),
    M2 - M1.


strip_extension_from_filename(FileName, Ext) when is_atom(FileName) ->
    strip_extension_from_filename(atom_to_list(FileName), Ext);

strip_extension_from_filename(FileName, Ext) when is_list(FileName) ->
    case lists:suffix(Ext, FileName) of
	true -> lists:sublist(FileName, 1, length(FileName) - length(Ext));
	false -> FileName
    end.


%%----------------------------------------------------------------------
%% Returns: {ok, Mib}|{error, Reason}
%% 
%%----------------------------------------------------------------------
read_mib(FileName) ->
    (catch do_read_mib(FileName)).

do_read_mib(FileName) ->
    ?read_mib(FileName).


%%----------------------------------------------------------------------
%% Converts a list of named bits to the integer value.
%% Returns: integer()|error
%%----------------------------------------------------------------------
bits_to_int(Val,Kibbles) ->
    bits_to_int(Val,Kibbles,0).

bits_to_int([],_Kibbles,Res) -> Res;
bits_to_int([Kibble|Ks],Kibbles,Res) ->
    case snmp_misc:assq(Kibble,Kibbles) of
	{value,V} ->
	    bits_to_int(Ks,Kibbles,Res + round(math:pow(2,V)));
	_ ->
	    error
    end.

			     
%%----------------------------------------------------------------------
%% Returns: {ok, {int(),int(),int(),int()}} | 
%%          {ok, {int(),int(),int(),int()},int(),int(),int(),int()} | 
%%          {error, Reason}
%%----------------------------------------------------------------------
ip(Host) ->
    ip(Host, inet).

ip(Host, Family) ->
    inet:getaddr(Host, Family).

ensure_trailing_dir_delimiter([]) -> "/";
ensure_trailing_dir_delimiter(DirSuggestion) ->
    case lists:last(DirSuggestion) of
	$/ -> DirSuggestion;
	_ -> lists:append(DirSuggestion,"/")
    end.


format_pdu(PDU, MiniMib) when is_record(PDU, pdu) ->
    #pdu{type         = T, 
	 error_status = ES, 
	 error_index  = EI,
	 request_id   = RID,
	 varbinds     = VBs} = PDU,
    Txt1 = if
	       (ES =:= noError) andalso (EI =:= 0) -> 
		   "";
	       (T =:= 'get-bulk-request') ->
		   "";
	       true ->
		   io_lib:format("*!*!* An error occured. *!*!* ~n"
				 "Error status = ~w, index = ~w.~n",
				 [ES, EI])
	   end,
    Txt2 = if T =:= 'snmpv2-trap' ->
		   io_lib:format("v2 Trap,          Request Id:~w~n", [RID]);
	      T =:= 'get-request' ->
		   io_lib:format("Get request,      Request Id:~w~n", [RID]);
	      T =:= 'get-next-request' ->
		   io_lib:format("Get-Next request, Request Id:~w~n", [RID]);
	      T =:= 'get-bulk-request' ->
		   io_lib:format("Get-Bulk request, Request Id:~w~n"
				 "  Non-repeaters = ~w~n"
				 "  Max-repetitions = ~w~n", [RID, ES, EI]);
	      T =:= 'set-request' ->
		   io_lib:format("Set request,      Request Id:~w~n", [RID]);
	      T =:= 'get-response' ->
		   io_lib:format("Response,         Request Id:~w~n", [RID]);
	      T =:= 'inform-request' ->
		   io_lib:format("Inform Request    Request Id:~w~n", [RID]);
	      T =:= report ->
		   io_lib:format("Report            Request Id:~w~n", [RID]);
	      true -> 
		   ""
	   end,
    [Txt1, Txt2, format_vbs(VBs, MiniMib)|"\n"];

format_pdu(#trappdu{enterprise    = Enterprise, 
		    agent_addr    = AgentAddr,
		    generic_trap  = GenericTrap, 
		    specific_trap = SpecificTrap,
		    time_stamp    = TimeStamp, 
		    varbinds      = VBs}, MiniMib) ->
    [io_lib:format("v1 Trap~n"
		   "     Generic: ~w~n"
		   "  Enterprise: ~w~n"
		   "    Specific: ~w~n"
		   "  Agent addr: ~w~n"
		   "   TimeStamp: ~w~n",
		   [GenericTrap,
		    element(1,symbolify_oid(MiniMib,Enterprise)),SpecificTrap,
		    AgentAddr, TimeStamp]),
     format_vbs(VBs, MiniMib) | "\n"].

format_vbs(Vbs, MiniMib) ->
    [format_vb(VB, MiniMib) || VB <- Vbs].

format_vb(#varbind{oid          = Oid, 
		   variabletype = Type, 
		   value        = Value}, MiniMib) ->
    {Soid, Mtype} = symbolify_oid(MiniMib, Oid),
    [io_lib:format("  ~w = ", [Soid]),
     format_val(Type, Mtype, Value, MiniMib) | "\n"].

format(Max, F, A) when is_integer(Max) ->
    case lists:flatten(io_lib:format(F,A)) of
	S when length(S) > Max ->
	    case lists:suffix("\n", S) of
		true ->
		    lists:concat([lists:sublist(S,Max), "...\n"]);
		false ->
		    lists:concat([lists:sublist(S,Max), "..."])
	    end;
	S ->
	    S
    end.


%%----------------------------------------------------------------------
%% Returns: (a nested) symbolified oid.
%%----------------------------------------------------------------------
symbolify_oid(MiniMib, Oid) ->
    case snmp_mini_mib:aliasname(MiniMib, Oid) of
	false ->
	    {Oid, unknown};
	{FoundOid, Aliasname, Type} ->
 	    Rest = snmp_misc:diff(Oid, FoundOid),
 	    {[Aliasname| Rest], Type}
    end.

format_val('OCTET STRING', 'BITS', Val, _MiniMib) ->
    io_lib:format("~w", [snmp_pdus:octet_str_to_bits(Val)]);
format_val('OBJECT IDENTIFIER', _, Val, MiniMib) ->
    {NVal, _} = symbolify_oid(MiniMib, Val),
    io_lib:format("~w", [NVal]);
format_val(_, _, Val, _MiniMib) ->
    io_lib:format("~p", [Val]).
