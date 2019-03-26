%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(snmpa_get_lib).

-export([
         try_get/2,
         try_get_instance/1,

         delete_prefixes/2,
         delete_index/1,

         sort_vbs/1,
         split_gb_vbs/2,
         split_vbs/1, split_vbs/3,
         split_vbs_view/2,

         make_value_a_correct_value/3,

         dbg_apply/3,

         user_err/2
        ]).



%% *** sort_vbs ***

-spec sort_vbs(VBs :: [snmp:varbind()]) -> [snmp:ivarbind()].

sort_vbs(VBs) ->
    snmpa_svbl:sort_varbindlist(get(mibserver), VBs).


%% *** split_vbs ***

-spec split_gb_vbs(NonRepeaters :: integer(),
                   VBs          :: [varbind()]) ->
                          {NonRepVBs :: [varbind()], RestVBs :: [varbind()]}.

split_gb_vbs(N, VBs) ->
    split_gb_vbs(N, VBs, []).

split_gb_vbs(N, Varbinds, Res) when N =< 0 ->
    {Res, Varbinds};
split_gb_vbs(N, [H | T], Res) ->
    split_gb_vbs(N-1, T, [H | Res]);
split_gb_vbs(_N, [], Res) ->
    {Res, []}.
     


%% *** split_vbs_view ***

-spec split_vbs_view(VBs     :: [varbind()],
                     MibView :: snmp_view_based_acm_mib:mibview()) ->
                            {OutSideView :: [varbind()],
                             InSideView  :: [varbind()]}.

split_vbs_view(VBs, MibView) ->
    ?vtrace("split the varbinds view", []),
    split_vbs_view(VBs, MibView, [], []).

split_vbs_view([], _MibView, Out, In) ->
    {Out, In};
split_vbs_view([Vb | Vbs], MibView, Out, In) ->
    case snmpa_acm:validate_mib_view(Vb#varbind.oid, MibView) of
	true -> split_vbs_view(Vbs, MibView, Out, [Vb | In]);
	false -> split_vbs_view(Vbs, MibView,
				[Vb#varbind{value = noSuchObject} | Out], In)
    end.



%% *** split_vbs/1,3 ***
%%
%% Sort out the 'end of' things (mib view of tables).
%%

-spec split_vbs(VBs :: [varbind()]) ->
                       {ok, Res :: [varbind()], EndOfs :: [varbond()]}.

split_vbs(VBs) ->
    split_vbs(VBs, [], []).

-spec split_vbs(VBs :: [varbind()],
                Res :: [varbind()],
                EndOfs :: [varbond()]) ->
                       {ok, Res :: [varbind()], EndOfs :: [varbond()]}.

split_vbs([], Res, EndOfs) ->
    {ok, Res, EndOfs};
split_vbs([Vb | Vbs], Res, EndOfs) ->
    case Vb#varbind.value of
	{endOfMibView, _} -> split_varbinds(Vbs, Res, [Vb | EndOfs]);
	{endOfTable, _}   -> split_varbinds(Vbs, Res, [Vb | EndOfs]);
	_                 -> split_varbinds(Vbs, [Vb | Res], EndOfs)
    end.


next_oid(Oid) ->
    case lists:reverse(Oid) of
	[H | T] -> lists:reverse([H+1 | T]);
	[] -> []
    end.



%% *** delete_prefixes ***

-spec delete_prefixes(Prefix :: oid(),
                      IVBs   :: [ivarbind()]) ->
                             [{ShortOID :: snmp:oid(), 
                               ASN1Type :: snmp:asn1_type()}].

delete_prefixes(Prefix, IVBs) ->
    [{snmp_misc:diff(Oid, Prefix), ME#me.asn1_type} ||
        #ivarbind{varbind = #varbind{oid = Oid}, mibentry = ME} <- IVBs].



%% *** delete_index ***

-spec delete_index(TableOids :: [{Col :: integer(), 
                                  Val :: term(), 
                                  OrgIndex :: integer()}]) ->
                          [Col :: integer()].

delete_index(TableOids) ->
    [Col || {Col, _Val, _OrgIndex} <- TableOids].



%%-----------------------------------------------------------------
%% transforms a (hopefully correct) return value ((perhaps) from a 
%% mib-function) to a typed and guaranteed correct return value.
%% An incorrect return value is transformed to {error, genErr}.
%% A correct return value is on the form: 
%% {error, <error-msg>} | {value, <variable-type>, <value>}
%%-----------------------------------------------------------------

-spec make_value_a_correct_value(Value :: term(),
                                 ASN1  :: snmp:asn1_type(),
                                 MFA   :: term()) ->
                                        {error, Error :: atom()} |
                                        {value, ValueType :: atom(), 
                                         Value :: term()}.

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'INTEGER' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Counter32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Unsigned32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'TimeTicks' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Counter64' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when (Asn1#asn1_type.bertype =:= 'BITS') andalso is_list(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    case snmp_misc:bits_to_int(Val,Kibbles) of
	error ->
	    wrongValue(Val, Mfa);
	Int ->
	    make_value_a_correct_value({value,Int},Asn1,Mfa)
    end;

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when (Asn1#asn1_type.bertype =:= 'BITS') andalso is_integer(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    {_Kibble,BitNo} = lists:last(Kibbles),
    case (1 bsl (BitNo+1)) of
	X when Val < X ->
	    {value,'BITS',Val};
	_Big ->
	    wrongValue(Val, Mfa)
    end;

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'OCTET STRING',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'OCTET STRING');

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'IpAddress',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'IpAddress');

make_value_a_correct_value({value, Oid},
			   #asn1_type{bertype = 'OBJECT IDENTIFIER'},
			   _Mfa) ->
    case snmp_misc:is_oid(Oid) of
	true  -> {value, 'OBJECT IDENTIFIER', Oid};
	_Else -> {error, wrongType}
    end;

make_value_a_correct_value({value, Val}, Asn1, _Mfa)
  when Asn1#asn1_type.bertype =:= 'Opaque' ->
    if is_list(Val) -> {value, 'Opaque', Val};
       true -> {error, wrongType}
    end;

make_value_a_correct_value({noValue, noSuchObject}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchObject};
make_value_a_correct_value({noValue, noSuchInstance}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value({noValue, noSuchName}, _ASN1Type, _Mfa) ->
    %% Transform this into a v2 value.  It is converted to noSuchName
    %% later if it was v1.  If it was v2, we use noSuchInstance.
    {value, noValue, noSuchInstance};
%% For backwards compatibility only - we really shouldn't allow this;
%% it makes no sense to return unSpecified for a variable! But we did
%% allow it previously. -- We transform unSpecified to noSuchInstance
%% (OTP-3303).
make_value_a_correct_value({noValue, unSpecified}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value(genErr, _ASN1Type, _MFA) ->
    {error, genErr};

make_value_a_correct_value(_WrongVal, _ASN1Type, undef) ->
    {error, genErr};

make_value_a_correct_value(WrongVal, ASN1Type, Mfa) ->
    user_err("Got ~w from ~w. (~w) Using genErr",
	     [WrongVal, Mfa, ASN1Type]),
    {error, genErr}.

check_integer(Val, Asn1, Mfa) ->
    case Asn1#asn1_type.assocList of
	undefined -> check_size(Val, Asn1, Mfa);
	Alist ->
	    case snmp_misc:assq(enums, Alist) of
		{value, Enums} -> check_enums(Val, Asn1, Enums, Mfa);
		false -> check_size(Val, Asn1, Mfa)
	    end
    end.

check_octet_string(String, Hi, Lo, Mfa, Type) ->
    Len = (catch length(String)), % it might not be a list
    case snmp_misc:is_string(String) of
	true when Lo =:= undefined -> {value, Type, String};
	true when Len =< Hi, Len >= Lo ->
	    {value, Type, String};
	true ->
	    wrongLength(String, Mfa);
	_Else ->
	    wrongType(String, Mfa)
    end.

check_size(Val, #asn1_type{lo = Lo, hi = Hi, bertype = Type}, Mfa) 
  when is_integer(Val) ->
    ?vtrace("check size of integer: "
	    "~n   Value:       ~p"
	    "~n   Upper limit: ~p"
	    "~n   Lower limit: ~p"
	    "~n   BER-type:    ~p",
	    [Val,Hi,Lo,Type]),
    if
	(Lo =:= undefined) andalso (Hi =:= undefined) -> {value, Type, Val};
	(Lo =:= undefined) andalso is_integer(Hi) andalso (Val =< Hi) ->
	    {value, Type, Val};
	is_integer(Lo) andalso (Val >= Lo) andalso (Hi =:= undefined) ->
	    {value, Type, Val};
	is_integer(Lo) andalso is_integer(Hi) andalso (Val >= Lo) andalso (Val =< Hi) ->
	    {value, Type, Val};
	true ->
	    wrongValue(Val, Mfa)
    end;
check_size(Val, _, Mfa) ->
    wrongType(Val, Mfa).

check_enums(Val, Asn1, Enums, Mfa) ->
    Association = 
	if
	    is_integer(Val) -> lists:keysearch(Val, 2, Enums);
	    is_atom(Val)    -> lists:keysearch(Val, 1, Enums);
	    true            -> {error, wrongType}
    end,
    case Association of
	{value, {_AliasIntName, Val2}} -> 
	    {value, Asn1#asn1_type.bertype, Val2};
	false ->
	    wrongValue(Val, Mfa);
	{error, wrongType} ->
	    wrongType(Val, Mfa)
    end.

wrongLength(Val, Mfa) ->
    report_err(Val, Mfa, wrongLength).

wrongValue(Val, Mfa) ->
    report_err(Val, Mfa, wrongValue).

wrongType(Val, Mfa) ->
    report_err(Val, Mfa, wrongType).

report_err(_Val, undef, Err) ->
    {error, Err};
report_err(Val, Mfa, Err) ->
    user_err("Got ~p from ~w. Using ~w", [Val, Mfa, Err]),
    {error, Err}.




is_valid_pdu_type('get-request')      -> true;
is_valid_pdu_type('get-next-request') -> true;
is_valid_pdu_type('get-bulk-request') -> true;
is_valid_pdu_type('set-request')      -> true;
is_valid_pdu_type(_)                  -> false.



%%-----------------------------------------------------------------
%% Func: try_get/2
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind |
%%          List of #varbind
%%-----------------------------------------------------------------

-spec try_get(IVB            :: ivarbind(),
              IsNotification :: boolean()) ->
                     {error, ErrorStatus, OrgIndex} | varbind() | [varbind()].

try_get(IVb, IsNotification) when is_record(IVb, ivarbind) ->
    ?vtrace("try_get(ivarbind) -> entry with"
	    "~n   IVb: ~p", [IVb]),
    get_var_value_from_ivb(IVb, IsNotification);
try_get({TableOid, TableVbs}, IsNotification) ->
    ?vtrace("try_get(table) -> entry with"
	    "~n   TableOid: ~p"
	    "~n   TableVbs: ~p", [TableOid, TableVbs]),
    [#ivarbind{mibentry = MibEntry}|_] = TableVbs,
    {NoAccessVbs, AccessVbs} =
	check_all_table_vbs(TableVbs, IsNotification, [], []),
    case get_tab_value_from_mib(MibEntry, TableOid, AccessVbs) of
	{error, ErrorStatus, OrgIndex} ->
	    {error, ErrorStatus, OrgIndex};
	NVbs ->
	    NVbs ++ NoAccessVbs
    end.


%%-----------------------------------------------------------------
%% Func: try_get_instance/1
%% Returns: {value, noValue, term()} |
%%          {value, Type, Value} |
%%          {error, ErrorStatus}
%%-----------------------------------------------------------------

-spec try_get_instance(ME :: snmp:me()) ->
                              {value, noValue, term()} |
                              {value, Type :: asn1_type(), Value :: term()} |
                              {error, error_status()}.

try_get_instance(#me{mfa = {M, F, A}, asn1_type = ASN1Type}) ->
    ?vtrace("try_get_instance -> entry with"
	    "~n   M: ~p"
	    "~n   F: ~p"
	    "~n   A: ~p", [M,F,A]),
    Result = (catch dbg_apply(M, F, [get | A])),
    % mib shall return {value, <a-nice-value-within-range>} |
    % {noValue, noSuchName} (v1) | 
    % {noValue, noSuchObject | noSuchInstance} (v2, v1)
    % everything else (including 'genErr') will generate 'genErr'.
    make_value_a_correct_value(Result, ASN1Type, {M, F, A}).



%%-----------------------------------------------------------------
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind
%%-----------------------------------------------------------------

get_var_value_from_ivb(#ivarbind{status   = noError,
                                 mibentry = ME,
                                 varbind  = VB} = IVb, IsNotification) ->
    ?vtrace("get_var_value_from_ivb(noError) -> entry", []),
    #varbind{org_index = OrgIndex, oid = Oid} = Vb,
    case ME#me.access of
	'not-accessible' -> 
	    Vb#varbind{value = noSuchInstance};
	'accessible-for-notify' when (IsNotification =:= false) -> 
	    Vb#varbind{value = noSuchInstance};
	'write-only' -> 
	    Vb#varbind{value = noSuchInstance};
	_ -> 
	    case get_var_value_from_mib(Me, Oid) of
		{value, Type, Value} ->
		    Vb#varbind{variabletype = Type, value = Value};
		{error, ErrorStatus} ->
		    {error, ErrorStatus, OrgIndex}
	    end
    end;
get_var_value_from_ivb(#ivarbind{status = Status, varbind = VB}, _) ->
    ?vtrace("get_var_value_from_ivb(~p) -> entry", [Status]),
    VB#varbind{value = Status}.



%%-----------------------------------------------------------------
%% Func: get_var_value_from_mib/1
%% Purpose: 
%% Pre:     Oid is a correct instance Oid (lookup checked that).
%% Returns: {error, ErrorStatus} |
%%          {value, Type, Value}
%% Returns: A correct return value (see make_value_a_correct_value)
%%-----------------------------------------------------------------
get_var_value_from_mib(#me{entrytype = variable,
			   asn1_type = ASN1Type,
			   mfa       = {Mod, Func, Args}},
		       _Oid) ->
    ?vtrace("get_var_value_from_mib(variable) -> entry when"
	    "~n   Mod:  ~p"
	    "~n   Func: ~p"
	    "~n   Args: ~p", [Mod, Func, Args]),
    Result = (catch dbg_apply(Mod, Func, [get | Args])),
    % mib shall return {value, <a-nice-value-within-range>} |
    % {noValue, noSuchName} (v1) | 
    % {noValue, noSuchObject | noSuchInstance} (v2, v1)
    % everything else (including 'genErr') will generate 'genErr'.
    make_value_a_correct_value(Result, ASN1Type, {Mod, Func, Args});

get_var_value_from_mib(#me{entrytype = table_column,
			   oid       = MeOid,
			   asn1_type = ASN1Type,
			   mfa       = {Mod, Func, Args}},
		       Oid) ->
    ?vtrace("get_var_value_from_mib(table_column) -> entry when"
	    "~n   MeOid: ~p"
	    "~n   Mod:   ~p"
	    "~n   Func:  ~p"
	    "~n   Args:  ~p"
	    "~n   Oid:   ~p", [MeOid, Mod, Func, Args, Oid]),
    Col      = lists:last(MeOid),
    Indexes  = snmp_misc:diff(Oid, MeOid),
    [Result] = (catch dbg_apply(Mod, Func, [get, Indexes, [Col] | Args])),
    make_value_a_correct_value(Result, ASN1Type, 
                               {Mod, Func, Args, Indexes, Col}).



%%-----------------------------------------------------------------
%% Runtime debugging of the agent.
%%-----------------------------------------------------------------

-spec dbg_apply(M :: atom(), F :: atom(), A :: list()) ->
                       any().

dbg_apply(M, F, A) ->
    case get(verbosity) of
	silence -> 
	    apply(M,F,A);
	_ ->
	    ?vlog("~n   apply: ~w,~w,~p~n", [M,F,A]),
	    Res = (catch apply(M,F,A)),
	    case Res of
		{'EXIT', Reason} ->
		    ?vinfo("Call to: "
			   "~n   Module:   ~p"
			   "~n   Function: ~p"
			   "~n   Args:     ~p"
			   "~n"
			   "~nresulted in an exit"
			   "~n"
			   "~n   ~p", [M, F, A, Reason]);
		_ ->
		    ?vlog("~n   returned: ~p", [Res])
	    end,
	    Res
    end.


%% ---------------------------------------------------------------------

user_err(F, A) ->
    snmpa_error:user_err(F, A).


