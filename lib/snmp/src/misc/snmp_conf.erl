%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%% 
%%----------------------------------------------------------------------
%% Purpose: Basic module for reading and verifying config files
%%----------------------------------------------------------------------
-module(snmp_conf).


%% External exports
-export([read_files/2, read/2]).

%% Basic (type) check functions
-export([check_mandatory/2,
	 check_integer/1, check_integer/2,
	 
	 check_string/1, check_string/2,

	 check_atom/2, 

	 check_timer/1,

	 check_ip/1, check_taddress/1, 
	 
	 check_packet_size/1, 

	 check_oid/1,
	 
	 check_mp_model/1, 
	 check_sec_model/1, check_sec_model/2, check_sec_model/3, 
	 check_sec_level/1,

	 all_integer/1
	]).


-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").

-define(VMODULE,"CONF").
-include("snmp_verbosity.hrl").


%%-----------------------------------------------------------------

read_files(Dir, Files) when is_list(Dir) andalso is_list(Files) ->
    read_files(Dir, Files, []).

read_files(_Dir, [], Res) ->
    lists:reverse(Res);
read_files(Dir, [{Gen, Filter, Check, FileName}|Files], Res) 
  when is_function(Filter) andalso 
       is_function(Check)  andalso 
       is_list(FileName) ->
    ?vdebug("read_files -> entry with"
	"~n   FileName: ~p", [FileName]),
    File = filename:join(Dir, FileName),
    case file:read_file_info(File) of
        {ok, _} ->
            Confs = read(File, Check),
            read_files(Dir, Files, [Filter(Confs)|Res]);
        {error, R} ->
	    ?vlog("failed reading file info for ~s: "
		  "~n   ~p", [FileName, R]),
            Gen(Dir),
            read_files(Dir, Files, [Filter([])|Res])
    end.


%% Ret. Res | exit(Reason)
read(File, Check) when is_function(Check) -> 
    ?vdebug("read -> entry with"
	"~n   File: ~p", [File]),
    Fd = open_file(File),

    case loop(Fd, [], Check, 1, File) of
        {error, Reason} ->     
            file:close(Fd),
            error(Reason);
        {ok, Res} ->
            file:close(Fd),
            Res
    end.

open_file(File) ->
    case file:open(File, [read]) of
        {ok, Fd} -> 
            Fd;
        {error, Reason} ->
            error({failed_open, File, Reason})
    end.

loop(Fd, Res, Check, StartLine, File) ->
    case do_read(Fd, "", StartLine) of
        {ok, Row, EndLine} ->
	    ?vtrace("loop -> "
		    "~n   Row:     ~p"
		    "~n   EndLine: ~p", [Row, EndLine]),
            case (catch Check(Row)) of
                ok -> 
		    ?vtrace("loop -> ok", []),
                    loop(Fd, [Row | Res], Check, EndLine, File);
                {ok, NewRow} ->
		    ?vtrace("loop -> ok: "
			    "~n   NewRow: ~p", [NewRow]),
                    loop(Fd, [NewRow | Res], Check, EndLine, File);
                {error, Reason} ->
		    ?vtrace("loop -> check error: "
			    "~n   Reason: ~p", [Reason]),
                    {error, {failed_check, File, StartLine, EndLine, Reason}};
                Error ->
		    ?vtrace("loop -> check failure: "
			    "~n   Error: ~p", [Error]),
                    {error, {failed_check, File, StartLine, EndLine, Error}}
		end;
        {error, EndLine, Error} ->
	    ?vtrace("loop -> read failure: "
		    "~n   Error: ~p", [Error]),
            {error, {failed_reading, File, StartLine, EndLine, Error}};
        eof ->
            {ok, Res}
    end.


do_read(Io, Prompt, StartLine) ->
    case io:request(Io, {get_until,Prompt,erl_scan,tokens,[StartLine]}) of
        {ok, Toks, EndLine} ->
            case erl_parse:parse_term(Toks) of
                {ok, Term} ->
                    {ok, Term, EndLine};
                {error, {Line, erl_parse, Error}} ->
                    {error, Line, {parse_error, Error}}
            end;
        {error,E,EndLine} ->
            {error, EndLine, E};
        {eof, _EndLine} ->
            eof;
        Other ->
            Other
    end.


%%-----------------------------------------------------------------


check_mandatory(L, [{Key, Value}|T]) ->
    case lists:keymember(Key, 1, L) of
	true -> 
	    check_mandatory(L, T);
	false when Value == mandatory -> 
	    error({missing_mandatory, Key});
	false ->
	    {value, V} = Value,
	    check_mandatory([{Key, V} | L], T)
    end;
check_mandatory(L, []) -> 
    {ok, L}.


%% ---------

check_integer(I) -> check_integer(I, any).

check_integer(I, any) when is_integer(I) -> ok;
check_integer(I, pos) when is_integer(I), I > 0 -> ok;
check_integer(I, neg) when is_integer(I), I < 0 -> ok;
check_integer(I1, {gt,  I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 > I2) -> ok;
check_integer(I1, {gte, I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 >= I2) -> ok;
check_integer(I1, {lt,  I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 < I2) -> ok;
check_integer(I1, {lte, I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 =< I2) -> ok;
check_integer(I1, {eq,  I1}) 
  when is_integer(I1) -> ok;
check_integer(I,  {range, L, U}) 
  when (is_integer(I) andalso 
	is_integer(L) andalso 
	is_integer(U) andalso 
	(I >= L) andalso (I =< U)) -> ok;
check_integer(I, _)  -> error({invalid_integer, I}).

check_packet_size(S) ->
    case (catch check_integer(S, {range, 484, 2147483647})) of
	ok ->
	    ok;
	{error, _} ->
	    error({invalid_packet_size, S})
    end.

%% ---------

check_string(X) when is_list(X) -> ok;
check_string(X) -> error({invalid_string, X}).

check_string(X, any) 
  when is_list(X) -> ok;
check_string(X, {gt, Len})   
  when is_list(X) andalso (length(X) > Len) -> ok;
check_string(X, {gt, _Len})  
  when is_list(X) -> error({invalid_length, X});
check_string(X, {gte, Len})  
  when is_list(X) andalso (length(X) >= Len) -> ok;
check_string(X, {gte, _Len}) 
  when is_list(X) -> error({invalid_length, X});
check_string(X, {lt, Len})   
  when is_list(X) andalso (length(X) < Len) -> ok;
check_string(X, {lt, _Len})  
  when is_list(X) -> error({invalid_length, X});
check_string(X, {lte, Len})  
  when is_list(X) andalso (length(X) =< Len) -> ok;
check_string(X, {lte, _Len}) 
  when is_list(X) -> error({invalid_length, X});
check_string(X, Len)       
  when is_list(X) andalso is_integer(Len) andalso (length(X) =:= Len) -> ok;
check_string(X, _Len)      when is_list(X) -> error({invalid_length, X});
check_string(X, _Len)                   -> error({invalid_string, X}).


check_atom(X, Atoms) ->
    case lists:keysearch(X, 1, Atoms) of
        {value, {X, Val}} -> 
	    {ok, Val};
        _ -> 
	    error({invalid_atom, X, Atoms})
    end.


%% ---------

check_mp_model(MPModel) when is_atom(MPModel) ->
    All = [{v1,  ?MP_V1}, {v2c, ?MP_V2C}, {v3,  ?MP_V3}],
    check_atom(MPModel, All);
check_mp_model(?MP_V1) ->
    {ok, ?MP_V1};
check_mp_model(?MP_V2C) ->
    {ok, ?MP_V2C};
check_mp_model(?MP_V3) ->
    {ok, ?MP_V3};
check_mp_model(BadMpModel) ->
    error({invalid_mp_model, BadMpModel}).


%% ---------

check_sec_model(SecModel) when is_atom(SecModel) ->
    check_sec_model(SecModel, []);
check_sec_model(?SEC_ANY) ->
    {ok, ?SEC_ANY};
check_sec_model(?SEC_V1) ->
    {ok, ?SEC_V1};
check_sec_model(?SEC_V2C) ->
    {ok, ?SEC_V2C};
check_sec_model(?SEC_USM) ->
    {ok, ?SEC_USM};
check_sec_model(BadSecModel) ->
    error({invalid_sec_model, BadSecModel}).

check_sec_model(SecModel, Exclude) when is_atom(SecModel) ->
    All = [{any, ?SEC_ANY},
           {v1,  ?SEC_V1},
           {v2c, ?SEC_V2C},
           {usm, ?SEC_USM}],
    Alt = [{X, Y} || {X, Y} <- All, not lists:member(X, Exclude)],
    case (catch check_atom(SecModel, Alt) ) of
	{error, _} ->
	    error({invalid_sec_model, SecModel});
	OK ->
	    OK
    end;
check_sec_model(BadSecModel, _Exclude) ->
    error({invalid_sec_model, BadSecModel}).

check_sec_model(v1, v1, Exclude) ->
    check_sec_model2(v1, ?SEC_V1, Exclude);
check_sec_model(v1, SecModel, _Exclude) ->
    error({invalid_sec_model, v1, SecModel});
check_sec_model(v2c, v2c, Exclude) ->
    check_sec_model2(v2c, ?SEC_V2C, Exclude);
check_sec_model(v2c, SecModel, _Exclude) ->
    error({invalid_sec_model, v2c, SecModel});
check_sec_model(v3, usm, Exclude) ->
    check_sec_model2(v3, ?SEC_USM, Exclude);
check_sec_model(v3, SecModel, _Exclude) ->
    error({invalid_sec_model, v3, SecModel});
check_sec_model(M1, M2, _Exclude) ->
    error({invalid_sec_model, M1, M2}).

check_sec_model2(SecModel, SM, Exclude) ->
    case lists:member(SecModel, Exclude) of
	false ->
	    {ok, SM};
	true ->
	    error({invalid_sec_model, SecModel})
    end.


%% ---------

check_sec_level(SecLevel) when is_atom(SecLevel) ->
    All = [{noAuthNoPriv, ?'SnmpSecurityLevel_noAuthNoPriv'}, 
	   {authNoPriv,   ?'SnmpSecurityLevel_authNoPriv'}, 
	   {authPriv,     ?'SnmpSecurityLevel_authPriv'}], 
    case (catch check_atom(SecLevel, All)) of
	{error, _} ->
	    error({invalid_sec_level, SecLevel});
	OK ->
	    OK
    end;
check_sec_level(?'SnmpSecurityLevel_noAuthNoPriv' = SL) ->
    {ok, SL};
check_sec_level(?'SnmpSecurityLevel_authNoPriv' = SL) ->
    {ok, SL};
check_sec_level(?'SnmpSecurityLevel_authPriv' = SL) ->
    {ok, SL};
check_sec_level(BadSecLevel) ->
    error({invalid_sec_level, BadSecLevel}).


%% ---------

check_taddress(X) when is_list(X) andalso (length(X) =:= 6) ->
    case (catch all_integer(X)) of
	true  -> 
	    ok;
	false -> 
	    error({invalid_taddress, X})
    end;
check_taddress(X) ->
    error({invalid_taddress, X}).


%% ---------

check_timer(infinity) ->
    {ok, infinity};
check_timer(T) when is_record(T, snmp_incr_timer) ->
    {ok, T};
check_timer({WaitFor, Factor, Incr, Retry} = T) ->
    case (catch do_check_timer(WaitFor, Factor, Incr, Retry)) of
	ok ->
	    {ok, #snmp_incr_timer{wait_for    = WaitFor,
				  factor      = Factor,
				  incr        = Incr,
				  max_retries = Retry}};
	_Err ->
	    error({invalid_timer, T})
    end;
check_timer(Timeout) ->
    case (catch check_integer(Timeout, {gt, 0})) of
	ok ->
	    {ok, #snmp_incr_timer{wait_for    = Timeout,
				  factor      = 1,
				  incr        = 0,
				  max_retries = 0}};
	_Err ->
	    error({invalid_timer, Timeout})
    end.

do_check_timer(WaitFor, Factor, Incr, Retry) ->
    check_integer(WaitFor, {gt,  0}),
    check_integer(Factor,  {gt,  0}),
    check_integer(Incr,    {gte, 0}),
    check_integer(Retry,   {gte, 0}),
    ok.

%% ---------

check_ip(X) when is_list(X) andalso (length(X) =:= 4) ->
    case (catch all_integer(X)) of
	true  -> 
	    ok;
	false -> 
	    error({invalid_ip_address, X})
    end;
check_ip(X) ->
    error({invalid_ip_address, X}).


%% ---------

check_oid([E1,E2|_] = X) when E1 * 40 + E2 =< 255 ->
    case all_integer(X) of
	true -> 
	    ok;
	_ -> 
	    error({invalid_object_identifier, X})
    end;
check_oid(X) -> 
    error({invalid_object_identifier, X}).


%% ---------

all_integer([H|T]) when is_integer(H) -> all_integer(T);
all_integer([_H|_T]) -> false;
all_integer([]) -> true.


%% ---------

error(Reason) ->
    throw({error, Reason}).

