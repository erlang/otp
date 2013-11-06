%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
%% 

-module(snmp_log).


-export([
	 create/4, create/5, create/6, open/1, open/2, 
	 change_size/2, close/1, sync/1, info/1, 
	 log/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7,
	 log_to_io/4,  log_to_io/5,  log_to_io/6
	]).
-export([
	 upgrade/1, upgrade/2, 
	 downgrade/1
	]).
-export([
	 validate/1, validate/2
	]).


-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").

-define(VMODULE,"LOG").
-include("snmp_verbosity.hrl").

-define(LOG_FORMAT, internal).
-define(LOG_TYPE,   wrap).

-record(snmp_log, {id, seqno}).


%% --------------------------------------------------------------------
%% Exported functions
%% --------------------------------------------------------------------

upgrade(Log) when is_record(Log, snmp_log) ->
    Log;
upgrade(Log) ->
    upgrade(Log, disabled).

upgrade(Log, _SeqNoGen) when is_record(Log, snmp_log) ->
    Log;
upgrade(Log, {M, F, A} = SeqNoGen) 
  when (is_atom(M) andalso is_atom(F) andalso is_list(A)) ->
    #snmp_log{id = Log, seqno = SeqNoGen};
upgrade(Log, SeqNoGen) 
  when is_function(SeqNoGen, 0) ->
    #snmp_log{id = Log, seqno = SeqNoGen};
upgrade(Log, disabled = SeqNoGen) ->
    #snmp_log{id = Log, seqno = SeqNoGen}.

downgrade(#snmp_log{id = Log}) ->
    Log;
downgrade(Log) ->
    Log.


%% -- create ---

create(Name, File, Size, Repair) ->
    create(Name, File, disabled, Size, Repair, false).

create(Name, File, Size, Repair, Notify) 
  when (((Repair =:= true) orelse 
	 (Repair =:= false) orelse 
	 (Repair =:= truncate) orelse 
	 (Repair =:= snmp_repair)) andalso 
	((Notify =:= true) orelse
	 (Notify =:= false))) ->
    create(Name, File, disabled, Size, Repair, Notify);
create(Name, File, SeqNoGen, Size, Repair) ->
    create(Name, File, SeqNoGen, Size, Repair, false).

create(Name, File, SeqNoGen, Size, Repair, Notify) 
  when (((Repair =:= true) orelse 
	 (Repair =:= false) orelse 
	 (Repair =:= truncate) orelse 
	 (Repair =:= snmp_repair)) andalso 
	((Notify =:= true) orelse
	 (Notify =:= false))) ->
    ?vtrace("create -> entry with"
	    "~n   Name:     ~p"
	    "~n   File:     ~p"
	    "~n   SeqNoGen: ~p"
	    "~n   Size:     ~p"
	    "~n   Repair:   ~p"
	    "~n   Notify:   ~p", [Name, File, SeqNoGen, Size, Repair, Notify]),
    log_open(Name, File, SeqNoGen, Size, Repair, Notify);
create(Name, File, SeqNoGen, Size, Repair, Notify) ->
    {error, {bad_args, Name, File, SeqNoGen, Size, Repair, Notify}}.
    
    
%% -- open ---

%% Open an already existing ( = open ) log

open(Name) ->
    open(Name, #snmp_log{seqno = disabled}).
open(Name, #snmp_log{seqno = SeqNoGen} = _OldLog) ->
    %% We include mode in the opts just to be on the safe side
    case disk_log:open([{name, Name}, {mode, read_write}]) of
	{ok, Log} ->
	    %% SeqNo must be proprly initiated also
	    {ok, #snmp_log{id = Log, seqno = SeqNoGen}};
	{repaired, Log, _RecBytes, _BadBytes} ->
	    {ok, #snmp_log{id = Log, seqno = SeqNoGen}};
	ERROR ->
	    ERROR
    end.


%% -- close ---

close(#snmp_log{id = Log}) ->
    ?vtrace("close -> entry with"
	    "~n   Log: ~p", [Log]),
    do_close(Log);
close(Log) ->
    do_close(Log).

do_close(Log) ->
    disk_log:close(Log).


%% -- close ---

sync(#snmp_log{id = Log}) ->
    do_sync(Log);
sync(Log) ->
    do_sync(Log).

do_sync(Log) ->
    ?vtrace("sync -> entry with"
	    "~n   Log: ~p", [Log]),
    disk_log:sync(Log).


%% -- info ---

info(#snmp_log{id = Log}) ->
    do_info(Log);
info(Log) ->
    do_info(Log).

do_info(Log) ->
    case disk_log:info(Log) of
	Info when is_list(Info) ->
	    Items = [no_current_bytes, no_current_items, 
		     current_file, no_overflows],
	    info_filter(Items, Info, []);
	Else ->
	    Else
    end.
    
info_filter([], _Info, Acc) ->
    {ok, Acc};
info_filter([Item|Items], Info, Acc) ->
    case lists:keysearch(Item, 1, Info) of
	{value, New} ->
	    info_filter(Items, Info, [New|Acc]);
	false ->
	    info_filter(Items, Info, Acc)
    end.


%% -- validate --

%% This function is used to "validate" a log.
%% At present this means making sure all entries
%% are in the proper order, and if sequence numbering
%% is used that no entries are missing.
%% It is intended to be used for testing.

validate(Log) ->
    validate(Log, false).

validate(#snmp_log{id = Log}, SeqNoReq) ->
    validate(Log, SeqNoReq);
validate(Log, SeqNoReq) 
  when ((SeqNoReq =:= true) orelse (SeqNoReq =:= false)) ->
    Validator = 
	fun({Timestamp, SeqNo, _Packet, _Addr, _Port}, {PrevTS, PrevSN}) ->
		?vtrace("validating log entry when"
			"~n   Timestamp: ~p"
			"~n   SeqNo:     ~p"
			"~n   PrevTS:    ~p"
			"~n   PrevSN:    ~p", 
			[Timestamp, SeqNo, PrevTS, PrevSN]),
		validate_timestamp(PrevTS, Timestamp),
		validate_seqno(PrevSN, SeqNo),
		{Timestamp, SeqNo};

	   ({Timestamp, _Packet, _Addr, _Port}, {PrevTS, _PrevSN}) when SeqNoReq =:= true -> 
		?vtrace("validating log entry when"
			"~n   Timestamp: ~p"
			"~n   PrevTS:    ~p", 
			[Timestamp, PrevTS]),
		throw({error, {missing_seqno, Timestamp}});

	   ({Timestamp, _Packet, _Addr, _Port}, {PrevTS, PrevSN}) -> 
		?vtrace("validating log entry when"
			"~n   Timestamp: ~p"
			"~n   PrevTS:    ~p", 
			[Timestamp, PrevTS]),
		validate_timestamp(PrevTS, Timestamp),
		{Timestamp, PrevSN};

	   (E, Acc) ->
		?vtrace("validating bad log entry when"
			"~n   E:   ~p"
			"~n   Acc: ~p", 
			[E, Acc]),
		throw({error, {bad_entry, E, Acc}})
	end,
    try
	begin
	    validate_loop(disk_log:chunk(Log, start), 
			  Log, Validator, first, first)
	end
    catch 
	throw:Error ->
	    Error
    end.
    
%% We shall check that TS2 >= TS1
validate_timestamp(first, _TS2) ->
    ok;
validate_timestamp({LT1, UT1} = TS1, {LT2, UT2} = TS2) ->
    LT1_Secs = calendar:datetime_to_gregorian_seconds(LT1),
    UT1_Secs = calendar:datetime_to_gregorian_seconds(UT1),
    LT2_Secs = calendar:datetime_to_gregorian_seconds(LT2),
    UT2_Secs = calendar:datetime_to_gregorian_seconds(UT2),
    case ((LT2_Secs >= LT1_Secs) andalso (UT2_Secs >= UT1_Secs)) of
	true ->
	    ok;
	false ->
	    throw({error, {invalid_timestamp, TS1, TS2}})
    end;
validate_timestamp(TS1, TS2) ->
    throw({error, {bad_timestamp, TS1, TS2}}).


%% The usual case when SN2 = SN1 + 1
validate_seqno(first, SN2) 
  when is_integer(SN2) >= 1 ->
    ok;

%% The usual case when SN2 = SN1 + 1
validate_seqno(SN1, SN2) 
  when is_integer(SN1) andalso is_integer(SN2) andalso 
       (SN2 =:= (SN1 + 1)) andalso (SN1 >= 1) ->
    ok;

%% The case when we have a wrap
validate_seqno(SN1, SN2) 
  when is_integer(SN1) andalso is_integer(SN2) andalso 
       (SN2 < SN1) andalso (SN2 >= 1) ->
    ok;

%% And everything else must be an error...
validate_seqno(SN1, SN2) -> 
    throw({error, {bad_seqno, SN1, SN2}}).

validate_loop(eof, _Log, _Validatior, _PrevTS, _PrevSN) ->
    ok;
validate_loop({error, _} = Error, _Log, _Validator, _PrevTS, _PrevSN) ->
    Error;
validate_loop({Cont, Terms}, Log, Validator, PrevTS, PrevSN) ->
    ?vtrace("validate_loop -> entry with"
	    "~n   Terms:  ~p"
	    "~n   PrevTS: ~p"
	    "~n   PrevSN: ~p", [Terms, PrevTS, PrevSN]),
    {NextTS, NextSN} = lists:foldl(Validator, {PrevTS, PrevSN}, Terms), 
    ?vtrace("validate_loop -> "
	    "~n   NextTS: ~p"
	    "~n   NextSN: ~p", [NextTS, NextSN]),
    validate_loop(disk_log:chunk(Log, Cont), Log, Validator, NextTS, NextSN);
validate_loop({Cont, Terms, BadBytes}, Log, Validator, PrevTS, PrevSN) ->
    ?vtrace("validate_loop -> entry with"
	    "~n   Terms:    ~p"
	    "~n   BadBytes: ~p"
	    "~n   PrevTS:   ~p"
	    "~n   PrevSN:   ~p", [Terms, BadBytes, PrevTS, PrevSN]),
    error_logger:error_msg("Skipping ~w bytes while validating ~p~n~n", 
			   [BadBytes, Log]),
    {NextTS, NextSN} = lists:foldl(Validator, {PrevTS, PrevSN}, Terms), 
    ?vtrace("validate_loop -> "
	    "~n   NextTS: ~p"
	    "~n   NextSN: ~p", [NextTS, NextSN]),
    validate_loop(disk_log:chunk(Log, Cont), Log, Validator, NextTS, NextSN);
validate_loop(Error, _Log, _Write, _PrevTS, _PrevSN) ->
    Error.
    

%% -- log ---

%%-----------------------------------------------------------------
%% For efficiency reasons, we want to log the packet as a binary.
%% This is only possible for messages that are not encrypted.
%% Therefore, Packet can be either a binary (encoded message), or
%% a tuple {V3Hdr, ScopedPduBytes}
%% 
%% log(Log, Packet, Addr, Port)
%%-----------------------------------------------------------------


log(#snmp_log{id = Log, seqno = SeqNo}, Packet, Addr, Port) ->
    ?vtrace("log -> entry with"
	    "~n   Log:  ~p"
	    "~n   Addr: ~p"
	    "~n   Port: ~p", [Log, Addr, Port]),
    Entry = make_entry(SeqNo, Packet, Addr, Port), 
%%     io:format("log -> "
%% 	      "~n   Entry: ~p"
%% 	      "~n   Info:  ~p"
%% 	      "~n", [Entry, disk_log:info(Log)]),
    Res = disk_log:alog(Log, Entry),
%%     io:format("log -> "
%% 	      "~n   Res:  ~p"
%% 	      "~n   Info: ~p"
%% 	      "~n", [Res, disk_log:info(Log)]),
    %% disk_log:sync(Log), 
    Res.
   


make_entry(SeqNoGen, Packet, Addr, Port) ->
    try next_seqno(SeqNoGen) of
	disabled ->
	    {timestamp(), Packet, Addr, Port};
	{ok, NextSeqNo} ->
	    {timestamp(), NextSeqNo, Packet, Addr, Port}
    catch
	_:_ ->
	    {timestamp(), Packet, Addr, Port}
    end.

next_seqno({M, F, A}) ->
    {ok, apply(M, F, A)};
next_seqno(F) when is_function(F) ->
    {ok, F()};
next_seqno(_) ->
    disabled.


%% -- change_size ---

change_size(#snmp_log{id = Log}, NewSize) ->
    do_change_size(Log, NewSize);
change_size(Log, NewSize) ->
    do_change_size(Log, NewSize).

do_change_size(Log, NewSize) ->
    ?vtrace("change_size -> entry with"
	    "~n   Log:     ~p"
	    "~n   NewSize: ~p", [Log, NewSize]),
    disk_log:change_size(Log, NewSize).


%% -- log_to_txt ---

log_to_txt(Log, FileName, Dir, Mibs, TextFile) ->
    log_to_txt(Log, FileName, Dir, Mibs, TextFile, null, null).

log_to_txt(Log, FileName, Dir, Mibs, TextFile, Start) ->
    log_to_txt(Log, FileName, Dir, Mibs, TextFile, Start, null).

log_to_txt(Log, FileName, Dir, Mibs, TextFile, Start, Stop) 
  when is_list(Mibs) andalso is_list(TextFile) ->
    ?vtrace("log_to_txt -> entry with"
	    "~n   Log:      ~p"
	    "~n   FileName: ~p"
	    "~n   Dir:      ~p"
	    "~n   Mibs:     ~p"
	    "~n   TextFile: ~p"
	    "~n   Start:    ~p"
	    "~n   Stop:     ~p", 
	    [Log, FileName, Dir, Mibs, TextFile, Start, Stop]),
    File = filename:join(Dir, FileName),
    Converter = fun(L) ->
			?vtrace("log_to_txt:fun -> entry with"
				"~n   L: ~p", [L]),
			do_log_to_file(L, TextFile, Mibs, Start, Stop)
		end,
    log_convert(Log, File, Converter).


%% -- log_to_io ---

log_to_io(Log, FileName, Dir, Mibs) ->
    log_to_io(Log, FileName, Dir, Mibs, null, null).

log_to_io(Log, FileName, Dir, Mibs, Start) ->
    log_to_io(Log, FileName, Dir, Mibs, Start, null).

log_to_io(Log, FileName, Dir, Mibs, Start, Stop) 
  when is_list(Mibs) ->
    ?vtrace("log_to_io -> entry with"
	    "~n   Log:      ~p"
	    "~n   FileName: ~p"
	    "~n   Dir:      ~p"
	    "~n   Mibs:     ~p"
	    "~n   Start:    ~p"
	    "~n   Stop:     ~p", 
	    [Log, FileName, Dir, Mibs, Start, Stop]),
    File = filename:join(Dir, FileName),
    Converter = fun(L) ->
			?vtrace("log_to_io:fun -> entry with"
				"~n   L: ~p", [L]),
			do_log_to_io(L, Mibs, Start, Stop)
		end,
    log_convert(Log, File, Converter).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% -- log_convert ---

log_convert(#snmp_log{id = Log}, File, Converter) ->
    do_log_convert(Log, File, Converter);
log_convert(Log, File, Converter) ->
    do_log_convert(Log, File, Converter).

do_log_convert(Log, File, Converter) ->
    ?vtrace("do_log_converter -> entry with"
    	    "~n   Log:  ~p"
    	    "~n   File: ~p", [Log, File]),
    Verbosity = get(verbosity), 
    {Pid, Ref} = 
	erlang:spawn_monitor(
	  fun() ->
		  put(sname, "LOG-CONVERTER"), 
		  put(verbosity, Verbosity), 
		  erlang:process_flag(trap_exit, true), 
		  ?vlog("begin converting", []),
		  Result = do_log_convert2(Log, File, Converter),
		  ?vlog("convert result: ~p", [Result]),
		  exit(Result)
	  end),
    receive 
	{'DOWN', Ref, process, Pid, Result} ->
	    ?vtrace("do_log_converter -> received result"
	    	    "~n   Result: ~p", [Result]),
	    Result
    end.
    
do_log_convert2(Log, File, Converter) ->
    %% First check if the caller process has already opened the
    %% log, because if we close an already open log we will cause
    %% a runtime error.
    ?vtrace("do_log_convert2 -> entry - check if owner", []),
    case is_owner(Log) of
	true ->
	    ?vtrace("do_log_convert2 -> owner - now convert", []),
	    Converter(Log);
	false ->
	    %% Not yet member of the ruling party, apply for membership...
	    ?vtrace("do_log_convert2 -> not owner - open", []),
	    case log_open(Log, File) of
		{ok, _} ->
		    ?vdebug("do_log_convert2 -> opened - now convert", []),
		    Res = Converter(Log),
		    ?vtrace("do_log_convert2 -> converted - now close", []),
		    disk_log:close(Log),
		    ?vtrace("do_log_convert2 -> closed - done", []),
		    Res;
		{error, {name_already_open, _}} ->
		    ?vdebug("do_log_convert2 -> "
			    "already opened - now convert", []),
                    Res = Converter(Log),
		    ?vtrace("do_log_convert2 -> converted - done", []),
		    Res;
                {error, Reason} ->
		    ?vinfo("do_log_convert2 -> failed open: "
			    "~n   Reason: ~p", [Reason]),
                    {error, {Log, Reason}}
	    end
    end.


%% -- do_log_to_text ---

do_log_to_file(Log, TextFile, Mibs, Start, Stop) ->
    ?vtrace("do_log_to_txt -> entry with"
	    "~n   Log:      ~p"
	    "~n   TextFile: ~p"
	    "~n   Start:    ~p"
	    "~n   Stop:     ~p", [Log, TextFile, Start, Stop]),
    case file:open(TextFile, [write]) of
        {ok, Fd} ->
	    ?vtrace("do_log_to_txt -> outfile created - create mini MIB", []),
            MiniMib = snmp_mini_mib:create(Mibs),
	    ?vtrace("do_log_to_txt -> mini-MIB created - begin conversion", []),
	    Write = fun(X) -> 
			    ?vtrace("do_log_to_txt:fun -> "
				    "entry - try format", []),
			    case format_msg(X, MiniMib, Start, Stop) of
				{ok, S} ->
				    ?vtrace("do_log_to_txt:fun -> "
					    "formated - now write", []),
				    io:format(Fd, "~s", [S]);
				_ ->
				    ?vdebug("do_log_to_txt:fun -> "
					    "format failed", []),
				    ok
			    end
		    end,
            Res = (catch loop(disk_log:chunk(Log, start), Log, Write)),
	    ?vtrace("do_log_to_txt -> converted - now delete mini-MIB", []),
	    snmp_mini_mib:delete(MiniMib), 
	    ?vtrace("do_log_to_txt -> "
		    "mini-MIB closed - now close output file", []),
            file:close(Fd),
	    ?vtrace("do_log_to_txt -> done", []),
            Res;
        {error, Reason} ->
	    ?vinfo("failed opening output file: "
		   "~n   TestFile: ~p"
		   "~n   Reason:   ~p", [TextFile, Reason]),
            {error, {TextFile, Reason}}
    end.


do_log_to_io(Log, Mibs, Start, Stop) ->
    ?vtrace("do_log_to_io -> entry with"
	    "~n   Log:   ~p"
	    "~n   Mibs:  ~p"
	    "~n   Start: ~p"
	    "~n   Stop:  ~p", [Log, Mibs, Start, Stop]),
    MiniMib = snmp_mini_mib:create(Mibs),
    ?vtrace("do_log_to_io -> mini-MIB created - begin conversion", []),
    Write = fun(X) -> 
		    ?vtrace("do_log_to_io:fun -> entry", []),
		    case format_msg(X, MiniMib, Start, Stop) of
			{ok, S} ->
			    ?vtrace("do_log_to_io:fun -> "
				    "formated - now write", []),
			    io:format("~s", [S]);
			_ ->
			    ?vdebug("do_log_to_io:fun -> "
				    "format failed", []),
			    ok
		    end
	    end,
    (catch loop(disk_log:chunk(Log, start), Log, Write)),
    ?vtrace("do_log_to_io -> converted - now delete mini-MIB", []),
    snmp_mini_mib:delete(MiniMib),
    ?vtrace("do_log_to_io -> done", []),
    ok.


loop(eof, _Log, _Write) ->
    ?vtrace("loop -> entry when eof", []),
    ok;
loop({error, _Reason} = Error, _Log, _Write) ->
    ?vtrace("loop -> entry with error"
	    "~n   Reason: ~p", [_Reason]),
    Error;
loop({Cont, Terms}, Log, Write) ->
    ?vtrace("loop -> entry with terms"
	    "~n   Cont:          ~p"
	    "~n   length(Terms): ~p", [Cont, length(Terms)]), 
   case (catch lists:foreach(Write, Terms)) of
	{'EXIT', Reason} ->
	    ?vtrace("loop -> failure while writing terms"
		    "~n   Reason: ~p", [Reason]),
	    {error, Reason};
	_X ->
	   ?vtrace("loop -> terms written"
		   "~n   X: ~p", [_X]),
	    loop(disk_log:chunk(Log, Cont), Log, Write)
    end;
loop({Cont, Terms, BadBytes}, Log, Write) ->
    error_logger:error_msg("Skipping ~w bytes while converting ~p~n~n", 
			   [BadBytes, Log]),
    case (catch lists:foreach(Write, Terms)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	_ ->
	    loop(disk_log:chunk(Log, Cont), Log, Write)
    end;
loop(Error, _Log, _Write) ->
    ?vtrace("loop -> entry with unknown"
	    "~n   Error: ~p", [Error]),
    Error.


format_msg(Entry, Mib, Start, Stop) ->
    TimeStamp = element(1, Entry),
    case timestamp_filter(TimeStamp, Start, Stop) of
        true ->
	    do_format_msg(Entry, Mib);
	false ->
	    ignore
    end.

%% This is an old-style entry, that never had the sequence-number
do_format_msg({Timestamp, Packet, {Addr, Port}}, Mib) ->
    ?vdebug("do_format_msg -> old style log entry", []),
    do_format_msg(Timestamp, Packet, Addr, Port, Mib);

%% This is the format without sequence-number
do_format_msg({Timestamp, Packet, Addr, Port}, Mib) ->
    ?vdebug("do_format_msg -> log entry without seqno", []),
    do_format_msg(Timestamp, Packet, Addr, Port, Mib);

%% This is the format with sequence-number
do_format_msg({Timestamp, SeqNo, Packet, Addr, Port}, Mib) ->
    ?vdebug("do_format_msg -> log entry with seqno", []),
    do_format_msg(Timestamp, SeqNo, Packet, Addr, Port, Mib);

%% This is crap...
do_format_msg(_, _) ->
    format_tab("** unknown entry in log file\n\n", []).

do_format_msg(TimeStamp, {V3Hdr, ScopedPdu}, Addr, Port, Mib) ->
    ?vtrace("do_format_msg -> entry with"
	    "~n   Timestamp: ~p"
	    "~n   Addr:      ~p"
	    "~n   Port:      ~p"
	    "~n   => Try decode scoped pdu", 
	    [TimeStamp, Addr, Port]),
    case (catch snmp_pdus:dec_scoped_pdu(ScopedPdu)) of
	ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
	    ?vtrace("do_format_msg -> scoped pdu decoded"
		    "~n   ScopedPDU: ~p", [ScopedPDU]),
	    Msg = #message{version = 'version-3',
			   vsn_hdr = V3Hdr,
			   data    = ScopedPDU},
	    f(ts2str(TimeStamp), "", Msg, Addr, Port, Mib);

	{'EXIT', Reason} ->
	    ?vinfo("Failed decoding scoped pdu: "
		   "~n   V3Hdr:     ~w"
		   "~n   ScopedPdu: ~w"
		   "~n   Reason:    ~p", [V3Hdr, ScopedPdu, Reason]),
	    format_tab("** error in log file at ~s from ~s:~w ~p\n\n", 
		       [ts2str(TimeStamp), ip(Addr), Port, Reason])
    end;

do_format_msg(TimeStamp, Packet, Addr, Port, Mib) ->
    ?vtrace("do_format_msg -> entry with"
	    "~n   Timestamp: ~p"
	    "~n   Addr:      ~p"
	    "~n   Port:      ~p"
	    "~n   => Try decode packet", 
	    [TimeStamp, Addr, Port]),
    case (catch snmp_pdus:dec_message(binary_to_list(Packet))) of
	#message{data = Data} = Msg when (is_record(Data, scopedPdu) orelse 
					  is_record(Data, pdu) orelse 
					  is_record(Data, trappdu)) ->
	    ?vtrace("do_format_msg -> packet decoded"
		    "~n   Msg: ~p", [Msg]),
	    f(ts2str(TimeStamp), "", Msg, Addr, Port, Mib);

	#message{version = Vsn, 
		 vsn_hdr = VsnHdr} = Msg ->
	    ?vinfo("Message not fully decoded: "
		   "~n   Msg: ~p", [Msg]),
	    Reason = 
		lists:flatten(
		  io_lib:format("Message not fully decoded: "
				"Vsn = ~p, VsnHdr = ~w", [Vsn, VsnHdr])),
	    format_tab("** error in log file ~s from ~s:~w => "
		       "\n   ~s\n\n", 
		       [ts2str(TimeStamp), ip(Addr), Port, Reason]);

	{'EXIT', Reason} ->
	    ?vinfo("Failed decoding packet: "
		   "~n   Packet: ~w"
		   "~n   Reason: ~p", [Packet, Reason]),
	    format_tab("** error in log file ~p\n\n", [Reason])
    end.
    
do_format_msg(TimeStamp, SeqNo, {V3Hdr, ScopedPdu}, Addr, Port, Mib) ->
    ?vtrace("do_format_msg -> entry with"
	    "~n   Timestamp: ~p"
	    "~n   SeqNo:     ~p"
	    "~n   Addr:      ~p"
	    "~n   Port:      ~p"
	    "~n   => Try decode scoped pdu", 
	    [TimeStamp, SeqNo, Addr, Port]),
    case (catch snmp_pdus:dec_scoped_pdu(ScopedPdu)) of
	ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
	    ?vtrace("do_format_msg -> scoped pdu decoded"
		    "~n   ScopedPDU: ~p", [ScopedPDU]),
	    Msg = #message{version = 'version-3',
			   vsn_hdr = V3Hdr,
			   data    = ScopedPDU},
	    f(ts2str(TimeStamp), sn2str(SeqNo), Msg, Addr, Port, Mib);

	{'EXIT', Reason} ->
	    ?vinfo("Failed decoding scoped pdu: "
		   "~n   V3Hdr:     ~w"
		   "~n   ScopedPdu: ~w"
		   "~n   Reason:    ~p", [V3Hdr, ScopedPdu, Reason]),
	    format_tab("** error in log file at ~s~s from ~s:~w ~p\n\n", 
		       [ts2str(TimeStamp), sn2str(SeqNo), 
			ip(Addr), Port, Reason])
    end;
do_format_msg(TimeStamp, SeqNo, Packet, Addr, Port, Mib) ->
    ?vtrace("do_format_msg -> entry with"
	    "~n   Timestamp: ~p"
	    "~n   SeqNo:     ~p"
	    "~n   Addr:      ~p"
	    "~n   Port:      ~p"
	    "~n   => Try decode message", 
	    [TimeStamp, SeqNo, Addr, Port]),
    case (catch snmp_pdus:dec_message(binary_to_list(Packet))) of
	#message{data = Data} = Msg when (is_record(Data, scopedPdu) orelse 
					  is_record(Data, pdu) orelse 
					  is_record(Data, trappdu)) ->
	    ?vtrace("do_format_msg -> message decoded"
		    "~n   Msg: ~p", [Msg]),
	    f(ts2str(TimeStamp), sn2str(SeqNo), Msg, Addr, Port, Mib);

	#message{version = Vsn, 
		 vsn_hdr = VsnHdr} = Msg ->
	    ?vinfo("Message not fully decoded: "
		   "~n   Msg: ~p", [Msg]),
	    Reason = 
		lists:flatten(
		  io_lib:format("Message not fully decoded: "
				"Vsn = ~p, VsnHdr = ~w", [Vsn, VsnHdr])),
	    format_tab("** error in log file ~s~s from ~s:~w => "
		       "\n   ~s\n\n", 
		       [ts2str(TimeStamp), sn2str(SeqNo), 
			ip(Addr), Port, Reason]);

	{'EXIT', Reason} ->
	    ?vinfo("Failed decoding packet: "
		   "~n   Packet: ~w"
		   "~n   Reason: ~p", [Packet, Reason]),
	    format_tab("** error in log file ~s (~s) from ~s:~w ~p\n\n", 
		       [ts2str(TimeStamp), sn2str(SeqNo), 
			ip(Addr), Port, Reason])
    end.
    
    
f(TimeStamp, SeqNo, 
  #message{version = Vsn, vsn_hdr = VsnHdr, data = Data}, 
  Addr, Port, Mib) ->
    try
	begin
	    Str    = format_pdu(Data, Mib),
	    HdrStr = format_header(Vsn, VsnHdr),
	    case get_type(Data) of
		trappdu ->
		    f_trap(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port);
		'snmpv2-trap' ->
		    f_trap(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port);
		'inform-request' ->
		    f_inform(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port);
		'get-response' ->
		    f_response(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port);
		report ->
		    f_report(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port);
		_ ->
		    f_request(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port)
	    end
	end
    catch
	T:E ->
	    ?vinfo("Failed formating log entry"
		   "~n   TimeStamp:  ~p"
		   "~n   SeqNo:      ~p"
		   "~n   Data:       ~p"
		   "~n   Vsn:        ~p"
		   "~n   VsnHdr:     ~p"
		   "~n   Addr:       ~p"
		   "~n   Port:       ~p"
		   "~n   Error Type: ~w"
		   "~n   Error:      ~p", 
		   [TimeStamp, SeqNo, Data, Vsn, VsnHdr, Addr, Port, T, E]),
	    format_tab("** error while formating log entry ~p\n\n", [{T, E}])
    end.

f_request(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("request ~s:~w - ~s [~s]~s ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, SeqNo, Vsn, Str]).

f_response(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("response ~s:~w - ~s [~s]~s ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, SeqNo, Vsn, Str]).

f_report(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("report ~s:~w - ~s [~s]~s ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, SeqNo, Vsn, Str]).

f_trap(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("trap ~s:~w - ~s [~s]~s ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, SeqNo, Vsn, Str]).

f_inform(TimeStamp, SeqNo, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("inform ~s:~w - ~s [~s]~s ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, SeqNo, Vsn, Str]).


%% Convert a timestamp 2-tupple to a printable string
%%
ts2str({Local,Universal}) ->
    lists:flatten(dat2str(Local) ++ " , " ++ dat2str(Universal));
ts2str(_) ->
    "".

%% Convert a sequence number integer to a printable string
%%
sn2str(SeqNo) when is_integer(SeqNo) ->
    " [" ++ integer_to_list(SeqNo) ++ "]";
sn2str(_) ->
    "".

%% Convert a datetime 2-tupple to a printable string
%%
dat2str({{Y,M,D},{H,Min,S}}) -> 
    io_lib:format("~w-~w-~w,~w:~w:~w",[Y,M,D,H,Min,S]).


timestamp_filter({Local,Universal},Start,Stop) ->
    tsf_ge(Local,Universal,Start) and tsf_le(Local,Universal,Stop);
timestamp_filter(_,_Start,_Stop) -> 
    true.

tsf_ge(_Local,_Universal,null) ->
    true;
tsf_ge(Local,_Universal,{local_time,DateTime}) ->
    tsf_ge(Local,DateTime);
tsf_ge(_Local,Universal,{universal_time,DateTime}) ->
    tsf_ge(Universal,DateTime);
tsf_ge(Local,_Universal,DateTime) ->
    tsf_ge(Local,DateTime).

tsf_ge(TimeStamp, DateTime) -> 
    T1 = calendar:datetime_to_gregorian_seconds(TimeStamp),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime),
    T1 >= T2.

tsf_le(_Local, _Universal, null) ->
    true;
tsf_le(Local, _Universal, {local_time, DateTime}) ->
    tsf_le(Local, DateTime);
tsf_le(_Local, Universal, {universal_time, DateTime}) ->
    tsf_le(Universal, DateTime);
tsf_le(Local, _Universal, DateTime) ->
    tsf_le(Local,DateTime).

tsf_le(TimeStamp, DateTime) ->
    T1 = calendar:datetime_to_gregorian_seconds(TimeStamp),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime),
    T1 =< T2.


%% In the output replace TAB by ESC TAB, and add a single trailing TAB.
%%
format_tab(Format, Args) ->
    Str  = lists:flatten(io_lib:format(Format, Args)),
    DStr = lists:map(fun($\t) -> "\e\t"; (C) -> C end, Str),
    {ok, io_lib:format("~s\t", [DStr])}.


format_header('version-1', CommunityStr) ->
    CommunityStr;
format_header('version-2', CommunityStr) ->
    CommunityStr;
format_header('version-3', #v3_hdr{msgFlags              = MsgFlags,
                                   msgSecurityModel      = SecModel,
                                   msgSecurityParameters = SecParams}) ->
    SecLevel = snmp_misc:get_sec_level(MsgFlags),
    case SecModel of
        ?SEC_USM ->
            case catch snmp_pdus:dec_usm_security_parameters(SecParams) of
                #usmSecurityParameters{msgAuthoritativeEngineID = AuthEngineID,
                                       msgUserName              = UserName} ->
                    io_lib:format("~w:\"~s\":\"~s\"",
                                  [SecLevel, AuthEngineID, UserName]);
                _ ->
                    "-"
            end;
        _ ->
            "\"unknown security model\""
    end.


format_pdu(#scopedPdu{contextName = Context, data = Pdu}, Mib) ->
    io_lib:format("Context: \"~s\"\n~s",
                  [Context, snmp_misc:format_pdu(Pdu, Mib)]);
format_pdu(Pdu, Mib) ->
    snmp_misc:format_pdu(Pdu, Mib).

get_type(#scopedPdu{data = Pdu}) ->
    get_type(Pdu);
get_type(Pdu) when is_record(Pdu, trappdu) ->
    trappdu;
get_type(#pdu{type = Type}) ->
    Type.


ip({A,B,C,D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A,B,C,D])).



%% -------------------------------------------------------------------    
%% Various utility functions
%% -------------------------------------------------------------------    

log_open(Name, File, {M, F, A} = SeqNoGen, Size, Repair, Notify) 
  when (is_atom(M) andalso is_atom(F) andalso is_list(A)) ->
    log_open2(Name, File, SeqNoGen, Size, Repair, Notify);
log_open(Name, File, SeqNoGen, Size, Repair, Notify) 
  when is_function(SeqNoGen, 0) ->
    log_open2(Name, File, SeqNoGen, Size, Repair, Notify);
log_open(Name, File, disabled = SeqNoGen, Size, Repair, Notify) ->
    log_open2(Name, File, SeqNoGen, Size, Repair, Notify);
log_open(_, _File, BadSeqNoGen, _Size, _Repair, _Notify) ->
    {error, {bad_seqno, BadSeqNoGen}}.

log_open2(Name, File, SeqNoGen, Size, Repair, Notify) ->
    case do_log_open(Name, File, Size, Repair, Notify) of
	{ok, Log} ->
	    {ok, #snmp_log{id = Log, seqno = SeqNoGen}};
	{repaired, Log, Rec, Bad} ->
	    ?vlog("log_open -> repaired: "
		  "~n   Rec: ~p"
		  "~n   Bad: ~p", [Rec, Bad]),
	    {ok, #snmp_log{id = Log, seqno = SeqNoGen}};
	Error ->
	    Error
    end.


%% We need to make sure we do not end up in an infinit loop
%% Take the number of files of the wrap log and add 2 (for
%% the index and size files).
do_log_open(Name, File, {_, N} = Size, snmp_repair = _Repair, Notify) ->
    do_snmp_log_open(Name, File, Size, N+2, Notify);

do_log_open(Name, File, Size, snmp_repair = _Repair, Notify) ->
    do_snmp_log_open(Name, File, Size, 1, Notify);

do_log_open(Name, File, Size, Repair, Notify) ->
    do_std_log_open(Name, File, Size, Repair, Notify).


do_snmp_log_open(Name, File, Size, N, Notify) when N =< 0 ->
    do_std_log_open(Name, File, Size, true, Notify);
do_snmp_log_open(Name, File, Size, N, Notify) ->
    case do_std_log_open(Name, File, Size, true, Notify) of
	{error, {not_a_log_file, XFile}} ->
	    case file:rename(XFile, lists:append([XFile, ".MOVED"])) of
		ok ->
		    ?vinfo("Failed open log file (even with repair) - "
			   "not a logfile:"
			   "~n   Attempting to move file aside (.MOVED)"
			   "~n   ~s", [XFile]),
		    do_snmp_log_open(Name, File, Size, N-1, Notify);
		Error ->
		    {error, {rename_failed, Error}}
	    end;
	{error, Reason} ->
	    ?vinfo("Failed open log file (even with repair) - "
		   "~n   Attempting to move old log file aside (.MOVED)"
		   "~n~p", [Reason]),
	    move_log(File),
	    do_std_log_open(Name, File, Size, true, Notify);
	Else ->
	    Else
    end.


%% First try to open the log without the size-spec.  This will 
%% succeed if the log has already been created.  In that case, 
%% we'll use whatever size the log had at the time it was closed.
do_std_log_open(Name, File, Size, Repair, Notify) ->
    Opts = [{name,   Name},
	    {file,   File},
	    {type,   ?LOG_TYPE},
	    {format, ?LOG_FORMAT},
	    {mode,   read_write},
	    {notify, Notify}, 
	    {repair, Repair}],
    case disk_log:open(Opts) of
	{error, {badarg, size}} ->
	    %% The log didn't exist, try with the size-spec
            disk_log:open([{size, Size} | Opts]);
	Else ->
	    Else
    end.


log_open(Name, File) ->
    Opts = [{name, Name}, 
	    {file, File}],
    case disk_log:open(Opts) of
	{error, {badarg, size}} ->
	    {error, no_such_log};
	Else ->
	    Else
    end.


move_log(File) ->
    Dir      = filename:dirname(File),
    FileName = filename:basename(File),
    case file:list_dir(Dir) of
        {ok, Files0} ->
	    Files = [F || F <- Files0, lists:prefix(FileName, F)],
	    F = fun(XFile) ->
			file:rename(XFile, lists:append([XFile, ".MOVED"]))
		end,
            lists:foreach(F, Files);
        _ ->
            ok
    end.
    

is_owner(Log) ->    
    lists:member(self(), log_owners(Log)).

log_owners(Log) ->
    Info = log_info(Log),
    case lists:keysearch(owners, 1, Info) of
	{value, {_, Pids}} ->
	    [P || {P, _} <- Pids];
	_ ->
	    []
    end.

log_info(Log) ->
    case disk_log:info(Log) of
	Info when is_list(Info) ->
	    Info;
	_ ->
	    []
    end.


timestamp() ->     
    {calendar:local_time(), calendar:universal_time()}.

