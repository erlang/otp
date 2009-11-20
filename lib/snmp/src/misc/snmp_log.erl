%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

-module(snmp_log).


-export([
	 create/4, create/5, 
	 change_size/2, close/1, sync/1, info/1, 
	 log/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7,
	 log_to_io/4,  log_to_io/5,  log_to_io/6
	]).


-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").

-define(VMODULE,"LOG").
-include("snmp_verbosity.hrl").

-define(LOG_FORMAT, internal).
-define(LOG_TYPE,   wrap).


%% --------------------------------------------------------------------
%% Exported functions
%% --------------------------------------------------------------------


%% -- create ---

create(Name, File, Size, Repair) ->
    create(Name, File, Size, Repair, false).

create(Name, File, Size, Repair, Notify) ->
    ?vtrace("create -> entry with"
	    "~n   Name:   ~p"
	    "~n   File:   ~p"
	    "~n   Size:   ~p"
	    "~n   Repair: ~p"
	    "~n   Notify: ~p", [Name, File, Size, Repair, Notify]),
    log_open(Name, File, Size, Repair, Notify).
    

%% -- close ---

close(Log) ->
    ?vtrace("close -> entry with"
	    "~n   Log: ~p", [Log]),
    disk_log:close(Log).


%% -- close ---

sync(Log) ->
    ?vtrace("sync -> entry with"
	    "~n   Log: ~p", [Log]),
    disk_log:sync(Log).

%% -- info ---

info(Log) ->
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


%% -- log ---

%%-----------------------------------------------------------------
%% For efficiency reasons, we want to log the packet as a binary.
%% This is only possible for messages that are not encrypted.
%% Therefore, Packet can be either a binary (encoded message), or
%% a tuple {V3Hdr, ScopedPduBytes}
%% 
%% log(Log, Packet, Addr, Port)
%%-----------------------------------------------------------------


log(Log, Packet, Addr, Port) ->
    ?vtrace("log -> entry with"
	    "~n   Log:  ~p"
	    "~n   Addr: ~p"
	    "~n   Port: ~p", [Log, Addr, Port]),
    Entry = {timestamp(), Packet, Addr, Port},
    disk_log:alog(Log, Entry).



%% -- change_size ---

change_size(Log, NewSize) ->
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
    File = filename:join(Dir, FileName),
    Converter = fun(L) ->
			do_log_to_io(L, Mibs, Start, Stop)
		end,
    log_convert(Log, File, Converter).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------


%% -- log_convert ---

log_convert(Log, File, Converter) ->
    %% First check if the caller process has already opened the
    %% log, because if we close an already open log we will cause
    %% a runtime error.
    case is_owner(Log) of
	true ->
	    Converter(Log);
	false ->
	    %% Not yet member of the ruling party, apply for membership...
	    %% If a log is opened as read_write it is not possible to 
	    %% open it as read_only. So, to get around this we open 
	    %% it under a different name...
	    Log2 = convert_name(Log),
	    case log_open(Log2, File) of
		{ok, _} ->
		    Res = Converter(Log2),
		    disk_log:close(Log2),
		    Res;
		{error, {name_already_open, _}} ->
                    Converter(Log2);
                {error, Reason} ->
                    {error, {Log, Reason}}
	    end
    end.

convert_name(Name) when is_list(Name) ->
    Name ++ "_tmp";
convert_name(Name) when is_atom(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_tmp");
convert_name(Name) ->
    lists:flatten(io_lib:format("~w_tmp", [Name])).


%% -- do_log_to_text ---

do_log_to_file(Log, TextFile, Mibs, Start, Stop) ->
    case file:open(TextFile, [write]) of
        {ok, Fd} ->
            MiniMib = snmp_mini_mib:create(Mibs),
	    Write = fun(X) -> 
			    case format_msg(X, MiniMib, Start, Stop) of
				{ok, S} ->
				    io:format(Fd, "~s", [S]);
				_ ->
				    ok
			    end
		    end,
            Res = (catch loop(disk_log:chunk(Log, start), Log, Write)),
	    snmp_mini_mib:delete(MiniMib), 
            file:close(Fd),
            Res;
        {error, Reason} ->
            {error, {TextFile, Reason}}
    end.


do_log_to_io(Log, Mibs, Start, Stop) ->
    MiniMib = snmp_mini_mib:create(Mibs),
    Write = fun(X) -> 
		    case format_msg(X, MiniMib, Start, Stop) of
			{ok, S} ->
			    io:format("~s", [S]);
			_ ->
			    ok
		    end
	    end,
    (catch loop(disk_log:chunk(Log, start), Log, Write)),
    snmp_mini_mib:delete(MiniMib),
    ok.


loop(eof, _Log, _Write) ->
    ok;
loop({error, _} = Error, _Log, _Write) ->
    Error;
loop({corrupt_log_file, _} = Reason, _Log, _Write) ->
    {error, Reason};
loop({Cont, Terms}, Log, Write) ->
    case (catch lists:foreach(Write, Terms)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	_ ->
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
    Error.

format_msg({TimeStamp, {V3Hdr, ScopedPdu}, {Addr, Port}}, 
	   Mib, Start, Stop) ->
    format_msg({TimeStamp, {V3Hdr, ScopedPdu}, Addr, Port}, 
	      Mib, Start, Stop);
format_msg({TimeStamp, {V3Hdr, ScopedPdu}, Addr, Port}, 
	  Mib, Start, Stop) ->
%     io:format("format_msg -> entry with"
% 	      "~n   TimeStamp: ~p"
% 	      "~n   Start:     ~p"
% 	      "~n   Stop:      ~p", [TimeStamp, Start, Stop]),
    case timestamp_filter(TimeStamp, Start, Stop) of
        true ->
            case (catch snmp_pdus:dec_scoped_pdu(ScopedPdu)) of
                ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
                    Msg = #message{version = 'version-3',
                                   vsn_hdr = V3Hdr,
                                   data    = ScopedPDU},
                    f(ts2str(TimeStamp), Msg, Addr, Port, Mib);
                {'EXIT', Reason} ->
                    format_tab("** error in log file at ~s from ~p:~w ~p\n\n", 
			       [ts2str(TimeStamp), ip(Addr), Port, Reason])
            end;
        false ->
            ignore
    end;
format_msg({TimeStamp, Packet, {Addr, Port}}, Mib, Start, Stop) -> 
    format_msg({TimeStamp, Packet, Addr, Port}, Mib, Start, Stop);
format_msg({TimeStamp, Packet, Addr, Port}, Mib, Start, Stop) ->
    case timestamp_filter(TimeStamp, Start, Stop) of
        true ->
            case (catch snmp_pdus:dec_message(binary_to_list(Packet))) of
                Msg when is_record(Msg, message) ->
                    f(ts2str(TimeStamp), Msg, Addr, Port, Mib);
                {'EXIT', Reason} ->
                    format_tab("** error in log file ~p\n\n", [Reason])
            end;
        false ->
            ignore
    end;
format_msg(_, _Mib, _Start, _Stop) ->
    format_tab("** unknown entry in log file\n\n", []).

f(TimeStamp, #message{version = Vsn, vsn_hdr = VsnHdr, data = Data}, 
  Addr, Port, Mib) ->
    Str    = format_pdu(Data, Mib),
    HdrStr = format_header(Vsn, VsnHdr),
    case get_type(Data) of
        trappdu ->
            f_trap(TimeStamp, Vsn, HdrStr, Str, Addr, Port);
        'snmpv2-trap' ->
            f_trap(TimeStamp, Vsn, HdrStr, Str, Addr, Port);
        'inform-request' ->
            f_inform(TimeStamp, Vsn, HdrStr, Str, Addr, Port);
        'get-response' ->
            f_response(TimeStamp, Vsn, HdrStr, Str, Addr, Port);
        report ->
            f_report(TimeStamp, Vsn, HdrStr, Str, Addr, Port);
        _ ->
            f_request(TimeStamp, Vsn, HdrStr, Str, Addr, Port)
    end.

f_request(TimeStamp, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("request ~s:~w - ~s [~s] ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, Vsn, Str]).

f_response(TimeStamp, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("response ~s:~w - ~s [~s] ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, Vsn, Str]).

f_report(TimeStamp, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("report ~s:~w - ~s [~s] ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, Vsn, Str]).

f_trap(TimeStamp, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("trap ~s:~w - ~s [~s] ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, Vsn, Str]).

f_inform(TimeStamp, Vsn, HdrStr, Str, Addr, Port) ->
    format_tab("inform ~s:~w - ~s [~s] ~w\n~s", 
	       [ip(Addr), Port, HdrStr, TimeStamp, Vsn, Str]).


%% Convert a timestamp 2-tupple to a printable string
%%
ts2str({Local,Universal}) ->
    dat2str(Local) ++ " , " ++ dat2str(Universal);
ts2str(_) ->
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

tsf_ge(TimeStamp,DateTime) -> 
    T1 = calendar:datetime_to_gregorian_seconds(TimeStamp),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime),
    T1 >= T2.

tsf_le(_Local,_Universal,null) ->
    true;
tsf_le(Local,_Universal,{local_time,DateTime}) ->
    tsf_le(Local,DateTime);
tsf_le(_Local,Universal,{universal_time,DateTime}) ->
    tsf_le(Universal,DateTime);
tsf_le(Local,_Universal,DateTime) ->
    tsf_le(Local,DateTime).

tsf_le(TimeStamp,DateTime) ->
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
    io_lib:format("~w.~w.~w.~w", [A,B,C,D]).



%% -------------------------------------------------------------------    
%% Various utility functions
%% -------------------------------------------------------------------    

log_open(Name, File, Size, Repair, Notify) ->
    case do_log_open(Name, File, Size, Repair, Notify) of
	{ok, Log} ->
	    {ok, Log};
	{repaired, Log, Rec, Bad} ->
	    ?vlog("log_open -> repaired: "
		  "~n   Rec: ~p"
		  "~n   Bad: ~p", [Rec, Bad]),
	    {ok, Log};
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
    Opts = [{name,   Name}, 
	    {file,   File}, 
	    {type,   ?LOG_TYPE},
	    {format, ?LOG_FORMAT},	    
	    {mode,   read_only}],
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

