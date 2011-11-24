%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(io).

-export([put_chars/1,put_chars/2,nl/0,nl/1,
	 get_chars/2,get_chars/3,get_line/1,get_line/2,
	 get_password/0, get_password/1,
	 setopts/1, setopts/2, getopts/0, getopts/1]).
-export([write/1,write/2,read/1,read/2,read/3]).
-export([columns/0,columns/1,rows/0,rows/1]).
-export([fwrite/1,fwrite/2,fwrite/3,fread/2,fread/3,
	 format/1,format/2,format/3]).
-export([scan_erl_exprs/1,scan_erl_exprs/2,scan_erl_exprs/3,
	 scan_erl_form/1,scan_erl_form/2,scan_erl_form/3,
	 parse_erl_exprs/1,parse_erl_exprs/2,parse_erl_exprs/3,
	 parse_erl_form/1,parse_erl_form/2,parse_erl_form/3]).
-export([request/1,request/2,requests/1,requests/2]).

-export_type([device/0, format/0]).

%%-------------------------------------------------------------------------

-type device() :: atom() | pid().
-type prompt() :: atom() | string().

-type error_description() :: term(). % Whatever the io-server sends.
-type request_error() :: {'error',error_description()}.

%% XXX: Some uses of line() in this file may need to read erl_scan:location()
-type line()   :: pos_integer().

%%-------------------------------------------------------------------------

%%
%% User interface.
%%

%% Writing and reading characters.

to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

o_request(Io, Request, Func) ->
    case request(Io, Request) of
	{error, Reason} ->
	    [_Name | Args] = tuple_to_list(to_tuple(Request)),
	    {'EXIT',{get_stacktrace,[_Current|Mfas]}} = (catch erlang:error(get_stacktrace)),
	    erlang:raise(error, conv_reason(Func, Reason), [{io, Func, [Io | Args]}|Mfas]);
	Other ->
	    Other
    end.

%% Put chars takes mixed *unicode* list from R13 onwards.
-spec put_chars(CharData) -> 'ok' when
      CharData :: unicode:chardata().

put_chars(Chars) ->
    put_chars(default_output(), Chars).

-spec put_chars(IoDevice, IoData) -> 'ok' when
      IoDevice :: device(),
      IoData :: unicode:chardata().

put_chars(Io, Chars) ->
    o_request(Io, {put_chars,unicode,Chars}, put_chars).

-spec nl() -> 'ok'.

nl() ->
    nl(default_output()).

-spec nl(IoDevice) -> 'ok' when
      IoDevice :: device().

nl(Io) ->
%    o_request(Io, {put_chars,io_lib:nl()}).
    o_request(Io, nl, nl).

-spec columns() -> {'ok', pos_integer()} | {'error', 'enotsup'}.

columns() ->
    columns(default_output()).

-spec columns(IoDevice) -> {'ok', pos_integer()} | {'error', 'enotsup'} when
      IoDevice :: device().

columns(Io) ->
    case request(Io, {get_geometry,columns}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.
	    
-spec rows() -> {'ok', pos_integer()} | {'error', 'enotsup'}.

rows() ->
    rows(default_output()).

-spec rows(IoDevice) -> {'ok', pos_integer()} | {'error', 'enotsup'} when
      IoDevice :: device().

rows(Io) ->
    case request(Io,{get_geometry,rows}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.    

-spec get_chars(Prompt, Count) -> Data | 'eof' when
      Prompt :: prompt(),
      Count :: non_neg_integer(),
      Data :: [unicode:unicode_char()] | unicode:unicode_binary().

get_chars(Prompt, N) ->
    get_chars(default_input(), Prompt, N).

-spec get_chars(IoDevice, Prompt, Count) -> Data | 'eof' | {error, Reason} when
      IoDevice :: device(),
      Prompt :: prompt(),
      Count :: non_neg_integer(),
      Reason :: term(),
      Data :: [unicode:unicode_char()] | unicode:unicode_binary().

get_chars(Io, Prompt, N) when is_integer(N), N >= 0 ->
    request(Io, {get_chars,unicode,Prompt,N}).

-spec get_line(Prompt) -> Data | 'eof' | {'error', Reason} when
      Prompt :: prompt(),
      Reason :: term(),
      Data :: [unicode:unicode_char()] | unicode:unicode_binary().

get_line(Prompt) ->
    get_line(default_input(), Prompt).

-spec get_line(IoDevice, Prompt) -> Data | 'eof' | {'error', term()} when
      IoDevice :: device(),
      Prompt :: prompt(),
      Data :: [unicode:unicode_char()] | unicode:unicode_binary().

get_line(Io, Prompt) ->
    request(Io, {get_line,unicode,Prompt}).

get_password() ->
    get_password(default_input()).

get_password(Io) ->
    request(Io, {get_password,unicode}).

-type encoding()   :: 'latin1' | 'unicode' | 'utf8' | 'utf16' | 'utf32'
                    | {'utf16', 'big' | 'little'} | {'utf32','big' | 'little'}.
-type expand_fun() :: fun((term()) -> {'yes'|'no', string(), [string(), ...]}).
-type opt_pair()   :: {'binary', boolean()}
                    | {'echo', boolean()}
                    | {'expand_fun', expand_fun()}
                    | {'encoding', encoding()}.

-spec getopts() -> [opt_pair()].

getopts() ->
    getopts(default_input()).

-spec getopts(IoDevice) -> [opt_pair()] when
      IoDevice :: device().

getopts(Io) ->
    request(Io, getopts).

-type setopt() :: 'binary' | 'list' | opt_pair().

-spec setopts(Opts) -> 'ok' | {'error', Reason} when
      Opts :: [setopt()],
      Reason :: term().

setopts(Opts) ->
    setopts(default_input(), Opts).

-spec setopts(IoDevice, Opts) -> 'ok' | {'error', Reason} when
      IoDevice :: device(),
      Opts :: [setopt()],
      Reason :: term().

setopts(Io, Opts) ->
    request(Io, {setopts, Opts}).

%% Writing and reading Erlang terms.

-spec write(Term) -> 'ok' when
      Term :: term().

write(Term) ->
    write(default_output(), Term).

-spec write(IoDevice, Term) -> 'ok' when
      IoDevice :: device(),
      Term :: term().

write(Io, Term) ->
    o_request(Io, {write,Term}, write).


-spec read(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: {'ok', Term :: term()}
              | 'eof'
              | {'error', ErrorInfo :: erl_scan:error_info()}.

% Read does not use get_until as erl_scan does not work with unicode
% XXX:PaN fixme?
read(Prompt) ->
    read(default_input(), Prompt).

-spec read(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: {'ok', Term :: term()}
              | 'eof'
              | {'error', ErrorInfo :: erl_scan:error_info()}.

read(Io, Prompt) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[1]}) of
	{ok,Toks,_EndLine} ->
	    erl_parse:parse_term(Toks);
%	{error, Reason} when atom(Reason) ->
%	    erlang:error(conv_reason(read, Reason), [Io, Prompt]);
	{error,E,_EndLine} ->
	    {error,E};
	{eof,_EndLine} ->
	    eof;
	Other ->
	    Other
    end.

-spec read(IoDevice, Prompt, StartLine) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLine :: line(),
      Result :: {'ok', Term :: term(), EndLine :: line()}
              | {'eof', EndLine :: line()}
              | {'error', ErrorInfo :: erl_scan:error_info(), ErrorLine :: line()}.

read(Io, Prompt, StartLine) when is_integer(StartLine) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[StartLine]}) of
	{ok,Toks,EndLine} ->
            case erl_parse:parse_term(Toks) of
                {ok,Term} -> {ok,Term,EndLine};
                {error,ErrorInfo} -> {error,ErrorInfo,EndLine}
            end;
	{error,_E,_EndLine} = Error ->
	    Error;
	{eof,_EndLine} = Eof ->
	    Eof;
	Other ->
	    Other
    end.

%% Formatted writing and reading.

conv_reason(_, arguments) -> badarg;
conv_reason(_, terminated) -> terminated;
conv_reason(_, {no_translation,_,_}) -> no_translation;
conv_reason(_, _Reason) -> badarg.

-type format() :: atom() | string() | binary().

-spec fwrite(Format) -> 'ok' when
      Format :: format().

fwrite(Format) ->
    format(Format).

-spec fwrite(Format, Data) -> 'ok' when
      Format :: format(),
      Data :: [term()].

fwrite(Format, Args) ->
    format(Format, Args).

-spec fwrite(IoDevice, Format, Data) -> 'ok' when
      IoDevice :: device(),
      Format :: format(),
      Data :: [term()].

fwrite(Io, Format, Args) ->
    format(Io, Format, Args).

-spec fread(Prompt, Format) -> Result when
      Prompt :: prompt(),
      Format :: format(),
      Result :: {'ok', Terms :: [term()]} | 'eof' | {'error', What :: term()}.

fread(Prompt, Format) ->
    fread(default_input(), Prompt, Format).

-spec fread(IoDevice, Prompt, Format) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Format :: format(),
      Result :: {'ok', Terms :: [term()]} | 'eof' | {'error', What :: term()}.

fread(Io, Prompt, Format) ->
    case request(Io, {fread,Prompt,Format}) of
%	{error, Reason} when atom(Reason) ->
%	    erlang:error(conv_reason(fread, Reason), [Io, Prompt, Format]);
	Other ->
	    Other
    end.

-spec format(Format) -> 'ok' when
      Format :: format().

format(Format) ->
    format(Format, []).

-spec format(Format, Data) -> 'ok' when
      Format :: format(),
      Data :: [term()].

format(Format, Args) ->
    format(default_output(), Format, Args).

-spec format(IoDevice, Format, Data) -> 'ok' when
      IoDevice :: device(),
      Format :: format(),
      Data :: [term()].

format(Io, Format, Args) ->
    o_request(Io, {format,Format,Args}, format).

%% Scanning Erlang code.

-spec scan_erl_exprs(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | request_error().
 
scan_erl_exprs(Prompt) ->
    scan_erl_exprs(default_input(), Prompt, 1).

-spec scan_erl_exprs(Device, Prompt) -> Result when
      Device :: device(),
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | request_error().

scan_erl_exprs(Io, Prompt) ->
    scan_erl_exprs(Io, Prompt, 1).

-spec scan_erl_exprs(Device, Prompt, StartLine) -> Result when
      Device :: device(),
      Prompt :: prompt(),
      StartLine :: line(),
      Result :: erl_scan:tokens_result() | request_error().

scan_erl_exprs(Io, Prompt, Pos0) ->
    request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0]}).

-spec scan_erl_form(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | request_error().

scan_erl_form(Prompt) ->
    scan_erl_form(default_input(), Prompt, 1).

-spec scan_erl_form(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | request_error().

scan_erl_form(Io, Prompt) ->
    scan_erl_form(Io, Prompt, 1).

-spec scan_erl_form(IoDevice, Prompt, StartLine) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLine :: line(),
      Result :: erl_scan:tokens_result() | request_error().

scan_erl_form(Io, Prompt, Pos0) ->
    request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0]}).

%% Parsing Erlang code.

-type parse_ret() :: {'ok', ExprList :: erl_parse:abstract_expr(), EndLine :: line()}
                   | {'eof', EndLine :: line()}
                   | {'error', ErrorInfo :: erl_scan:error_info(), ErrorLine :: line()}
                   | request_error().

-spec parse_erl_exprs(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: parse_ret().

parse_erl_exprs(Prompt) ->
    parse_erl_exprs(default_input(), Prompt, 1).

-spec parse_erl_exprs(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: parse_ret().

parse_erl_exprs(Io, Prompt) ->
    parse_erl_exprs(Io, Prompt, 1).

-spec parse_erl_exprs(IoDevice, Prompt, StartLine) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLine :: line(),
      Result :: parse_ret().

parse_erl_exprs(Io, Prompt, Pos0) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0]}) of
	{ok,Toks,EndPos} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

-type parse_form_ret() :: {'ok', AbsForm :: erl_parse:abstract_form(), EndLine :: line()}
                        | {'eof', EndLine :: line()}
                        | {'error', ErrorInfo :: erl_scan:error_info(), ErrorLine :: line()}
                        | request_error().

-spec parse_erl_form(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: parse_form_ret().

parse_erl_form(Prompt) ->
    parse_erl_form(default_input(), Prompt, 1).

-spec parse_erl_form(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: parse_form_ret().

parse_erl_form(Io, Prompt) ->
    parse_erl_form(Io, Prompt, 1).

-spec parse_erl_form(IoDevice, Prompt, StartLine) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLine :: line(),
      Result :: parse_form_ret().

parse_erl_form(Io, Prompt, Pos0) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0]}) of
	{ok,Toks,EndPos} ->
	    case erl_parse:parse_form(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

%% Miscellaneous functions.

request(Request) ->
    request(default_output(), Request).

request(standard_io, Request) ->
    request(group_leader(), Request);
request(Pid, Request) when is_pid(Pid) ->
    execute_request(Pid, io_request(Pid, Request));
request(Name, Request) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    request(Pid, Request)
    end.

execute_request(Pid, {Convert,Converted}) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {io_request,self(),Pid,Converted},
    if
	Convert ->
	    convert_binaries(wait_io_mon_reply(Pid, Mref));
	true ->
	    wait_io_mon_reply(Pid, Mref)
    end.

requests(Requests) ->				%Requests as atomic action
    requests(default_output(), Requests).

requests(standard_io, Requests) ->              %Requests as atomic action
    requests(group_leader(), Requests);
requests(Pid, Requests) when is_pid(Pid) ->
    {Convert, Converted} = io_requests(Pid, Requests),
    execute_request(Pid,{Convert,{requests,Converted}});
requests(Name, Requests) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    requests(Pid, Requests)
    end.


default_input() ->
    group_leader().

default_output() ->
    group_leader().

wait_io_mon_reply(From, Mref) ->
    receive
	{io_reply, From, Reply} ->
	    erlang:demonitor(Mref),
	    receive 
		{'DOWN', Mref, _, _, _} -> true
	    after 0 -> true
	    end,
	    Reply;
	{'EXIT', From, _What} ->
	    receive
		{'DOWN', Mref, _, _, _} -> true
	    after 0 -> true
	    end,
	    {error,terminated};
	{'DOWN', Mref, _, _, _} ->
	    receive
		{'EXIT', From, _What} -> true
	    after 0 -> true
	    end,
	    {error,terminated}
    end.


%% io_requests(Requests)
%%  Transform requests into correct i/o server messages. Only handle the
%%  one we KNOW must be changed, others, including incorrect ones, are
%%  passed straight through. Perform a flatten on the request list.

io_requests(Pid, Rs) ->
    io_requests(Pid, Rs, [], []).

io_requests(Pid, [{requests,Rs1}|Rs], Cont, Tail) ->
    io_requests(Pid, Rs1, [Rs|Cont], Tail);
io_requests(Pid, [R], [], _Tail) ->
    {Conv,Request} = io_request(Pid, R),
    {Conv,[Request]};
io_requests(Pid, [R|Rs], Cont, Tail) ->
    {_,Request} = io_request(Pid, R),
    {Conv,Requests} = io_requests(Pid, Rs, Cont, Tail),
    {Conv,[Request|Requests]};
io_requests(Pid, [], [Rs|Cont], Tail) ->
    io_requests(Pid, Rs, Cont, Tail);
io_requests(_Pid, [], [], _Tail) -> 
    {false,[]}.


bc_req(Pid,{Op,Enc,Param},MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    {false,{Op,Enc,Param}};
	false ->
	    {MaybeConvert,{Op,Param}}
    end;
bc_req(Pid,{Op,Enc,P,F},MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    {false,{Op,Enc,P,F}};
	false ->
	    {MaybeConvert,{Op,P,F}}
    end;
bc_req(Pid, {Op,Enc,M,F,A},MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    {false,{Op,Enc,M,F,A}};
	false ->
	    {MaybeConvert,{Op,M,F,A}}
    end;
bc_req(Pid, {Op,Enc,P,M,F,A},MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    {false,{Op,Enc,P,M,F,A}};
	false ->
	    {MaybeConvert,{Op,P,M,F,A}}
    end;
bc_req(Pid,{Op,Enc},MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    {false,{Op, Enc}};
	false ->
	    {MaybeConvert,Op}
    end.

io_request(Pid, {write,Term}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,write,[Term]},false);
io_request(Pid, {format,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,format,[Format,Args]},false);
io_request(Pid, {fwrite,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,fwrite,[Format,Args]},false);
io_request(Pid, nl) ->
    bc_req(Pid,{put_chars,unicode,io_lib:nl()},false);
io_request(Pid, {put_chars,Enc,Chars}=Request0) 
  when is_list(Chars), node(Pid) =:= node() ->
    %% Convert to binary data if the I/O server is guaranteed to be new
    Request =
	case catch unicode:characters_to_binary(Chars,Enc) of
	    Binary when is_binary(Binary) ->
		{put_chars,Enc,Binary};
	    _ ->
		Request0
	end,
    {false,Request};
io_request(Pid, {put_chars,Enc,Chars}=Request0) 
  when is_list(Chars) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    case catch unicode:characters_to_binary(Chars,Enc,unicode) of
		Binary when is_binary(Binary) ->
		    {false,{put_chars,unicode,Binary}};
		_ ->
		    {false,Request0}
	    end;
	false ->
	    %% Convert back to old style put_chars message...
	    case catch unicode:characters_to_binary(Chars,Enc,latin1) of
		Binary when is_binary(Binary) ->
		    {false,{put_chars,Binary}};
		_ ->
		    {false,{put_chars,Chars}}
	    end
    end;
io_request(Pid, {fread,Prompt,Format}) ->
    bc_req(Pid,{get_until,unicode,Prompt,io_lib,fread,[Format]},true);
io_request(Pid, {get_until,Enc,Prompt,M,F,A}) ->
    bc_req(Pid,{get_until,Enc,Prompt,M,F,A},true);
io_request(Pid, {get_chars,Enc,Prompt,N}) ->
    bc_req(Pid,{get_chars,Enc,Prompt,N},true);
io_request(Pid, {get_line,Enc,Prompt}) ->
    bc_req(Pid,{get_line,Enc,Prompt},true);
io_request(Pid, {get_password,Enc}) ->
    bc_req(Pid,{get_password, Enc},true);
io_request(_Pid, R) ->				%Pass this straight through
    {false,R}.

convert_binaries(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(Bin,latin1,unicode);
convert_binaries(Else) ->
    Else.
