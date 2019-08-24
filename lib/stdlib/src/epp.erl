%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

-module(epp).

%% An Erlang code preprocessor.

-export([open/1, open/2,open/3,open/5,close/1,format_error/1]).
-export([scan_erl_form/1,parse_erl_form/1,macro_defs/1]).
-export([parse_file/1, parse_file/2, parse_file/3]).
-export([default_encoding/0, encoding_to_string/1,
         read_encoding_from_binary/1, read_encoding_from_binary/2,
         set_encoding/1, set_encoding/2, read_encoding/1, read_encoding/2]).
-export([interpret_file_attribute/1]).
-export([normalize_typed_record_fields/1,restore_typed_record_fields/1]).

%%------------------------------------------------------------------------

-export_type([source_encoding/0]).

-type macros() :: [atom() | {atom(), term()}].
-type epp_handle() :: pid().
-type source_encoding() :: latin1 | utf8.

-type ifdef() :: 'ifdef' | 'ifndef' | 'if' | 'else'.

-type name() :: atom().
-type argspec() :: 'none'                       %No arguments
                 | non_neg_integer().           %Number of arguments
-type argnames() :: [atom()].
-type tokens() :: [erl_scan:token()].
-type predef() :: 'undefined' | {'none', tokens()}.
-type userdef() :: {argspec(), {argnames(), tokens()}}.
-type used() :: {name(), argspec()}.

-type function_name_type() :: 'undefined'
			    | {atom(),non_neg_integer()}
			    | tokens().

-type warning_info() :: {erl_anno:location(), module(), term()}.

-define(DEFAULT_ENCODING, utf8).

%% Epp state record.
-record(epp, {file :: file:io_device()
                    | 'undefined',              %Current file
	      location=1,         		%Current location
              delta=0 :: non_neg_integer(),     %Offset from Location (-file)
              name="" :: file:name(),           %Current file name
              name2="" :: file:name(),          %-"-, modified by -file
              istk=[] :: [ifdef()],             %Ifdef stack
              sstk=[] :: [#epp{}],              %State stack
              path=[] :: [file:name()],         %Include-path
              macs = #{}		        %Macros (don't care locations)
	            :: #{name() => predef() | [userdef()]},
              uses = #{}			%Macro use structure
	            :: #{name() => [{argspec(), [used()]}]},
              default_encoding = ?DEFAULT_ENCODING :: source_encoding(),
	      pre_opened = false :: boolean(),
	      fname = [] :: function_name_type()
	     }).

%% open(Options)
%% open(FileName, IncludePath)
%% open(FileName, IncludePath, PreDefMacros)
%% open(FileName, IoDevice, StartLocation, IncludePath, PreDefMacros)
%% close(Epp)
%% scan_erl_form(Epp)
%% parse_erl_form(Epp)
%% parse_file(Epp)
%% parse_file(FileName, Options)
%% parse_file(FileName, IncludePath, PreDefMacros)
%% macro_defs(Epp)

-spec open(FileName, IncludePath) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      Epp :: epp_handle(),
      ErrorDescriptor :: term().

open(Name, Path) ->
    open(Name, Path, []).

-spec open(FileName, IncludePath, PredefMacros) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      PredefMacros :: macros(),
      Epp :: epp_handle(),
      ErrorDescriptor :: term().

open(Name, Path, Pdm) ->
    internal_open([{name, Name}, {includes, Path}, {macros, Pdm}], #epp{}).

open(Name, File, StartLocation, Path, Pdm) ->
    internal_open([{name, Name}, {includes, Path}, {macros, Pdm}],
		  #epp{file=File, pre_opened=true, location=StartLocation}).

-spec open(Options) ->
		  {'ok', Epp} | {'ok', Epp, Extra} | {'error', ErrorDescriptor} when
      Options :: [{'default_encoding', DefEncoding :: source_encoding()} |
		  {'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'source_name', SourceName :: file:name()} |
		  {'macros', PredefMacros :: macros()} |
		  {'name',FileName :: file:name()} |
		  'extra'],
      Epp :: epp_handle(),
      Extra :: [{'encoding', source_encoding() | 'none'}],
      ErrorDescriptor :: term().

open(Options) ->
    internal_open(Options, #epp{}).

internal_open(Options, St) ->
    case proplists:get_value(name, Options) of
        undefined ->
            erlang:error(badarg);
        Name ->
            Self = self(),
            Epp = spawn(fun() -> server(Self, Name, Options, St) end),
            case epp_request(Epp) of
                {ok, Pid, Encoding} ->
                    case proplists:get_bool(extra, Options) of
                        true -> {ok, Pid, [{encoding, Encoding}]};
                        false -> {ok, Pid}
                    end;
                Other ->
                    Other
            end
    end.

-spec close(Epp) -> 'ok' when
      Epp :: epp_handle().

close(Epp) ->
    %% Make sure that close is synchronous as a courtesy to test
    %% cases that test for resource leaks.
    Ref = erlang:monitor(process, Epp),
    R = epp_request(Epp, close),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    R.

scan_erl_form(Epp) ->
    epp_request(Epp, scan_erl_form).

-spec parse_erl_form(Epp) ->
    {'ok', AbsForm} | {error, ErrorInfo} |
    {'warning',WarningInfo} | {'eof',Line} when
      Epp :: epp_handle(),
      AbsForm :: erl_parse:abstract_form(),
      Line :: erl_anno:line(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      WarningInfo :: warning_info().

parse_erl_form(Epp) ->
    case epp_request(Epp, scan_erl_form) of
	{ok,Toks} ->
	    erl_parse:parse_form(Toks);
	Other ->
	    Other
    end.

macro_defs(Epp) ->
    epp_request(Epp, macro_defs).

%% format_error(ErrorDescriptor) -> String
%%  Return a string describing the error.

-spec format_error(ErrorDescriptor) -> io_lib:chars() when
      ErrorDescriptor :: term().

format_error(cannot_parse) ->
    io_lib:format("cannot parse file, giving up", []);
format_error({bad,W}) ->
    io_lib:format("badly formed '~s'", [W]);
format_error(missing_parenthesis) ->
    io_lib:format("badly formed define: missing closing right parenthesis",[]);
format_error(premature_end) ->
    "premature end";
format_error({call,What}) ->
    io_lib:format("illegal macro call '~ts'",[What]);
format_error({undefined,M,none}) ->
    io_lib:format("undefined macro '~ts'", [M]);
format_error({undefined,M,A}) ->
    io_lib:format("undefined macro '~ts/~p'", [M,A]);
format_error({depth,What}) ->
    io_lib:format("~s too deep",[What]);
format_error({mismatch,M}) ->
    io_lib:format("argument mismatch for macro '~ts'", [M]);
format_error({arg_error,M}) ->
    io_lib:format("badly formed argument for macro '~ts'", [M]);
format_error({redefine,M}) ->
    io_lib:format("redefining macro '~ts'", [M]);
format_error({redefine_predef,M}) ->
    io_lib:format("redefining predefined macro '~s'", [M]);
format_error({circular,M,none}) ->
    io_lib:format("circular macro '~ts'", [M]);
format_error({circular,M,A}) ->
    io_lib:format("circular macro '~ts/~p'", [M,A]);
format_error({include,W,F}) ->
    io_lib:format("can't find include ~s \"~ts\"", [W,F]);
format_error({illegal,How,What}) ->
    io_lib:format("~s '-~s'", [How,What]);
format_error({illegal_function,Macro}) ->
    io_lib:format("?~s can only be used within a function", [Macro]);
format_error({illegal_function_usage,Macro}) ->
    io_lib:format("?~s must not begin a form", [Macro]);
format_error(elif_after_else) ->
    "'elif' following 'else'";
format_error({'NYI',What}) ->
    io_lib:format("not yet implemented '~s'", [What]);
format_error({error,Term}) ->
    io_lib:format("-error(~tp).", [Term]);
format_error({warning,Term}) ->
    io_lib:format("-warning(~tp).", [Term]);
format_error(E) -> file:format_error(E).

-spec parse_file(FileName, IncludePath, PredefMacros) ->
                {'ok', [Form]} | {error, OpenError} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      Form :: erl_parse:abstract_form() | {'error', ErrorInfo} | {'eof',Line},
      PredefMacros :: macros(),
      Line :: erl_anno:line(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Path, Predefs) ->
    parse_file(Ifile, [{includes, Path}, {macros, Predefs}]).

-spec parse_file(FileName, Options) ->
        {'ok', [Form]} | {'ok', [Form], Extra} | {error, OpenError} when
      FileName :: file:name(),
      Options :: [{'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'source_name', SourceName :: file:name()} |
		  {'macros', PredefMacros :: macros()} |
		  {'default_encoding', DefEncoding :: source_encoding()} |
		  'extra'],
      Form :: erl_parse:abstract_form() | {'error', ErrorInfo} | {'eof',Line},
      Line :: erl_anno:line(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      Extra :: [{'encoding', source_encoding() | 'none'}],
      OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Options) ->
    case internal_open([{name, Ifile} | Options], #epp{}) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    close(Epp),
	    {ok,Forms};
	{ok,Epp,Extra} ->
	    Forms = parse_file(Epp),
	    close(Epp),
	    {ok,Forms,Extra};
	{error,E} ->
	    {error,E}
    end.

-spec parse_file(Epp) -> [Form] when
      Epp :: epp_handle(),
      Form :: erl_parse:abstract_form() | {'error', ErrorInfo} |
	      {'warning',WarningInfo} | {'eof',Line},
      Line :: erl_anno:line(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      WarningInfo :: warning_info().

parse_file(Epp) ->
    case parse_erl_form(Epp) of
	{ok,Form} ->
            [Form|parse_file(Epp)];
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{warning,W} ->
	    [{warning,W}|parse_file(Epp)];
	{eof,Location} ->
	    [{eof,Location}]
    end.

-spec default_encoding() -> source_encoding().

default_encoding() ->
    ?DEFAULT_ENCODING.

-spec encoding_to_string(Encoding) -> string() when
      Encoding :: source_encoding().

encoding_to_string(latin1) -> "coding: latin-1";
encoding_to_string(utf8) -> "coding: utf-8".

-spec read_encoding(FileName) -> source_encoding() | none when
      FileName :: file:name().

read_encoding(Name) ->
    read_encoding(Name, []).

-spec read_encoding(FileName, Options) -> source_encoding() | none when
      FileName :: file:name(),
      Options :: [Option],
      Option :: {in_comment_only, boolean()}.

read_encoding(Name, Options) ->
    InComment = proplists:get_value(in_comment_only, Options, true),
    case file:open(Name, [read]) of
        {ok,File} ->
            try read_encoding_from_file(File, InComment)
            after ok = file:close(File)
            end;
        _Error ->
            none
    end.

-spec set_encoding(File) -> source_encoding() | none when
      File :: io:device(). % pid(); raw files don't work

set_encoding(File) ->
    set_encoding(File, ?DEFAULT_ENCODING).

-spec set_encoding(File, Default) -> source_encoding() | none when
      Default :: source_encoding(),
      File :: io:device(). % pid(); raw files don't work

set_encoding(File, Default) ->
    Encoding = read_encoding_from_file(File, true),
    Enc = case Encoding of
              none -> Default;
              Encoding -> Encoding
          end,
    ok = io:setopts(File, [{encoding, Enc}]),
    Encoding.

-spec read_encoding_from_binary(Binary) -> source_encoding() | none when
      Binary :: binary().

-define(ENC_CHUNK, 32).
-define(N_ENC_CHUNK, 16). % a total of 512 bytes

read_encoding_from_binary(Binary) ->
    read_encoding_from_binary(Binary, []).

-spec read_encoding_from_binary(Binary, Options) ->
                                       source_encoding() | none when
      Binary :: binary(),
      Options :: [Option],
      Option :: {in_comment_only, boolean()}.

read_encoding_from_binary(Binary, Options) ->
    InComment = proplists:get_value(in_comment_only, Options, true),
    try
        com_nl(Binary, fake_reader(0), 0, InComment)
    catch
        throw:no ->
            none
    end.

fake_reader(N) ->
    fun() when N =:= ?N_ENC_CHUNK ->
            throw(no);
       () ->
            {<<>>, fake_reader(N+1)}
    end.

-spec read_encoding_from_file(File, InComment) -> source_encoding() | none when
      File :: io:device(),
      InComment :: boolean().

read_encoding_from_file(File, InComment) ->
    {ok, Pos0} = file:position(File, cur),
    Opts = io:getopts(File),
    Encoding0 = lists:keyfind(encoding, 1, Opts),
    Binary0 = lists:keyfind(binary, 1, Opts),
    ok = io:setopts(File, [binary, {encoding, latin1}]),
    try
        {B, Fun} = (reader(File, 0))(),
        com_nl(B, Fun, 0, InComment)
    catch
        throw:no ->
            none
    after
        {ok, Pos0} = file:position(File, Pos0),
        ok = io:setopts(File, [Binary0, Encoding0])
    end.

reader(Fd, N) ->
    fun() when N =:= ?N_ENC_CHUNK ->
            throw(no);
       () ->
            case file:read(Fd, ?ENC_CHUNK) of
                eof ->
                    {<<>>, reader(Fd, N+1)};
                {ok, Bin} ->
                    {Bin, reader(Fd, N+1)};
                {error, _} ->
                    throw(no) % ignore errors
            end
    end.

com_nl(_, _, 2, _) ->
    throw(no);
com_nl(B, Fun, N, false=Com) ->
    com_c(B, Fun, N, Com);
com_nl(B, Fun, N, true=Com) ->
    com(B, Fun, N, Com).

com(<<"\n",B/binary>>, Fun, N, Com) ->
    com_nl(B, Fun, N+1, Com);
com(<<"%", B/binary>>, Fun, N, Com) ->
    com_c(B, Fun, N, Com);
com(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
    com(B, Fun, N, Com);
com(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com(B, Fun1, N, Com).

com_c(<<"c",B/binary>>, Fun, N, Com) ->
    com_oding(B, Fun, N, Com);
com_c(<<"\n",B/binary>>, Fun, N, Com) ->
    com_nl(B, Fun, N+1, Com);
com_c(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
    com_c(B, Fun, N, Com);
com_c(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_c(B, Fun1, N, Com).

com_oding(<<"oding",B/binary>>, Fun, N, Com) ->
    com_sep(B, Fun, N, Com);
com_oding(B, Fun, N, Com) when byte_size(B) >= length("oding") ->
    com_c(B, Fun, N, Com);
com_oding(B, Fun, N, Com) ->
    {B1, Fun1} = Fun(),
    com_oding(list_to_binary([B, B1]), Fun1, N, Com).

com_sep(<<":",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_sep(<<"=",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_sep(<<"\s",B/binary>>, Fun, N, Com) ->
    com_sep(B, Fun, N, Com);
com_sep(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_sep(B, Fun1, N, Com);
com_sep(B, Fun, N, Com) ->
    com_c(B, Fun, N, Com).

com_space(<<"\s",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_space(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_space(B, Fun1, N, Com);
com_space(B, Fun, N, _Com) ->
    com_enc(B, Fun, N, [], []).

com_enc(<<C:1/unit:8,B/binary>>, Fun, N, L, Ps) when C >= $a, C =< $z;
                                                     C >= $A, C =< $Z;
                                                     C >= $0, C =< $9 ->
    com_enc(B, Fun, N, [C | L], Ps);
com_enc(<<>>, Fun, N, L, Ps) ->
    case Fun() of
        {<<>>, _} ->
            com_enc_end([L | Ps]);
        {B, Fun1} ->
            com_enc(B, Fun1, N, L, Ps)
    end;
com_enc(<<"-",B/binary>>, Fun, N, L, Ps) ->
    com_enc(B, Fun, N, [], [L | Ps]);
com_enc(_B, _Fun, _N, L, Ps) ->
    com_enc_end([L | Ps]).

com_enc_end(Ps0) ->
    Ps = lists:reverse([lists:reverse(lowercase(P)) || P <- Ps0]),
    com_encoding(Ps).

com_encoding(["latin","1"|_]) ->
    latin1;
com_encoding(["utf","8"|_]) ->
    utf8;
com_encoding(_) ->
    throw(no). % Don't try any further

lowercase(S) ->
    unicode:characters_to_list(string:lowercase(S)).

normalize_typed_record_fields([]) ->
    {typed, []};
normalize_typed_record_fields(Fields) ->
    normalize_typed_record_fields(Fields, [], false).

normalize_typed_record_fields([], NewFields, Typed) ->
    case Typed of
	true -> {typed, lists:reverse(NewFields)};
	false -> not_typed
    end;
normalize_typed_record_fields([{typed_record_field,Field,_}|Rest],
			      NewFields, _Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], true);
normalize_typed_record_fields([Field|Rest], NewFields, Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], Typed).

restore_typed_record_fields([]) ->
    [];
restore_typed_record_fields([{attribute,La,record,{Record,_NewFields}},
                             {attribute,La,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    [{attribute,La,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([{attribute,La,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    %% This clause is due to the compiler's 'E' option.
    %% Record information kept by erl_expand_records.
    [{attribute,La,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([Form|Forms]) ->
    [Form|restore_typed_record_fields(Forms)].

server(Pid, Name, Options, #epp{pre_opened=PreOpened}=St) ->
    process_flag(trap_exit, true),
    case PreOpened of
        false ->
            case file:open(Name, [read]) of
                {ok,File} ->
                    init_server(Pid, Name, Options, St#epp{file = File});
                {error,E} ->
                    epp_reply(Pid, {error,E})
            end;
        true ->
            init_server(Pid, Name, Options, St)
    end.

init_server(Pid, FileName, Options, St0) ->
    SourceName = proplists:get_value(source_name, Options, FileName),
    Pdm = proplists:get_value(macros, Options, []),
    Ms0 = predef_macros(FileName),
    case user_predef(Pdm, Ms0) of
	{ok,Ms1} ->
	    #epp{file = File, location = AtLocation} = St0,
            DefEncoding = proplists:get_value(default_encoding, Options,
                                              ?DEFAULT_ENCODING),
            Encoding = set_encoding(File, DefEncoding),
            epp_reply(Pid, {ok,self(),Encoding}),
            %% ensure directory of current source file is
            %% first in path
            Path = [filename:dirname(FileName) |
                    proplists:get_value(includes, Options, [])],
            St = St0#epp{delta=0, name=SourceName, name2=SourceName,
			 path=Path, macs=Ms1,
			 default_encoding=DefEncoding},
            From = wait_request(St),
            Anno = erl_anno:new(AtLocation),
            enter_file_reply(From, file_name(SourceName), Anno,
			     AtLocation, code),
            wait_req_scan(St);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File) ->
    Machine = list_to_atom(erlang:system_info(machine)),
    Anno = line1(),
    OtpVersion = list_to_integer(erlang:system_info(otp_release)),
    Defs = [{'FILE', 	           {none,[{string,Anno,File}]}},
	    {'FUNCTION_NAME',      undefined},
	    {'FUNCTION_ARITY',     undefined},
	    {'LINE',		   {none,[{integer,Anno,1}]}},
	    {'MODULE',	           undefined},
	    {'MODULE_STRING',      undefined},
	    {'BASE_MODULE',	   undefined},
	    {'BASE_MODULE_STRING', undefined},
	    {'MACHINE',	           {none,[{atom,Anno,Machine}]}},
	    {Machine,	           {none,[{atom,Anno,true}]}},
	    {'OTP_RELEASE',	   {none,[{integer,Anno,OtpVersion}]}}
	   ],
    maps:from_list(Defs).

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
    user_predef(Pdm, Ms#{M=>{none,Exp}});
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
    case Ms of
	#{M:=Defs} when is_list(Defs) ->
	     %% User defined macros.
	    {error,{redefine,M}};
	#{M:=_Defs} ->
	    %% Predefined macros.
	    {error,{redefine_predef,M}};
	_ ->
	    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
	    user_predef(Pdm, Ms#{M=>[{none,{none,Exp}}]})
    end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
    user_predef([{M,true}|Pdm], Ms);
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_request(St) ->
    receive
	{epp_request,From,scan_erl_form} -> From;
	{epp_request,From,macro_defs} ->
	    %% Return the old format to avoid any incompability issues.
	    Defs = [{{atom,K},V} || {K,V} <- maps:to_list(St#epp.macs)],
	    epp_reply(From, Defs),
	    wait_request(St);
	{epp_request,From,close} ->
	    close_file(St),
	    epp_reply(From, ok),
	    exit(normal);
	{'EXIT',_,R} ->
	    exit(R);
	Other ->
	    io:fwrite("Epp: unknown '~w'\n", [Other]),
	    wait_request(St)
    end.

close_file(#epp{pre_opened = true}) ->
    ok;
close_file(#epp{pre_opened = false, file = File}) ->
    ok = file:close(File).

wait_req_scan(St) ->
    From = wait_request(St),
    scan_toks(From, St).

wait_req_skip(St, Sis) ->
    From = wait_request(St),
    skip_toks(From, St, Sis).

%% enter_file(FileName, IncludeToken, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

enter_file(_NewName, Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{loc(Inc),epp,{depth,"include"}}}),
    wait_req_scan(St);
enter_file(NewName, Inc, From, St) ->
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
            Loc = start_loc(St#epp.location),
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E} ->
	    epp_reply(From, {error,{loc(Inc),epp,{include,file,NewName}}}),
	    wait_req_scan(St)
    end.

%% enter_file2(File, FullName, From, EppState, AtLocation) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file2(NewF, Pname, From, St0, AtLocation) ->
    Anno = erl_anno:new(AtLocation),
    enter_file_reply(From, Pname, Anno, AtLocation, code),
    Ms0 = St0#epp.macs,
    Ms = Ms0#{'FILE':={none,[{string,Anno,Pname}]}},
    %% update the head of the include path to be the directory of the new
    %% source file, so that an included file can always include other files
    %% relative to its current location (this is also how C does it); note
    %% that the directory of the parent source file (the previous head of
    %% the path) must be dropped, otherwise the path used within the current
    %% file will depend on the order of file inclusions in the parent files
    Path = [filename:dirname(Pname) | tl(St0#epp.path)],
    DefEncoding = St0#epp.default_encoding,
    _ = set_encoding(NewF, DefEncoding),
    #epp{file=NewF,location=AtLocation,name=Pname,name2=Pname,delta=0,
         sstk=[St0|St0#epp.sstk],path=Path,macs=Ms,
         default_encoding=DefEncoding}.

enter_file_reply(From, Name, LocationAnno, AtLocation, Where) ->
    Anno0 = loc_anno(AtLocation),
    Anno = case Where of
               code -> Anno0;
               generated -> erl_anno:set_generated(true, Anno0)
           end,
    Rep = {ok, [{'-',Anno},{atom,Anno,file},{'(',Anno},
		{string,Anno,Name},{',',Anno},
		{integer,Anno,get_line(LocationAnno)},{')',LocationAnno},
                {dot,Anno}]},
    epp_reply(From, Rep).

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when is_atom(N) ->
    atom_to_list(N).

leave_file(From, St) ->
    case St#epp.istk of
	[I|Cis] ->
	    epp_reply(From,
		      {error,{St#epp.location,epp,
                              {illegal,"unterminated",I}}}),
	    leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
	    case St#epp.sstk of
		[OldSt|Sts] ->
		    close_file(St),
                    #epp{location=OldLoc, delta=Delta, name=OldName,
                         name2=OldName2} = OldSt,
                    CurrLoc = add_line(OldLoc, Delta),
                    Anno = erl_anno:new(CurrLoc),
		    Ms0 = St#epp.macs,
		    Ms = Ms0#{'FILE':={none,[{string,Anno,OldName2}]}},
                    NextSt = OldSt#epp{sstk=Sts,macs=Ms,uses=St#epp.uses},
		    enter_file_reply(From, OldName, Anno, CurrLoc, code),
                    case OldName2 =:= OldName of
                        true ->
                            ok;
                        false ->
                            NFrom = wait_request(NextSt),
                            OldAnno = erl_anno:new(OldLoc),
                            enter_file_reply(NFrom, OldName2, OldAnno,
                                             CurrLoc, generated)
                        end,
                    wait_req_scan(NextSt);
		[] ->
		    epp_reply(From, {eof,St#epp.location}),
		    wait_req_scan(St)
	    end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.location) of
	{ok,Toks,Cl} ->
	    scan_toks(Toks, From, St#epp{location=Cl});
	{error,E,Cl} ->
	    epp_reply(From, {error,E}),
	    wait_req_scan(St#epp{location=Cl});
	{eof,Cl} ->
	    leave_file(From, St#epp{location=Cl});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
	    leave_file(wait_request(St), St)	%This serious, just exit!
    end.

scan_toks([{'-',_Lh},{atom,_Ld,define}=Define|Toks], From, St) ->
    scan_define(Toks, Define, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,undef}=Undef|Toks], From, St) ->
    scan_undef(Toks, Undef, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,error}=Error|Toks], From, St) ->
    scan_err_warn(Toks, Error, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,warning}=Warn|Toks], From, St) ->
    scan_err_warn(Toks, Warn, From, St);
scan_toks([{'-',_Lh},{atom,_Li,include}=Inc|Toks], From, St) ->
    scan_include(Toks, Inc, From, St);
scan_toks([{'-',_Lh},{atom,_Li,include_lib}=IncLib|Toks], From, St) ->
    scan_include_lib(Toks, IncLib, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifdef}=IfDef|Toks], From, St) ->
    scan_ifdef(Toks, IfDef, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifndef}=IfnDef|Toks], From, St) ->
    scan_ifndef(Toks, IfnDef, From, St);
scan_toks([{'-',_Lh},{atom,_Le,'else'}=Else|Toks], From, St) ->
    scan_else(Toks, Else, From, St);
scan_toks([{'-',_Lh},{'if',_Le}=If|Toks], From, St) ->
    scan_if(Toks, If, From, St);
scan_toks([{'-',_Lh},{atom,_Le,elif}=Elif|Toks], From, St) ->
    scan_elif(Toks, Elif, From, St);
scan_toks([{'-',_Lh},{atom,_Le,endif}=Endif|Toks], From, St) ->
    scan_endif(Toks, Endif, From, St);
scan_toks([{'-',_Lh},{atom,_Lf,file}=FileToken|Toks0], From, St) ->
    case catch expand_macros(Toks0, St) of
	Toks1 when is_list(Toks1) ->
            scan_file(Toks1, FileToken, From, St);
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end;
scan_toks(Toks0, From, St) ->
    case catch expand_macros(Toks0, St#epp{fname=Toks0}) of
	Toks1 when is_list(Toks1) ->
	    epp_reply(From, {ok,Toks1}),
	    wait_req_scan(St#epp{macs=scan_module(Toks1, St#epp.macs)});
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end.

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
    scan_module_1(Ts, Ms);
scan_module([{'-',_Lh},{atom,_Lm,extends},{'(',_Ll}|Ts], Ms) ->
    scan_extends(Ts, Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',L}|Ts], Ms) ->
    %% Parameterized modules.
    scan_module_1([A,{')',L}|Ts], Ms);
scan_module_1([{atom,Ln,A}=ModAtom,{')',_Lr}|_Ts], Ms0) ->
    ModString = atom_to_list(A),
    Ms = Ms0#{'MODULE':={none,[ModAtom]}},
    Ms#{'MODULE_STRING':={none,[{string,Ln,ModString}]}};
scan_module_1(_Ts, Ms) -> Ms.

scan_extends([{atom,Ln,A}=ModAtom,{')',_Lr}|_Ts], Ms0) ->
    ModString = atom_to_list(A),
    Ms = Ms0#{'BASE_MODULE':={none,[ModAtom]}},
    Ms#{'BASE_MODULE_STRING':={none,[{string,Ln,ModString}]}};
scan_extends(_Ts, Ms) -> Ms.

scan_err_warn([{'(',_}|_]=Toks0, {atom,_,Tag}=Token, From, St) ->
    try expand_macros(Toks0, St) of
	Toks when is_list(Toks) ->
	    case erl_parse:parse_term(Toks) of
		{ok,Term} ->
		    epp_reply(From, {Tag,{loc(Token),epp,{Tag,Term}}});
		{error,_} ->
		    epp_reply(From, {error,{loc(Token),epp,{bad,Tag}}})
	    end
    catch
	_:_ ->
	    epp_reply(From, {error,{loc(Token),epp,{bad,Tag}}})
    end,
    wait_req_scan(St);
scan_err_warn(_Toks, {atom,_,Tag}=Token, From, St) ->
    epp_reply(From, {error,{loc(Token),epp,{bad,Tag}}}),
    wait_req_scan(St).

%% scan_define(Tokens, DefineToken, From, EppState)

scan_define([{'(',_Lp},{Type,_Lm,_}=Mac|Toks], Def, From, St)
  when Type =:= atom; Type =:= var ->
    scan_define_1(Toks, Mac, Def, From, St);
scan_define(_Toks, Def, From, St) ->
    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
    wait_req_scan(St).

scan_define_1([{',',_}=Comma|Toks], Mac,_Def, From, St) ->
    case catch macro_expansion(Toks, Comma) of
        Expansion when is_list(Expansion) ->
	    scan_define_2(none, {none,Expansion}, Mac, From, St);
        {error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St)
    end;
scan_define_1([{'(',_Lc}|Toks], Mac, Def, From, St) ->
    case catch macro_pars(Toks, []) of
        {ok,{As,_}=MacroDef} ->
            Len = length(As),
	    scan_define_2(Len, MacroDef, Mac, From, St);
	{error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St);
        _ ->
            epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
            wait_req_scan(St)
    end;
scan_define_1(_Toks, _Mac, Def, From, St) ->
    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
    wait_req_scan(St).

scan_define_2(Arity, Def, {_,_,Key}=Mac, From, #epp{macs=Ms}=St) ->
    case Ms of
	#{Key:=Defs} when is_list(Defs) ->
	    %% User defined macros: can be overloaded
	    case proplists:is_defined(Arity, Defs) of
		true ->
		    epp_reply(From, {error,{loc(Mac),epp,{redefine,Key}}}),
		    wait_req_scan(St);
		false ->
		    scan_define_cont(From, St, Key, Defs, Arity, Def)
	    end;
	#{Key:=_} ->
	    %% Predefined macros: cannot be overloaded
	    epp_reply(From, {error,{loc(Mac),epp,{redefine_predef,Key}}}),
	    wait_req_scan(St);
	_ ->
	    scan_define_cont(From, St, Key, [], Arity, Def)
    end.

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(F, #epp{macs=Ms0}=St, M, Defs, Arity, Def) ->
    Ms = Ms0#{M=>[{Arity,Def}|Defs]},
    try macro_uses(Def) of
        U ->
	    Uses0 = St#epp.uses,
	    Val = [{Arity,U}|case Uses0 of
				 #{M:=UseList} -> UseList;
				 _ -> []
			     end],
	    Uses = Uses0#{M=>Val},
            scan_toks(F, St#epp{uses=Uses,macs=Ms})
    catch
        {error, Line, Reason} ->
            epp_reply(F, {error,{Line,epp,Reason}}),
            wait_req_scan(St)
    end.

macro_uses({_Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {'?', _} | Rest]) ->
    macro_ref(Rest);
macro_ref([{'?', _}, {atom, _, A}=Atom | Rest]) ->
    Lm = loc(Atom),
    Arity = count_args(Rest, Lm, A),
    [{A,Arity} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, _, A}=Var | Rest]) ->
    Lm = loc(Var),
    Arity = count_args(Rest, Lm, A),
    [{A,Arity} | macro_ref(Rest)];
macro_ref([_Token | Rest]) ->
    macro_ref(Rest).

%% scan_undef(Tokens, UndefToken, From, EppState)

scan_undef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From, St) ->
    Macs = maps:remove(M, St#epp.macs),
    Uses = maps:remove(M, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From,St) ->
    Macs = maps:remove(M, St#epp.macs),
    Uses = maps:remove(M, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef(_Toks, Undef, From, St) ->
    epp_reply(From, {error,{loc(Undef),epp,{bad,undef}}}),
    wait_req_scan(St).

%% scan_include(Tokens, IncludeToken, From, St)

scan_include(Tokens0, Inc, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_include1(Tokens, Inc, From, St).

scan_include1([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}], Inc,
              From, St) ->
    NewName = expand_var(NewName0),
    enter_file(NewName, Inc, From, St);
scan_include1(_Toks, Inc, From, St) ->
    epp_reply(From, {error,{loc(Inc),epp,{bad,include}}}),
    wait_req_scan(St).

%% scan_include_lib(Tokens, IncludeToken, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

expand_lib_dir(Name) ->
    try
	[App|Path] = filename:split(Name),
	LibDir = code:lib_dir(list_to_atom(App)),
	{ok,fname_join([LibDir|Path])}
    catch
	_:_ ->
	    error
    end.

scan_include_lib(Tokens0, Inc, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_include_lib1(Tokens, Inc, From, St).

scan_include_lib1([{'(',_Llp},{string,_Lf,_NewName0},{')',_Lrp},{dot,_Ld}],
                  Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{loc(Inc),epp,{depth,"include_lib"}}}),
    wait_req_scan(St);
scan_include_lib1([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}],
                  Inc, From, St) ->
    NewName = expand_var(NewName0),
    Loc = start_loc(St#epp.location),
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E1} ->
	    case expand_lib_dir(NewName) of
		{ok,Header} ->
		    case file:open(Header, [read]) of
			{ok,NewF} ->
			    wait_req_scan(enter_file2(NewF, Header, From,
                                                      St, Loc));
			{error,_E2} ->
			    epp_reply(From,
				      {error,{loc(Inc),epp,
                                              {include,lib,NewName}}}),
			    wait_req_scan(St)
		    end;
		error ->
		    epp_reply(From, {error,{loc(Inc),epp,
                                            {include,lib,NewName}}}),
		    wait_req_scan(St)
	    end
    end;
scan_include_lib1(_Toks, Inc, From, St) ->
    epp_reply(From, {error,{loc(Inc),epp,{bad,include_lib}}}),
    wait_req_scan(St).

%% scan_ifdef(Tokens, IfdefToken, From, EppState)
%% scan_ifndef(Tokens, IfdefToken, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, From, St) ->
    case St#epp.macs of
	#{M:=_Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	_ ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, From, St) ->
    case St#epp.macs of
	#{M:=_Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	_ ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef(_Toks, IfDef, From, St) ->
    epp_reply(From, {error,{loc(IfDef),epp,{bad,ifdef}}}),
    wait_req_skip(St, [ifdef]).

scan_ifndef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, From, St) ->
    case St#epp.macs of
	#{M:=_Def} ->
	    skip_toks(From, St, [ifndef]);
	_ ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, From, St) ->
    case St#epp.macs of
	#{M:=_Def} ->
	    skip_toks(From, St, [ifndef]);
	_ ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef(_Toks, IfnDef, From, St) ->
    epp_reply(From, {error,{loc(IfnDef),epp,{bad,ifndef}}}),
    wait_req_skip(St, [ifndef]).

%% scan_else(Tokens, ElseToken, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,_Ld}], Else, From, St) ->
    case St#epp.istk of
	['else'|Cis] ->
	    epp_reply(From, {error,{loc(Else),
                                    epp,{illegal,"repeated",'else'}}}),
	    wait_req_skip(St#epp{istk=Cis}, ['else']);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, ['else']);
	[] ->
	    epp_reply(From, {error,{loc(Else),epp,
                                    {illegal,"unbalanced",'else'}}}),
	    wait_req_scan(St)
    end;
scan_else(_Toks, Else, From, St) ->
    epp_reply(From, {error,{loc(Else),epp,{bad,'else'}}}),
    wait_req_scan(St).

%% scan_if(Tokens, IfToken, From, EppState)
%%  Handle the conditional parsing of a file.

scan_if([{'(',_}|_]=Toks, If, From, St) ->
    try eval_if(Toks, St) of
	true ->
	    scan_toks(From, St#epp{istk=['if'|St#epp.istk]});
	_ ->
	    skip_toks(From, St, ['if'])
    catch
	throw:Error0 ->
	    Error = case Error0 of
			{_,erl_parse,_} ->
			    {error,Error0};
			_ ->
			    {error,{loc(If),epp,Error0}}
		    end,
	    epp_reply(From, Error),
	    wait_req_skip(St, ['if'])
    end;
scan_if(_Toks, If, From, St) ->
    epp_reply(From, {error,{loc(If),epp,{bad,'if'}}}),
    wait_req_skip(St, ['if']).

eval_if(Toks0, St) ->
    Toks = expand_macros(Toks0, St),
    Es1 = case erl_parse:parse_exprs(Toks) of
	      {ok,Es0} -> Es0;
	      {error,E} -> throw(E)
	  end,
    Es = rewrite_expr(Es1, St),
    assert_guard_expr(Es),
    Bs = erl_eval:new_bindings(),
    LocalFun = fun(_Name, _Args) ->
		       error(badarg)
	       end,
    try erl_eval:exprs(Es, Bs, {value,LocalFun}) of
	{value,Res,_} ->
	    Res
    catch
	_:_ ->
	    false
    end.

assert_guard_expr([E0]) ->
    E = rewrite_expr(E0, none),
    case erl_lint:is_guard_expr(E) of
	false ->
	    throw({bad,'if'});
	true ->
	    ok
    end;
assert_guard_expr(_) ->
    throw({bad,'if'}).

%% Dual-purpose rewriting function. When the second argument is
%% an #epp{} record, calls to defined(Symbol) will be evaluated.
%% When the second argument is 'none', legal calls to our built-in
%% functions are eliminated in order to turn the expression into
%% a legal guard expression.

rewrite_expr({call,_,{atom,_,defined},[N0]}, #epp{macs=Macs}) ->
    %% Evaluate defined(Symbol).
    N = case N0 of
	    {var,_,N1} -> N1;
	    {atom,_,N1} -> N1;
	    _ -> throw({bad,'if'})
	end,
    {atom,0,maps:is_key(N, Macs)};
rewrite_expr({call,_,{atom,_,Name},As0}, none) ->
    As = rewrite_expr(As0, none),
    Arity = length(As),
    case erl_internal:bif(Name, Arity) andalso
	not erl_internal:guard_bif(Name, Arity) of
	false ->
	    %% A guard BIF, an -if built-in, or an unknown function.
	    %% Eliminate the call so that erl_lint will not complain.
	    %% The call might fail later at evaluation time.
	    to_conses(As);
	true ->
	    %% An auto-imported BIF (not guard BIF). Not allowed.
	    throw({bad,'if'})
    end;
rewrite_expr([H|T], St) ->
    [rewrite_expr(H, St)|rewrite_expr(T, St)];
rewrite_expr(Tuple, St) when is_tuple(Tuple) ->
    list_to_tuple(rewrite_expr(tuple_to_list(Tuple), St));
rewrite_expr(Other, _) ->
    Other.

to_conses([H|T]) ->
    {cons,0,H,to_conses(T)};
to_conses([]) ->
    {nil,0}.

%% scan_elif(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(_Toks, Elif, From, St) ->
    case St#epp.istk of
	['else'|Cis] ->
	    epp_reply(From, {error,{loc(Elif),
                                    epp,{illegal,"unbalanced",'elif'}}}),
	    wait_req_skip(St#epp{istk=Cis}, ['else']);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, ['elif']);
	[] ->
	    epp_reply(From, {error,{loc(Elif),epp,
                                    {illegal,"unbalanced",elif}}}),
	    wait_req_scan(St)
    end.

%% scan_endif(Tokens, EndifToken, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,_Ld}], Endif, From, St) ->
    case St#epp.istk of
	[_I|Cis] ->
	    scan_toks(From, St#epp{istk=Cis});
	[] ->
	    epp_reply(From, {error,{loc(Endif),epp,
                                    {illegal,"unbalanced",endif}}}),
	    wait_req_scan(St)
    end;
scan_endif(_Toks, Endif, From, St) ->
    epp_reply(From, {error,{loc(Endif),epp,{bad,endif}}}),
    wait_req_scan(St).

%% scan_file(Tokens, FileToken, From, EppState)
%%  Set the current file and line to the given file and line.
%%  Note that the line of the attribute itself is kept.

scan_file(Tokens0, Tf, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_file1(Tokens, Tf, From, St).

scan_file1([{'(',_Llp},{string,_Ls,Name},{',',_Lc},{integer,_Li,Ln},{')',_Lrp},
            {dot,_Ld}], Tf, From, St) ->
    Anno = erl_anno:new(Ln),
    enter_file_reply(From, Name, Anno, loc(Tf), generated),
    Ms0 = St#epp.macs,
    Ms = Ms0#{'FILE':={none,[{string,line1(),Name}]}},
    Locf = loc(Tf),
    NewLoc = new_location(Ln, St#epp.location, Locf),
    Delta = get_line(element(2, Tf))-Ln + St#epp.delta,
    wait_req_scan(St#epp{name2=Name,location=NewLoc,delta=Delta,macs=Ms});
scan_file1(_Toks, Tf, From, St) ->
    epp_reply(From, {error,{loc(Tf),epp,{bad,file}}}),
    wait_req_scan(St).

new_location(Ln, Le, Lf) when is_integer(Lf) ->
    Ln+(Le-Lf);
new_location(Ln, {Le,_}, {Lf,_}) ->
    {Ln+(Le-Lf),1}.

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(From, St, [I|Sis]) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.location) of
	{ok,[{'-',_Lh},{atom,_Li,ifdef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifdef,I|Sis]);
	{ok,[{'-',_Lh},{atom,_Li,ifndef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifndef,I|Sis]);
	{ok,[{'-',_Lh},{'if',_Li}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, ['if',I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,'else'}=Else|_Toks],Cl}->
	    skip_else(Else, From, St#epp{location=Cl}, [I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,'elif'}=Elif|Toks],Cl}->
	    skip_elif(Toks, Elif, From, St#epp{location=Cl}, [I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,endif}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, Sis);
	{ok,_Toks,Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [I|Sis]);
	{error,E,Cl} ->
	    case E of
		{_,file_io_server,invalid_unicode} ->
		    %% The compiler needs to know that there was
		    %% invalid unicode characters in the file
		    %% (and there is no point in continuing anyway
		    %% since io server process has terminated).
		    epp_reply(From, {error,E}),
		    leave_file(wait_request(St), St);
		_ ->
		    %% Some other invalid token, such as a bad floating
		    %% point number. Just ignore it.
		    skip_toks(From, St#epp{location=Cl}, [I|Sis])
	    end;
	{eof,Cl} ->
	    leave_file(From, St#epp{location=Cl,istk=[I|Sis]});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
	    leave_file(wait_request(St), St)	%This serious, just exit!
    end;
skip_toks(From, St, []) ->
    scan_toks(From, St).

skip_else(Else, From, St, ['else'|Sis]) ->
    epp_reply(From, {error,{loc(Else),epp,{illegal,"repeated",'else'}}}),
    wait_req_skip(St, ['else'|Sis]);
skip_else(_Else, From, St, ['elif'|Sis]) ->
    skip_toks(From, St, ['else'|Sis]);
skip_else(_Else, From, St, [_I]) ->
    scan_toks(From, St#epp{istk=['else'|St#epp.istk]});
skip_else(_Else, From, St, Sis) ->
    skip_toks(From, St, Sis).

skip_elif(_Toks, Elif, From, St, ['else'|_]=Sis) ->
    epp_reply(From, {error,{loc(Elif),epp,elif_after_else}}),
    wait_req_skip(St, Sis);
skip_elif(Toks, Elif, From, St, [_I]) ->
    scan_if(Toks, Elif, From, St);
skip_elif(_Toks, _Elif, From, St, Sis) ->
    skip_toks(From, St, Sis).

%% macro_pars(Tokens, ArgStack)
%% macro_expansion(Tokens, Anno)
%%  Extract the macro parameters and the expansion from a macro definition.

macro_pars([{')',_Lp}, {',',_Ld}=Comma|Ex], Args) ->
    {ok, {lists:reverse(Args), macro_expansion(Ex, Comma)}};
macro_pars([{var,_,Name}, {')',_Lp}, {',',_Ld}=Comma|Ex], Args) ->
    false = lists:member(Name, Args),		%Prolog is nice
    {ok, {lists:reverse([Name|Args]), macro_expansion(Ex, Comma)}};
macro_pars([{var,_L,Name}, {',',_}|Ts], Args) ->
    false = lists:member(Name, Args),
    macro_pars(Ts, [Name|Args]).

macro_expansion([{')',_Lp},{dot,_Ld}], _T0) -> [];
macro_expansion([{dot,_}=Dot], _T0) ->
    throw({error,loc(Dot),missing_parenthesis});
macro_expansion([T|Ts], _T0) ->
    [T|macro_expansion(Ts, T)];
macro_expansion([], T0) -> throw({error,loc(T0),premature_end}).

%% expand_macros(Tokens, St)
%% expand_macro(Tokens, MacroToken, RestTokens)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same location as the macro call.

expand_macros(MacT, M, Toks, St) ->
    #epp{macs=Ms,uses=U} = St,
    Lm = loc(MacT),
    Tinfo = element(2, MacT),
    case expand_macro1(Lm, M, Toks, Ms) of
	{ok,{none,Exp}} ->
	    check_uses([{M,none}], [], U, Lm),
	    Toks1 = expand_macros(expand_macro(Exp, Tinfo, [], #{}), St),
	    expand_macros(Toks1++Toks, St);
	{ok,{As,Exp}} ->
	    check_uses([{M,length(As)}], [], U, Lm),
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, #{}),
	    expand_macros(expand_macro(Exp, Tinfo, Toks1, Bs), St)
    end.

expand_macro1(Lm, M, Toks, Ms) ->
    Arity = count_args(Toks, Lm, M),
    case Ms of
	#{M:=undefined} ->
	    %% Predefined macro without definition.
            throw({error,Lm,{undefined,M,Arity}});
	#{M:=[{none,Def}]} ->
            {ok,Def};
	#{M:=Defs} when is_list(Defs) ->
	    case proplists:get_value(Arity, Defs) of
                undefined ->
                    throw({error,Lm,{mismatch,M}});
                Def ->
                    {ok,Def}
            end;
        #{M:=PreDef} ->
	    %% Predefined macro.
            {ok,PreDef};
        _ ->
	    %% Macro not found.
            throw({error,Lm,{undefined,M,Arity}})
    end.

check_uses([], _Anc, _U, _Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {Name,Arity} = M,
	    throw({error,Lm,{circular,Name,Arity}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.

get_macro_uses({M,Arity}, U) ->
    case U of
	#{M:=L} ->
	    proplists:get_value(Arity, L, proplists:get_value(none, L, []));
	_ ->
	    []
    end.

%% Macro expansion
%% Note: io:scan_erl_form() does not return comments or white spaces.
expand_macros([{'?',_Lq},{atom,_Lm,M}=MacT|Toks], St) ->
    expand_macros(MacT, M, Toks, St);
%% Special macros
expand_macros([{'?',_Lq},{var,Lm,'FUNCTION_NAME'}=Token|Toks], St0) ->
    St = update_fun_name(Token, St0),
    case St#epp.fname of
	undefined ->
	    [{'?',_Lq},Token];
	{Name,_} ->
	    [{atom,Lm,Name}]
    end ++ expand_macros(Toks, St);
expand_macros([{'?',_Lq},{var,Lm,'FUNCTION_ARITY'}=Token|Toks], St0) ->
    St = update_fun_name(Token, St0),
    case St#epp.fname of
	undefined ->
	    [{'?',_Lq},Token];
	{_,Arity} ->
	    [{integer,Lm,Arity}]
    end ++ expand_macros(Toks, St);
expand_macros([{'?',_Lq},{var,Lm,'LINE'}=Tok|Toks], St) ->
    Line = erl_scan:line(Tok),
    [{integer,Lm,Line}|expand_macros(Toks, St)];
expand_macros([{'?',_Lq},{var,_Lm,M}=MacT|Toks], St) ->
    expand_macros(MacT, M, Toks, St);
%% Illegal macros
expand_macros([{'?',_Lq},Token|_Toks], _St) ->
    T = case erl_scan:text(Token) of
            Text when is_list(Text) ->
                Text;
            undefined ->
                Symbol = erl_scan:symbol(Token),
                io_lib:fwrite(<<"~tp">>, [Symbol])
        end,
    throw({error,loc(Token),{call,[$?|T]}});
expand_macros([T|Ts], St) ->
    [T|expand_macros(Ts, St)];
expand_macros([], _St) -> [].

%% bind_args(Tokens, MacroLocation, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call.

bind_args([{'(',_Llp},{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',_Llp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
bind_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

macro_args([{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',_Lc}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
macro_args([], Lm, M, _As, _Bs) ->
    throw({error,Lm,{arg_error,M}}); % Cannot happen.
macro_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

store_arg(L, M, _A, [], _Bs) ->
    throw({error,L,{mismatch,M}});
store_arg(_L, _M, A, Arg, Bs) ->
    Bs#{A=>Arg}.

%% count_args(Tokens, MacroLine, MacroName)
%%  Count the number of arguments in a macro call.
count_args([{'(', _Llp},{')',_Lrp}|_Toks], _Lm, _M) ->
    0;
count_args([{'(', _Llp},{',',_Lc}|_Toks], Lm, M) ->
    throw({error,Lm,{arg_error,M}});
count_args([{'(',_Llp}|Toks0], Lm, M) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, 1);
count_args(_Toks, _Lm, _M) ->
    none.

count_args([{')',_Lrp}|_Toks], _Lm, _M, NbArgs) ->
    NbArgs;
count_args([{',',_Lc},{')',_Lrp}|_Toks], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args([{',',_Lc}|Toks0], Lm, M, NbArgs) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, NbArgs+1);
count_args([], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args(_Toks, Lm, M, _NbArgs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".

macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'fun',Lc}|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'fun',Lc}|Arg]);
macro_arg([{'fun',_}=Fun,{var,_,_}=Name|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [Name,Fun|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{'try',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'try',Lr}|Arg]);
macro_arg([{'cond',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'cond',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], _E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroTokenInfo, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, MacroLocation, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the location of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,_Lv,V}|Ts], L, Rest, Bs) ->
    case Bs of
	#{V:=Val} ->
	    expand_arg(Val, Ts, L, Rest, Bs);
	_ ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,_Lv,V}|Ts], L, Rest, Bs) ->
    case Bs of
	#{V:=Val} ->
            expand_arg(stringify(Val, L), Ts, L, Rest, Bs);
	_ ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([T|Ts], L, Rest, Bs) ->
    [setelement(2, T, L)|expand_macro(Ts, L, Rest, Bs)];
expand_macro([], _L, Rest, _Bs) -> Rest.

expand_arg([A|As], Ts, _L, Rest, Bs) ->
    %% It is not obvious that the location of arguments should replace L.
    NextL = element(2, A),
    [A|expand_arg(As, Ts, NextL, Rest, Bs)];
expand_arg([], Ts, L, Rest, Bs) ->
    expand_macro(Ts, L, Rest, Bs).

%%%
%%% Here follows support for the ?FUNCTION_NAME and ?FUNCTION_ARITY
%%% macros. Since the parser has not been run yet, we don't know the
%%% name and arity of the current function. Therefore, we will need to
%%% scan the beginning of the current form to extract the name and
%%% arity of the function.
%%%

update_fun_name(Token, #epp{fname=Toks0}=St) when is_list(Toks0) ->
    %% ?FUNCTION_NAME or ?FUNCTION_ARITY is used for the first time in
    %% a function.  First expand macros (except ?FUNCTION_NAME and
    %% ?FUNCTION_ARITY) in the form.

    Toks1 = (catch expand_macros(Toks0, St#epp{fname=undefined})),

    %% Now extract the name and arity from the stream of tokens, and store
    %% the result in the #epp{} record so we don't have to do it
    %% again.

    case Toks1 of
	[{atom,_,Name},{'(',_}|Toks] ->
	    %% This is the beginning of a function definition.
	    %% Scan the token stream up to the matching right
	    %% parenthesis and count the number of arguments.
	    FA = update_fun_name_1(Toks, 1, {Name,0}, St),
	    St#epp{fname=FA};
	[{'?',_}|_] ->
	    %% ?FUNCTION_NAME/?FUNCTION_ARITY used at the beginning
	    %% of a form. Does not make sense.
	    {var,_,Macro} = Token,
	    throw({error,loc(Token),{illegal_function_usage,Macro}});
	_ when is_list(Toks1) ->
	    %% Not the beginning of a function (an attribute or a
	    %% syntax error).
	    {var,_,Macro} = Token,
	    throw({error,loc(Token),{illegal_function,Macro}});
	_ ->
	    %% A macro expansion error. Return a dummy value and
	    %% let the caller notice and handle the error.
	    St#epp{fname={'_',0}}
    end;
update_fun_name(_Token, St) ->
    St.

update_fun_name_1([Tok|Toks], L, FA, St) ->
    case classify_token(Tok) of
	comma ->
	    if
		L =:= 1 ->
		    {Name,Arity} = FA,
		    update_fun_name_1(Toks, L, {Name,Arity+1}, St);
		true ->
		    update_fun_name_1(Toks, L, FA, St)
	    end;
	left ->
	    update_fun_name_1(Toks, L+1, FA, St);
	right when L =:= 1 ->
	    FA;
	right ->
	    update_fun_name_1(Toks, L-1, FA, St);
	other ->
	    case FA of
		{Name,0} ->
		    update_fun_name_1(Toks, L, {Name,1}, St);
		{_,_} ->
		    update_fun_name_1(Toks, L, FA, St)
	    end
    end;
update_fun_name_1([], _, FA, _) ->
    %% Syntax error, but never mind.
    FA.

classify_token({C,_}) -> classify_token_1(C);
classify_token(_) -> other.

classify_token_1(',') -> comma;
classify_token_1('(') -> left;
classify_token_1('{') -> left;
classify_token_1('[') -> left;
classify_token_1('<<') -> left;
classify_token_1(')') -> right;
classify_token_1('}') -> right;
classify_token_1(']') -> right;
classify_token_1('>>') -> right;
classify_token_1(_) -> other.


%%% stringify(Ts, L) returns a list of one token: a string which when
%%% tokenized would yield the token list Ts.

%% erl_scan:text(T) is not backward compatible with this.
%% Note that escaped characters will be replaced by themselves.
token_src({dot, _}) ->
    ".";
token_src({X, _}) when is_atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({char,_,C}) ->
    io_lib:write_char(C);
token_src({string, _, X}) ->
    io_lib:write_string(X);
token_src({_, _, X}) ->
    io_lib:format("~w", [X]).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~ts", [token_src(T)]) | stringify1(Tokens)].

stringify(Ts, L) ->
    [$\s | S] = lists:flatten(stringify1(Ts)),
    [{string, L, S}].

coalesce_strings([{string,A,S} | Tokens]) ->
    coalesce_strings(Tokens, A, [S]);
coalesce_strings([T | Tokens]) ->
    [T | coalesce_strings(Tokens)];
coalesce_strings([]) ->
    [].

coalesce_strings([{string,_,S}|Tokens], A, S0) ->
    coalesce_strings(Tokens, A, [S | S0]);
coalesce_strings(Tokens, A, S) ->
    [{string,A,lists:append(lists:reverse(S))} | coalesce_strings(Tokens)].

%% epp_request(Epp)
%% epp_request(Epp, Request)
%% epp_reply(From, Reply)
%%  Handle communication with the epp.

epp_request(Epp) ->
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_request(Epp, Req) ->
    Epp ! {epp_request,self(),Req},
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_reply(From, Rep) ->
    From ! {epp_reply,self(),Rep},
    ok.

wait_epp_reply(Epp, Mref) ->
    receive
	{epp_reply,Epp,Rep} ->
	    erlang:demonitor(Mref, [flush]),
	    Rep;
	{'DOWN',Mref,_,_,E} ->
	    receive {epp_reply,Epp,Rep} -> Rep
	    after 0 -> exit(E)
	    end
    end.

expand_var([$$ | _] = NewName) ->
    case catch expand_var1(NewName) of
	{ok, ExpName} ->
	    ExpName;
	_ ->
	    NewName
    end;
expand_var(NewName) ->
    NewName.

expand_var1(NewName) ->
    [[$$ | Var] | Rest] = filename:split(NewName),
    Value = os:getenv(Var),
    true = Value =/= false,
    {ok, fname_join([Value | Rest])}.

fname_join(["." | [_|_]=Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).

%% The line only. (Other tokens may have the column and text as well...)
loc_anno(Line) when is_integer(Line) ->
    erl_anno:new(Line);
loc_anno({Line,_Column}) ->
    erl_anno:new(Line).

loc(Token) ->
    erl_scan:location(Token).

add_line(Line, Offset) when is_integer(Line) ->
    Line+Offset;
add_line({Line, Column}, Offset) ->
    {Line+Offset, Column}.

start_loc(Line) when is_integer(Line) ->
    1;
start_loc({_Line, _Column}) ->
    {1, 1}.

line1() ->
    erl_anno:new(1).

get_line(Anno) ->
    erl_anno:line(Anno).

%% epp has always output -file attributes when entering and leaving
%% included files (-include, -include_lib). Starting with R11B the
%% -file attribute is also recognized in the input file. This is
%% mainly aimed at yecc, the parser generator, which uses the -file
%% attribute to get correct lines in messages referring to code
%% supplied by the user (actions etc in .yrl files).
%%
%% In a perfect world (read: perfectly implemented applications such
%% as Xref, Cover, Debugger, etc.) it would not be necessary to
%% distinguish -file attributes from epp and the input file. The
%% Debugger for example could have one window for each referred file,
%% each window with its own set of breakpoints etc. The line numbers
%% of the abstract code would then point into different windows
%% depending on the -file attribute. [Note that if, as is the case for
%% yecc, code has been copied into the file, then it is possible that
%% the copied code differ from the one referred to by the -file
%% attribute, which means that line numbers can mismatch.] In practice
%% however it is very rare with Erlang functions in included files, so
%% only one window is used per module. This means that the line
%% numbers of the abstract code have to be adjusted to refer to the
%% top-most source file. The function interpret_file_attributes/1
%% below interprets the -file attribute and returns forms where line
%% numbers refer to the top-most file. The -file attribute forms that
%% have been output by epp (corresponding to -include and
%% -include_lib) are kept, but the user's -file attributes are
%% removed. This seems sufficient for now.
%%
%% It turns out to be difficult to distinguish -file attributes in the
%% input file from the ones added by epp unless some action is taken.
%% The (less than perfect) solution employed is to let epp assign
%% negative line numbers to user supplied -file attributes.

%% Note: it is assumed that the second element is a line or a key-list
%% where 'line' can be found.

interpret_file_attribute(Forms) ->
    interpret_file_attr(Forms, 0, []).

interpret_file_attr([{attribute,Anno,file,{File,Line}}=Form | Forms],
                    Delta, Fs) ->
    L = get_line(Anno),
    Generated = erl_anno:generated(Anno),
    if
        Generated ->
            %% -file attribute
            interpret_file_attr(Forms, (L + Delta) - Line, Fs);
        not Generated ->
            %% -include or -include_lib
            % true = L =:= Line,
            case Fs of
                [_, File | Fs1] -> % end of included file
                    [Form | interpret_file_attr(Forms, 0, [File | Fs1])];
                _ -> % start of included file
                    [Form | interpret_file_attr(Forms, 0, [File | Fs])]
            end
    end;
interpret_file_attr([Form0 | Forms], Delta, Fs) ->
    F = fun(Anno) ->
                Line = erl_anno:line(Anno),
                erl_anno:set_line(Line + Delta, Anno)
        end,
    Form = erl_parse:map_anno(F, Form0),
    [Form | interpret_file_attr(Forms, Delta, Fs)];
interpret_file_attr([], _Delta, _Fs) ->
    [].

