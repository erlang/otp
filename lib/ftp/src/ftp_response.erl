%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
%%
%% Description: This module impements handling of ftp server responses.

-module(ftp_response).

%% Internal API
-export([parse_lines/3, interpret/1, error_string/1]).

-include("ftp_internal.hrl").

%% First group of reply code digits 
-define(POS_PREL, 1).
-define(POS_COMPL, 2).
-define(POS_INTERM, 3).
-define(TRANS_NEG_COMPL, 4).
-define(PERM_NEG_COMPL, 5).
%% Second group of reply code digits
-define(SYNTAX,0).
-define(INFORMATION,1).
-define(CONNECTION,2).
-define(AUTH_ACC,3).
-define(UNSPEC,4).
-define(FILE_SYSTEM,5).

%%%=========================================================================
%%%  INTERNAL API 
%%%=========================================================================

%%--------------------------------------------------------------------------
%% parse_lines(Data, AccLines, StatusCode)  -> {ok, Lines} | 
%%                                             {continue, {Data, 
%%                                                       AccLines, StatusCode}}
%%
%% Data = binary() - data recived on the control connection from the 
%%                   ftp-server. 
%% AccLines = [string()] 
%% StatusCode     =  start | {byte(), byte(), byte()} | finish -
%%                   Indicates where in the parsing process we are.
%%                   start - (looking for the status code of the message)
%%                   {byte(), byte(), byte()} - status code found, now
%%                   looking for the last line indication.
%%                   finish - now on the last line.
%% Description: Parses a ftp control response message. 
%%      "A reply is defined to contain the 3-digit code, followed by Space
%%      <SP>, followed by one line of text (where some maximum line length
%%      has been specified), and terminated by the Telnet end-of-line
%%      code (CRLF), or a so called multilined reply for example:
%%
%%                                123-First line
%%                                Second line
%%                                  234 A line beginning with numbers
%%                                123 The last line
%%
%%         The user-process then simply needs to search for the second
%%         occurrence of the same reply code, followed by <SP> (Space), at
%%         the beginning of a line, and ignore all intermediary lines.  If
%%         an intermediary line begins with a 3-digit number, the Server
%%         will pad the front to avoid confusion.
%%--------------------------------------------------------------------------

%% Make sure we received the first 4 bytes so we know how to parse
%% the FTP server response e.i. is the response composed of one
%% or multiple lines.
parse_lines(Bin, Lines, start) when size(Bin) < 4 ->
    {continue, {Bin, Lines, start}};
%% Multiple lines exist
parse_lines(<<C1, C2, C3, $-, Rest/binary>>, Lines, start) ->
    parse_lines(Rest, [$-, C3, C2, C1 | Lines], {C1, C2, C3});
%% Only one line exists
parse_lines(<<C1, C2, C3, ?WHITE_SPACE, Bin/binary>>, Lines, start) ->
    parse_lines(Bin, [?WHITE_SPACE, C3, C2, C1 | Lines], finish);

%% Last line found
parse_lines(<<?CR, ?LF, C1, C2, C3, ?WHITE_SPACE, Rest/binary>>, Lines, {C1, C2, C3}) ->
    parse_lines(Rest, [?WHITE_SPACE, C3, C2, C1, ?LF, ?CR | Lines], finish);
%% Potential end found  wait for more data 
parse_lines(<<?CR, ?LF, C1, C2, C3>> = Bin, Lines, {C1, C2, C3}) ->
    {continue, {Bin, Lines, {C1, C2, C3}}};
%% Intermidate line begining with status code
parse_lines(<<?CR, ?LF, C1, C2, C3, Rest/binary>>, Lines, {C1, C2, C3}) ->
    parse_lines(Rest, [C3, C2, C1, ?LF, ?CR  | Lines], {C1, C2, C3});

%% Potential last line wait for more data
parse_lines(<<?CR, ?LF, C1, C2>> = Data, Lines, {C1, C2, _} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_lines(<<?CR, ?LF, C1>> = Data, Lines, {C1, _, _} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_lines(<<?CR, ?LF>> = Data, Lines, {_,_,_} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_lines(<<?LF>> = Data, Lines, {_,_,_} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_lines(<<>> = Data, Lines, {_,_,_} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
%% Part of the multiple lines
parse_lines(<<Octet, Rest/binary>>, Lines, {_,_, _} = StatusCode) ->
    parse_lines(Rest, [Octet | Lines], StatusCode);

%% End of FTP server response found
parse_lines(<<?CR, ?LF>>, Lines, finish) ->
    {ok, lists:reverse([?LF, ?CR | Lines]), <<>>}; 
parse_lines(<<?CR, ?LF, Rest/binary>>, Lines, finish) ->
    {ok, lists:reverse([?LF, ?CR | Lines]), Rest}; 

%% Potential end found  wait for more data 
parse_lines(<<?CR>> = Data, Lines, finish) ->
    {continue, {Data, Lines, finish}};
parse_lines(<<>> = Data, Lines, finish) ->
    {continue, {Data, Lines, finish}};
%% Part of last line
parse_lines(<<Octet, Rest/binary>>, Lines, finish) ->
    parse_lines(Rest, [Octet | Lines], finish).

%%--------------------------------------------------------------------------
%% interpret(Lines) ->  {Status, Text} 
%%	Lines  = [byte(), byte(), byte() | Text] - ftp server response as
%%      returned by parse_lines/3
%%	Stauts = atom() (see interpret_status/3)
%%      Text = [string()]
%%
%% Description: Create nicer data to match on.
%%--------------------------------------------------------------------------
interpret([Didgit1, Didgit2, Didgit3 | Data]) ->
    Code1 = Didgit1 - $0,
    Code2 = Didgit2 - $0,
    Code3 = Didgit3 - $0,
    {interpret_status(Code1, Code2, Code3), Data}.

%%--------------------------------------------------------------------------
%% error_string(Error) -> string()
%%	Error =  {error, term()} | term()
%%
%% Description: Translates error codes into strings intended for
%% human interpretation.
%%--------------------------------------------------------------------------
error_string({error, Reason}) ->
    error_string(Reason);

error_string(echunk) -> "Synchronisation error during chunk sending.";
error_string(eclosed) -> "Session has been closed.";
error_string(econn) ->  "Connection to remote server prematurely closed.";
error_string(eexists) ->"File or directory already exists.";
error_string(ehost) ->  "Host not found, FTP server not found, "
		      "or connection rejected.";
error_string(elogin) -> "User not logged in.";
error_string(enotbinary) -> "Term is not a binary.";
error_string(epath) ->  "No such file or directory, already exists, "
		      "or permission denied.";
error_string(etype) ->  "No such type.";
error_string(euser) ->  "User name or password not valid.";
error_string(etnospc) -> "Insufficient storage space in system.";
error_string(enofile) -> "No files found or file unavailable";
error_string(epnospc) -> "Exceeded storage allocation "
		       "(for current directory or dataset).";
error_string(efnamena) -> "File name not allowed.";
error_string(Reason) -> 
    lists:flatten(io_lib:format("Unknown error: ~w", [Reason])).

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% Positive Preleminary Reply
interpret_status(?POS_PREL,_,_)                   -> pos_prel; 
%%FIXME ??? 3??? interpret_status(?POS_COMPL, ?AUTH_ACC, 3)        -> tls_upgrade; 
interpret_status(?POS_COMPL, ?AUTH_ACC, 4)        -> tls_upgrade; 
%% Positive Completion Reply
interpret_status(?POS_COMPL,_,_)                  -> pos_compl;
%% Positive Intermediate Reply nedd account
interpret_status(?POS_INTERM,?AUTH_ACC,2)         -> pos_interm_acct;
%% Positive Intermediate Reply
interpret_status(?POS_INTERM,_,_)                 -> pos_interm; 
%% No files found or file not available
interpret_status(?TRANS_NEG_COMPL,?FILE_SYSTEM,0) -> enofile;
%% No storage area no action taken
interpret_status(?TRANS_NEG_COMPL,?FILE_SYSTEM,2) -> etnospc;
%% Temporary Error, no action taken
interpret_status(?TRANS_NEG_COMPL,_,_)            -> trans_neg_compl;
%% Permanent disk space error, the user shall not try again
interpret_status(?PERM_NEG_COMPL,?FILE_SYSTEM,0)  -> epath;
interpret_status(?PERM_NEG_COMPL,?FILE_SYSTEM,2)  -> epnospc;
interpret_status(?PERM_NEG_COMPL,?FILE_SYSTEM,3)  -> efnamena; 
interpret_status(?PERM_NEG_COMPL,?AUTH_ACC,0)     -> elogin; 
interpret_status(?PERM_NEG_COMPL,_,_)             -> perm_neg_compl.

