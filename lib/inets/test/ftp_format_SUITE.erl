%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(ftp_format_SUITE).
-author('ingela@erix.ericsson.se').

-include_lib("common_test/include/ct.hrl").
-include("ftp_internal.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

suite() -> 
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,5}}
    ].

all() -> 
    [{group, ftp_response}, format_error].

groups() -> 
    [{ftp_response, [],
      [ftp_150, ftp_200, ftp_220, ftp_226, ftp_257, ftp_331,
       ftp_425, ftp_other_status_codes, ftp_multiple_lines_status_in_msg,
       ftp_multiple_lines, ftp_multipel_ctrl_messages]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_, Config) ->
    Config.
end_per_testcase(_, _) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

ftp_150() ->
    [{doc, "Especially check that respons can be devided in a random place."}].
ftp_150(Config) when is_list(Config) ->
    FtpResponse = ["150 ASCII data conn", "ection for /bin/ls ",
		   "(134.138.177", ".89,50434) (0 bytes).\r\n"],
    
    "150 ASCII data connection for /bin/ls "
	"(134.138.177.89,50434) (0 bytes).\r\n" = Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_prel, _} = ftp_response:interpret(Msg).
   
ftp_200() ->
    [{doc, "Especially check that respons can be devided after the first status "
    "code character and in the end delimiter."}].
ftp_200(Config) when is_list(Config) ->
    FtpResponse = ["2", "00 PORT command successful.", [?CR], [?LF]], 
    
    "200 PORT command successful.\r\n" =  Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_compl, _} = ftp_response:interpret(Msg),
    ok.

ftp_220() ->
    [{doc, "Especially check that respons can be devided after the "
     "first with space "}].
ftp_220(Config) when is_list(Config) ->
    FtpResponse = ["220 ","fingon FTP server (SunOS 5.8) ready.\r\n"], 
    
    "220 fingon FTP server (SunOS 5.8) ready.\r\n" = Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_compl, _} = ftp_response:interpret(Msg),
    ok.

ftp_226() ->
    [{doc, "Especially check that respons can be devided after second status code"
    " character and in the end delimiter."}].
ftp_226(Config) when is_list(Config) ->
    FtpResponse = ["22" "6 Transfer complete.\r", [?LF]],
    
    "226 Transfer complete.\r\n"  = Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_compl, _} = ftp_response:interpret(Msg),
    ok.

ftp_257() ->
    [{doc, "Especially check that quoted chars do not cause a problem."}].
ftp_257(Config) when is_list(Config) ->
    FtpResponse = ["257 \"/\" is current directory.\r\n"], 
    
    "257 \"/\" is current directory.\r\n" = Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_compl, _} = ftp_response:interpret(Msg),
    ok.

ftp_331() ->
    [{doc, "Especially check that respons can be devided after the third status "
    " status code character."}].
ftp_331(Config) when is_list(Config) ->
    %% Brake before white space after code
    FtpResponse = 
	["331"," Guest login ok, send ient as password.\r\n"],
    
    "331 Guest login ok, send ient as password.\r\n" = Msg =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_interm, _} = ftp_response:interpret(Msg),
    ok.

ftp_425() ->
    [{doc, "Especially check a message that was received in only one part."}].
ftp_425(Config) when is_list(Config) ->
    FtpResponse = 
	["425 Can't build data connection: Connection refused.\r\n"],

    "425 Can't build data connection: Connection refused.\r\n" 
	= Msg = parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {trans_neg_compl, _} = ftp_response:interpret(Msg),
    ok.

ftp_multiple_lines_status_in_msg() ->
    [{doc, "check that multiple lines gets parsed correct, even if we have "
      " the status code within the msg being sent"}].
ftp_multiple_lines_status_in_msg(Config) when is_list(Config) ->
    ML = "230-User usr-230 is logged in\r\n" ++
        "230 OK. Current directory is /\r\n",
    {ok, ML, <<>>} = ftp_response:parse_lines(list_to_binary(ML), [], start),
    ok.

ftp_multiple_lines() ->
    [{doc, "Especially check multiple lines devided in significant places"}].
ftp_multiple_lines(Config) when is_list(Config) ->
    FtpResponse =   ["21", "4","-The",
		     " following commands are recognized:\r\n"
		     "   USER    EPRT    STRU    MAIL*   ALLO    CWD",
		     "     STAT*   XRMD \r\n"
		     "   PASS    LPRT    MODE    MSND* "
		     "  REST*   XCWD    HELP    PWD  ", [?CRLF],
		     "   ACCT*   EPSV    RETR    MSOM*   RNFR    LIST  "
		     "  NOOP    XPWD \r\n",
		     "   REIN*   LPSV    STOR    MSAM*   RNTO    NLST "
		     "   MKD     CDUP \r\n"
		     "   QUIT    PASV    APPE    MRSQ*   ABOR    SITE* "
		     "  XMKD    XCUP \r\n"
		     "   PORT    TYPE    MLFL*   MRCP*   DELE    SYST "
		     "   RMD     STOU \r\n"
		     "214 (*'s => unimplemented)", [?CR], [?LF]],


    FtpResponse1 =   ["214-", "The",
		      " following commands are recognized:\r\n"
		      "   USER    EPRT    STRU    MAIL*   ALLO    CWD",
		      "     STAT*   XRMD \r\n"
		      "   PASS    LPRT    MODE    MSND* "
		      "  REST*   XCWD    HELP    PWD  ", [?CRLF],
		      "   ACCT*   EPSV    RETR    MSOM*   RNFR    LIST  "
		      "  NOOP    XPWD \r\n",
		      "   REIN*   LPSV    STOR    MSAM*   RNTO    NLST "
		      "   MKD     CDUP \r\n"
		      "   QUIT    PASV    APPE    MRSQ*   ABOR    SITE* "
		      "  XMKD    XCUP \r\n"
			 "   PORT    TYPE    MLFL*   MRCP*   DELE    SYST "
		      "   RMD     STOU \r\n"
		      "2", "14 (*'s => unimplemented)", [?CR], [?LF]],
    
    FtpResponse2 =   ["214-", "The",
		      " following commands are recognized:\r\n"
		      "   USER    EPRT    STRU    MAIL*   ALLO    CWD",
		      "     STAT*   XRMD \r\n"
		      "   PASS    LPRT    MODE    MSND* "
		      "  REST*   XCWD    HELP    PWD  ", [?CRLF],
		      "   ACCT*   EPSV    RETR    MSOM*   RNFR    LIST  "
		      "  NOOP    XPWD \r\n",
		      "   REIN*   LPSV    STOR    MSAM*   RNTO    NLST "
		      "   MKD     CDUP \r\n"
		      "   QUIT    PASV    APPE    MRSQ*   ABOR    SITE* "
		      "  XMKD    XCUP \r\n"
		      "   PORT    TYPE    MLFL*   MRCP*   DELE    SYST "
		      "   RMD     STOU \r\n"
		      "21", "4"," (*'s => unimplemented)", [?CR], [?LF]],
    
    MultiLineResultStr = 
	"214-The following commands are recognized:\r\n"
	"   USER    EPRT    STRU    MAIL*   ALLO    CWD     STAT*   "
	"XRMD \r\n"
	"   PASS    LPRT    MODE    MSND*   REST*   XCWD    HELP    "
	"PWD  \r\n"
	"   ACCT*   EPSV    RETR    MSOM*   RNFR    LIST    NOOP    "
	"XPWD \r\n"
	"   REIN*   LPSV    STOR    MSAM*   RNTO    NLST    MKD     "
	"CDUP \r\n"
	"   QUIT    PASV    APPE    MRSQ*   ABOR    SITE*   XMKD    "
	"XCUP \r\n"
	"   PORT    TYPE    MLFL*   MRCP*   DELE    SYST    RMD     "
	"STOU \r\n"
	"214 (*'s => unimplemented)\r\n", 
    
    MultiLineResultStr =
	parse(ftp_response, parse_lines, [[], start], FtpResponse), 
    {pos_compl, _} = ftp_response:interpret(MultiLineResultStr),
    
    MultiLineResultStr = parse(ftp_response, parse_lines, [[], start], 
			       FtpResponse1),
    
    MultiLineResultStr = parse(ftp_response, parse_lines, [[], start], 
			       FtpResponse2),
    ok.

ftp_other_status_codes() ->
    [{doc, "Check that other valid status codes, than the ones above, are handled"
     "by ftp_response:interpret/1. Note there are som ftp status codes" 
     "that will not be received with the current ftp instruction support," 
     "they are not included here."}].
ftp_other_status_codes(Config) when is_list(Config) ->

    %% 1XX
    {pos_prel, _ }  = ftp_response:interpret("120 Foobar\r\n"),
      
    %% 2XX
    {pos_compl, _ }  = ftp_response:interpret("202 Foobar\r\n"),
    {pos_compl, _ }  = ftp_response:interpret("221 Foobar\r\n"),
    {pos_compl, _ }  = ftp_response:interpret("227 Foobar\r\n"),
    {pos_compl, _ }  = ftp_response:interpret("230 Foobar\r\n"),
    {pos_compl, _ }  = ftp_response:interpret("250 Foobar\r\n"),
    
    %% 3XX
    {pos_interm_acct, _ }  = ftp_response:interpret("332 Foobar\r\n"),
    {pos_interm, _ }  = ftp_response:interpret("350 Foobar\r\n"),

    %% 4XX
    {trans_neg_compl, _ }  = ftp_response:interpret("421 Foobar\r\n"),
    {trans_neg_compl, _ }  = ftp_response:interpret("426 Foobar\r\n"),
    {enofile, _ }  = ftp_response:interpret("450 Foobar\r\n"),
    {trans_neg_compl, _ }  = ftp_response:interpret("451 Foobar\r\n"),
    {etnospc, _ }  = ftp_response:interpret("452 Foobar\r\n"),

    %% 5XX
    {perm_neg_compl, _ }  = ftp_response:interpret("500 Foobar\r\n"),
    {perm_neg_compl, _ }  = ftp_response:interpret("501 Foobar\r\n"),
    {perm_neg_compl, _ }  = ftp_response:interpret("503 Foobar\r\n"),
    {perm_neg_compl, _ }  = ftp_response:interpret("504 Foobar\r\n"),
    {elogin, _ } = ftp_response:interpret("530 Foobar\r\n"),
    {perm_neg_compl, _ }  = ftp_response:interpret("532 Foobar\r\n"),
    {epath, _ }  = ftp_response:interpret("550 Foobar\r\n"),
    {epnospc, _ }  = ftp_response:interpret("552 Foobar\r\n"),
    {efnamena, _ }  = ftp_response:interpret("553 Foobar\r\n"),
    ok.
       
ftp_multipel_ctrl_messages() ->
    [{doc, "The ftp server may send more than one control message as a reply," 
     "check that they are handled one at the time."}].
ftp_multipel_ctrl_messages(Config) when is_list(Config) ->
    FtpResponse = ["200 PORT command successful.\r\n200 Foobar\r\n"], 
    
    {"200 PORT command successful.\r\n" = Msg, NextMsg} =
	parse(ftp_response, parse_lines, [[], start], FtpResponse),
    {pos_compl, _} = ftp_response:interpret(Msg),
    NewMsg = parse(ftp_response, parse_lines, [[], start], NextMsg),
    {pos_compl, _} = ftp_response:interpret(NewMsg),
    ok. 


%%-------------------------------------------------------------------------
format_error(Config) when is_list(Config) ->
    "Synchronisation error during chunk sending." =
	ftp:formaterror(echunk),
    "Session has been closed." = ftp:formaterror(eclosed),
    "Connection to remote server prematurely closed." =
	ftp:formaterror(econn),
    "File or directory already exists." =  ftp:formaterror(eexists),
    "Host not found, FTP server not found, or connection rejected." =
	ftp:formaterror(ehost),
    "User not logged in." = ftp:formaterror(elogin),
    "Term is not a binary." = ftp:formaterror(enotbinary),
    "No such file or directory, already exists, or permission denied."
	= ftp:formaterror(epath),
    "No such type." = ftp:formaterror(etype),
    "User name or password not valid." =  ftp:formaterror(euser),
    "Insufficient storage space in system." = ftp:formaterror(etnospc),
    "Exceeded storage allocation (for current directory or dataset)." 
	= ftp:formaterror(epnospc),
    "File name not allowed." = ftp:formaterror(efnamena),
    "Unknown error: foobar" = ftp:formaterror({error, foobar}).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse(Module, Function, Args, Bin) when is_binary(Bin) ->
    parse(Module, Function, Args, [binary_to_list(Bin)]);

parse(Module, Function, [AccLines, StatusCode], [Data | Rest]) ->
    case  Module:Function(list_to_binary(Data), AccLines, StatusCode) of
	{ok, Result, <<>>} ->
	    Result;
	{ok, Result, Next} ->
	    {Result, Next};
	{continue, {NewData, NewAccLines, NewStatusCode}} ->
	    case Rest of
		[] ->
		    ct:fail({wrong_input, Data, Rest});
		[_ | _] ->
		    parse(Module, Function, [NewAccLines, NewStatusCode], 
			  [binary_to_list(NewData) ++ hd(Rest) | tl(Rest)])
	    end
    end.
