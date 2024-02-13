%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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
-module(io_proto_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([setopts_getopts/1,unicode_options/1,unicode_options_gen/1, 
	 binary_options/1, read_modes_gl/1,
	 read_modes_ogl/1, broken_unicode/1,eof_on_pipe/1,
         unicode_prompt/1, shell_slogan/1, raw_stdout/1, raw_stdout_isatty/1,
         file_read_stdin_binary_mode/1, file_read_stdin_list_mode/1,
         file_read_stdin_unicode_translation_error_binary_mode/1,
         file_read_stdin_unicode_translation_error_list_mode/1,
         file_read_line_stdin_unicode_translation_error_binary_mode/1,
         file_read_line_stdin_unicode_translation_error_list_mode/1,
         io_get_chars_stdin_binary_mode/1, io_get_chars_stdin_list_mode/1,
         io_get_until_stdin_binary_mode/1, io_get_until_stdin_list_mode/1,
         io_get_chars_file_read_stdin_binary_mode/1,
         file_read_stdin_latin1_binary_mode/1,
         file_read_stdin_latin1_list_mode/1,
         io_fwrite_stdin_latin1_mode/1
        ]).


-export([io_server_proxy/1,start_io_server_proxy/0, proxy_getall/1, 
	 proxy_setnext/2, proxy_quit/1]).
%% For spawn
-export([answering_machine1/3, answering_machine2/3]).

-export([uprompt/1, slogan/0, session_slogan/0]).

-export([write_raw_to_stdout/0, read_raw_from_stdin/1]).

-export([get_until_eof/2]).

%%-define(debug, true).

-ifdef(debug).
-define(dbg(Data),io:format(standard_error, "DBG: ~p\r\n",[Data])).
-else.
-define(dbg(Data),noop).
-endif.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [setopts_getopts, unicode_options, unicode_options_gen,
     binary_options, read_modes_gl, read_modes_ogl,
     broken_unicode, eof_on_pipe, unicode_prompt,
     shell_slogan, raw_stdout, raw_stdout_isatty,
     file_read_stdin_binary_mode,
     file_read_stdin_list_mode,
     file_read_stdin_unicode_translation_error_binary_mode,
     file_read_stdin_unicode_translation_error_list_mode,
     file_read_line_stdin_unicode_translation_error_binary_mode,
     file_read_line_stdin_unicode_translation_error_list_mode,
     io_get_chars_stdin_binary_mode,
     io_get_chars_stdin_list_mode,
     io_get_until_stdin_binary_mode,
     io_get_until_stdin_list_mode,
     io_get_chars_file_read_stdin_binary_mode,
     file_read_stdin_latin1_binary_mode,
     file_read_stdin_latin1_list_mode,
     io_fwrite_stdin_latin1_mode
    ].

groups() -> 
    [].

init_per_suite(Config) ->
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM","vt100"),
    DefShell = rtnode:get_default_shell(),
    [{default_shell,DefShell},{term, Term}|Config].

end_per_suite(Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-record(state, {
                q = [],
                nxt = eof,
                mode = list
               }).

uprompt(_L) ->
    [1050,1072,1082,1074,1086,32,1077,32,85,110,105,99,111,100,101,32,63].

%% Test that an Unicode prompt does not crash the shell.
unicode_prompt(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    case proplists:get_value(default_shell,Config) of
	new ->
	    rtnode:run(
              [{putline,""},
               {putline, "2."},
               {expect, "[\n ]2"},
               {putline, "shell:prompt_func({io_proto_SUITE,uprompt})."},
               {expect, "[\n ]default"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect, "\\Q\"hej\\n\"\\E"},
               {putline, "io:setopts([{binary,true}])."},
               {expect, "[\n ]ok"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect,"[\n ]hej"},
               {expect, "\\Q<<\"hej\\n\">>\\E"}
              ],[],"",["-pa",PA]);
        _ ->
            ok
    end,
    %% And one with oldshell
    rtnode:run(
      [{putline,""},
       {putline, "2."},
       {expect, "[\n ]2"},
       {putline, "shell:prompt_func({io_proto_SUITE,uprompt})."},
       {expect, "default"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect, "\\Q\"hej\\n\"\\E"},
       {putline, "io:setopts([{binary,true}])."},
       {expect, "[\n ]\\?*ok"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect,"[\n ]\\?*hej"},
       {expect, "\\Q<<\"hej\\n\">>\\E"}
      ],[],"",["-oldshell","-pa",PA]),
    ok.

%% Test that an Unicode prompt does not crash the shell.
shell_slogan(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    case proplists:get_value(default_shell,Config) of
	new ->
	    rtnode:run(
              [{expect, "\\Q"++string:trim(erlang:system_info(system_version))++"\\E"},
               {expect, "\\Q"++io_lib:format("Eshell V~s (press Ctrl+G to abort, type help(). for help)",[erlang:system_info(version)])++"\\E"}
              ],[],"",[]),
      	    rtnode:run(
              [{expect, "\nTest slogan"},
               {expect, "\nTest session slogan \\("}
              ],[],"",["-stdlib","shell_slogan","\"Test slogan\"",
                       "-stdlib","shell_session_slogan","\"Test session slogan\""]),
      	    rtnode:run(
              [{expect, "\nTest slogan"},
               {expect, "\\Q\nTest session slogan (\\E"}
              ],[],"",["-stdlib","shell_slogan","fun io_proto_SUITE:slogan/0",
                       "-stdlib","shell_session_slogan","fun io_proto_SUITE:session_slogan/0",
                       "-pa",PA]);
        _ ->
            ok
    end.

slogan() ->
    "Test slogan".
session_slogan() ->
    "Test session slogan".

%% Check io:setopts and io:getopts functions.
setopts_getopts(Config) when is_list(Config) ->
    FileName = filename:join([proplists:get_value(priv_dir,Config),
			      "io_proto_SUITE_setopts_getopts.dat"]),
    {ok,WFile} = file:open(FileName,[write]),
    Server = start_io_server_proxy(),
    [{binary, false}] = io:getopts(Server),
    [getopts] = proxy_getall(Server),
    [{binary,false},{encoding,latin1}] = lists:sort(io:getopts(WFile)),
    proxy_setnext(Server,"Hej"),
    "Hej" = io:get_line(Server,''),
    proxy_setnext(Server,"Hej"++[532]),
    [$H,$e,$j,532] = io:get_line(Server,''),
    ok = io:setopts(Server,[{binary,true}]),
    proxy_setnext(Server,"Hej"),
    <<"Hej">> = io:get_line(Server,''),
    proxy_setnext(Server,"Hej"++[532]),
    <<72,101,106,200,148>> = io:get_line(Server,''),
    [$H,$e,$j,532] = lists:flatten(io_lib:format("~ts",[<<72,101,106,200,148>>])),
    file:write(WFile,<<"HejA">>),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,unicode)),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf16,big})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf16,little})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf32,big})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf32,little})),
    file:close(WFile),
    {ok,RFile} = file:open(FileName,[read]),
    [{binary,false},{encoding,latin1}] = lists:sort(io:getopts(RFile)),
    [$H,$e,$j,$A] = io:get_chars(RFile,'',4),
    io:setopts(RFile,[{encoding,unicode}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,unicode}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,big}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf16,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,little}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf16,little}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,big}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf32,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,little}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf32,little}}] =
	lists:sort(io:getopts(RFile)),
    eof = io:get_line(RFile,''),
    file:position(RFile,0),
    io:setopts(RFile,[{binary,true},{encoding,latin1}]),
    <<$H,$e,$j,$A>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,latin1}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,unicode}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,unicode}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,big}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf16,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,little}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf16,little}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,big}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf32,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,little}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf32,little}}] =
	lists:sort(io:getopts(RFile)),
    eof = io:get_line(RFile,''),
    file:close(RFile),
    case proplists:get_value(default_shell,Config) of
	new ->
	    %% So, lets test another node with new interactive shell
	    rtnode:run(
              [{putline,""},
               {putline, "2."},
               {expect, "[\n ]2[^.]"},
               {putline, "lists:keyfind(binary,1,io:getopts())."},
               {expect, "{binary,false}"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect, "\\Q\"hej\\n\"\\E"},
               {putline, "io:setopts([{binary,true}])."},
               {expect, "[\n ]ok"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect, "\\Q<<\"hej\\n\">>\\E"}
              ],[]);
        _ ->
            ok
    end,
    %% And one with oldshell
    rtnode:run(
      [{putline,""},
       {putline, "2."},
       {expect, "[\n ]2[^.]"},
       {putline, "lists:keyfind(binary,1,io:getopts())."},
       {expect, "[\n ]{binary,false}"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect, "\\Q\"hej\\n\"\\E"},
       {putline, "io:setopts([{binary,true}])."},
       {expect, "[\n ]ok"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect, "\\Q<<\"hej\\n\">>\\E"}
      ],[],"",["-oldshell"]),
    ok.

%% Test that reading from stdin using file:read works when io is in binary mode
file_read_stdin_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read(standard_io, 3) end, [binary]),

    erlang:port_command(ErlPort, "abc"),
    {ok, "got: <<\"abc\">>\n"} = gen_tcp:recv(P, 0),
    erlang:port_command(ErlPort, "def"),
    {ok, "got: <<\"def\">>\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read works when io is in binary mode
file_read_stdin_list_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read(standard_io, 3) end, [list]),

    erlang:port_command(ErlPort, "abc"),
    {ok, "got: \"abc\"\n"} = gen_tcp:recv(P, 0),
    erlang:port_command(ErlPort, "def"),
    {ok, "got: \"def\"\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read returns
%% correct error when in binary mode
file_read_stdin_unicode_translation_error_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read(standard_io, 3) end, [binary]),

    erlang:port_command(ErlPort, <<"ęö€"/utf8>>),
    {ok, "error: {no_translation,unicode,latin1}\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read returns
%% correct error when in list mode
file_read_stdin_unicode_translation_error_list_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read(standard_io, 3) end, [list]),

    erlang:port_command(ErlPort, <<"ęö€"/utf8>>),
    {ok, "error: {no_translation,unicode,latin1}\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read_line returns
%% correct error when in binary mode
file_read_line_stdin_unicode_translation_error_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read_line(standard_io) end, [binary]),

    erlang:port_command(ErlPort, <<"ę\nö\n€"/utf8>>),
    {ok, "error: {no_translation,unicode,latin1}\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read_line returns
%% correct error when in list mode
file_read_line_stdin_unicode_translation_error_list_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(fun() -> file:read_line(standard_io) end, [list]),

    erlang:port_command(ErlPort, <<"ę\nö\n€"/utf8>>),
    {ok, "error: {no_translation,unicode,latin1}\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using io:get_chars works when io is in binary mode
io_get_chars_stdin_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(
                         fun() ->
                                 case io:get_chars(standard_io, "", 1) of
                                     eof -> eof;
                                     Chars -> {ok, Chars}
                                 end
                         end, [binary]),

    erlang:port_command(ErlPort, "x\n"),
    {ok, "got: <<\"x\">>\n"} = gen_tcp:recv(P, 0),
    {ok, "got: <<\"\\n\">>\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using custom io_request works when io is in binary mode
io_get_until_stdin_binary_mode(_Config) ->

    GetUntilEof =
        fun() ->
                IoServer = group_leader(),
                IoServer !
                    {io_request,
                     self(),
                     IoServer,
                     {get_until, unicode, '', ?MODULE, get_until_eof, []}},
                receive
                    {io_reply, IoServer, Data} ->
                        {ok, Data}
                end
        end,

    {ok, P, ErlPort} = start_stdin_node(GetUntilEof, [binary]),

    erlang:port_command(ErlPort, "x\n"),
    {error, timeout} = gen_tcp:recv(P, 0, 250),
    ErlPort ! {self(), close},
    {ok, "got: <<\"x\\n\">>\n"} = gen_tcp:recv(P, 0),

    {ok, P2, ErlPort2} = start_stdin_node(GetUntilEof, [binary]),

    {error, timeout} = gen_tcp:recv(P2, 0, 250),
    ErlPort2 ! {self(), close},
    {ok, "got: eof\n"} = gen_tcp:recv(P2, 0),

    ok.

%% Test that reading from stdin using custom io_request works when io is in list mode
io_get_until_stdin_list_mode(_Config) ->

    GetUntilEof =
        fun() ->
                IoServer = group_leader(),
                IoServer !
                    {io_request,
                     self(),
                     IoServer,
                     {get_until, unicode, '', ?MODULE, get_until_eof, []}},
                receive
                    {io_reply, IoServer, Data} ->
                        {ok, Data}
                end
        end,

    {ok, P, ErlPort} = start_stdin_node(GetUntilEof, [list]),

    erlang:port_command(ErlPort, "x\n"),
    {error, timeout} = gen_tcp:recv(P, 0, 250),
    ErlPort ! {self(), close},
    {ok, "got: \"x\\n\"\n"} = gen_tcp:recv(P, 0),

    {ok, P2, ErlPort2} = start_stdin_node(GetUntilEof, [list]),

    {error, timeout} = gen_tcp:recv(P2, 0, 250),
    ErlPort2 ! {self(), close},
    {ok, "got: eof\n"} = gen_tcp:recv(P2, 0),

    ok.

get_until_eof([],eof) -> {done,eof,[]};
get_until_eof(ThisFar,eof) -> {done,ThisFar,eof};
get_until_eof(ThisFar,CharList) -> {more,ThisFar++CharList}.

%% Test that reading from stdin using io:get_chars works when io is in list mode
io_get_chars_stdin_list_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(
                         fun() -> case io:get_chars(standard_io, "", 1) of
                                      eof -> eof;
                                      Chars -> {ok, Chars}
                                  end
                         end, [list]),

    erlang:port_command(ErlPort, "x\n"),
    {ok, "got: \"x\"\n"} = gen_tcp:recv(P, 0),
    {ok, "got: \"\\n\"\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that mixing io:get_chars and file:read works when stdin is in binary mode.
io_get_chars_file_read_stdin_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(
                         fun() -> case file:read(standard_io, 1) of
                                      eof -> eof;
                                      {ok, Chars} ->
                                          case io:get_line(standard_io, "") of
                                              eof -> Chars;
                                              Line ->
                                                  {ok, [Chars, Line]}
                                          end
                                  end
                         end, [binary]),

    erlang:port_command(ErlPort, "1\n"),
    {ok, "got: [<<\"1\">>,<<\"\\n\">>]\n"} = gen_tcp:recv(P, 0),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0),

    ok.

%% Test that reading from stdin using file:read_line works when io is not utf8
file_read_stdin_latin1_binary_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(
                         fun() -> file:read_line(standard_io) end,
                         [binary],
                         "-kernel standard_io_encoding latin1"),

    %% Invalid utf8
    erlang:port_command(ErlPort, <<192,128,10,192,128,10,192,128,10>>),

    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P, 0, 5000),
    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P, 0, 5000),
    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P, 0, 5000),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0, 5000),

    {ok, P2, ErlPort2} = start_stdin_node(
                         fun() -> file:read(standard_io, 5) end,
                         [binary],
                         "-kernel standard_io_encoding latin1"),

    %% Valid utf8
    erlang:port_command(ErlPort2, <<"duπaduπaduπa"/utf8>>),

    {ok, "got: <<100,117,207,128,97>>\n"} = gen_tcp:recv(P2, 0, 5000),
    {ok, "got: <<100,117,207,128,97>>\n"} = gen_tcp:recv(P2, 0, 5000),
    {ok, "got: <<100,117,207,128,97>>\n"} = gen_tcp:recv(P2, 0, 5000),
    ErlPort2 ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P2, 0, 5000),

    %% Setting using io:setopts used to hang on Windows, see #7459 for details.
    {ok, P3, ErlPort3} = start_stdin_node(
                         fun() -> file:read_line(standard_io) end,
                         [binary],
                         "-eval \"io:setopts([{encoding, latin1}])\""),

    %% Invalid utf8
    erlang:port_command(ErlPort3, <<192,128,10,192,128,10,192,128,10>>),

    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P3, 0, 5000),
    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P3, 0, 5000),
    {ok, "got: <<192,128,10>>\n"} = gen_tcp:recv(P3, 0, 5000),
    ErlPort3 ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P3, 0, 5000),

    ok.

%% Test that reading from stdin using file:read_line works when io is not utf8
file_read_stdin_latin1_list_mode(_Config) ->
    {ok, P, ErlPort} = start_stdin_node(
                         fun() -> file:read_line(standard_io) end,
                         [list],
                         "-kernel standard_io_encoding latin1"),

    %% Invalid utf8
    erlang:port_command(ErlPort, <<192,128,10,192,128,10,192,128,10>>),

    {ok, "got: [192,128,10]\n"} = gen_tcp:recv(P, 0, 5000),
    {ok, "got: [192,128,10]\n"} = gen_tcp:recv(P, 0, 5000),
    {ok, "got: [192,128,10]\n"} = gen_tcp:recv(P, 0, 5000),
    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0, 5000),

    {ok, P2, ErlPort2} = start_stdin_node(
                         fun() -> file:read(standard_io, 5) end,
                         [list],
                         "-kernel standard_io_encoding latin1"),

    %% Valid utf8
    erlang:port_command(ErlPort2, <<"duπaduπaduπa"/utf8>>),

    {ok, "got: [100,117,207,128,97]\n"} = gen_tcp:recv(P2, 0, 5000),
    {ok, "got: [100,117,207,128,97]\n"} = gen_tcp:recv(P2, 0, 5000),
    {ok, "got: [100,117,207,128,97]\n"} = gen_tcp:recv(P2, 0, 5000),
    ErlPort2 ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P2, 0, 5000),

    ok.

%% Test that reading from stdin using file:read works when io is not utf8,
%% but unicode is printed out
io_fwrite_stdin_latin1_mode(_Config) ->
    {ok, P, ErlPort} =
        start_stdin_node(
          fun() -> case file:read(standard_io, 5) of
                       {ok, Chars} ->
                           %% We've read a unicode string as latin1,
                           %% which means that if we convert it to
                           %% a binary it will be seen as the original
                           %% unicode string.
                           io:format("~ts",[list_to_binary(Chars)]),
                           {ok, Chars};
                       Else ->
                           Else
                   end
          end,
          [list],
          "-kernel standard_io_encoding latin1"),

    %% Valid utf8
    erlang:port_command(ErlPort, <<"duπa"/utf8>>),

    {ok, "got: [100,117,207,128,97]\n"} = gen_tcp:recv(P, 0, 5000),
    receive
        {ErlPort, {data, Data}} ->
            %% On stdout any unicode should be translated to hex syntax
            "du\\x{3C0}a" = Data
    end,

    ErlPort ! {self(), close},
    {ok, "got: eof"} = gen_tcp:recv(P, 0, 5000),

    ok.

start_stdin_node(ReadFun, IoOptions) ->
    start_stdin_node(ReadFun, IoOptions, "").
start_stdin_node(ReadFun, IoOptions, ExtraArgs) ->
    {ok, L} = gen_tcp:listen(0,[{active, false},{packet,4}]),
    {ok, Port} = inet:port(L),
    Cmd = lists:append(
            [ct:get_progname(),
             " -noshell ",
             ExtraArgs,
             " -pa ", filename:dirname(code:which(?MODULE)),
             " -s ", atom_to_list(?MODULE), " read_raw_from_stdin ", integer_to_list(Port)]),
    ct:log("~p~n", [Cmd]),
    ErlPort = open_port({spawn, Cmd}, [stream, eof, stderr_to_stdout,
                                       {env, [{"LC_CTYPE","en_US.UTF-8"}]}]),
    {ok, P} = gen_tcp:accept(L),
    gen_tcp:send(P, term_to_binary(IoOptions)),
    gen_tcp:send(P, term_to_binary(ReadFun)),
    {ok, P, ErlPort}.

read_raw_from_stdin([Port]) ->
    try
        {ok, P} = gen_tcp:connect(localhost, list_to_integer(atom_to_list(Port)),
                                  [binary, {packet, 4}, {active, false}]),
        {ok, OptionsBin} = gen_tcp:recv(P, 0),
        io:setopts(standard_io, binary_to_term(OptionsBin)),
        {ok, ReadFunBin} = gen_tcp:recv(P, 0),
        spawn(fun() ->
                      gen_tcp:recv(P, 0),
                      init:stop("crash")
              end),
        read_raw_from_stdin(binary_to_term(ReadFunBin), P)
    catch E:R:ST ->
            io:format(standard_error, "~p  ~p",[Port,{E,R,ST}])
    end.
read_raw_from_stdin(ReadFun, P) ->
    case ReadFun() of
        eof ->
            gen_tcp:send(P, "got: eof"),
            init:stop();
        {ok, Char} ->
            gen_tcp:send(P, unicode:characters_to_binary(
                              io_lib:format("got: ~p\n",[Char]))),
            read_raw_from_stdin(ReadFun, P);
        {ok, Fmt, Char} ->
            gen_tcp:send(P, unicode:characters_to_binary(
                              io_lib:format("got: "++Fmt++"\n",[Char]))),
            read_raw_from_stdin(ReadFun, P);
        {error, Reason} ->
            gen_tcp:send(P, unicode:characters_to_binary(
                              io_lib:format("error: ~p\n",[Reason]))),
            read_raw_from_stdin(ReadFun, P)
    end.

get_lc_ctype() ->
    case {os:type(),os:version()} of
	{{unix,sunos},{5,N,_}} when N =< 8 ->
	    "iso_8859_1";
	_ ->
	    "ISO-8859-1"
    end.

%% Test various unicode options.
unicode_options(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    PrivDir = proplists:get_value(priv_dir,Config),
    %% A string in both russian and greek characters, which is present
    %% in all the internal test files (but in different formats of course)...
    TestData = [1090,1093,1077,32,1073,1080,1075,32,
		1088,1077,1076,32,1092,1086,1100,32,1093,
		1072,1089,32,1089,1086,1100,32,932,951,949,
		32,946,953,947,32,961,949,948,32,
		963,959,967,32,945,961,949,32,966,959,967,949,963],
    %% Testdata from Chinese open source customer, that triggered OTP-7974
    TestData2 = [46,46,46,12411,12370,12411,12370,44,12411,12370,12411,12370,44,
		 12411,12370,12411,12370,44,12411,12370,12411,12370,44,12411,12370,
		 12411,12370,44,44,44,12411,12370,12411,12370,44,44,12411,12370,12411,
		 12370,44,12411,12370,12411,12370,44,12411,12370,12411,12370,44,12411,
		 12370,12411,12370,44,12411,12370,12411,12370,44,44,44,10],

    %% The external test files are generated with a BOM writing
    %% text editor. A shorter line is written (with two characters 
    %% larger than 127).
    ExternalTestData = [197,116,101,114,101,114,246,118,114,97],
    InternalBomFiles = ["testdata_utf8_bom.dat",
			"testdata_utf16_big_bom.dat",
			"testdata_utf16_little_bom.dat",
			"testdata_utf32_big_bom.dat",
			"testdata_utf32_little_bom.dat"],
    AllNoBom = [{utf8,"testdata_utf8.dat"},
		{utf16,"testdata_utf16_big.dat"},
		{{utf16,big},"testdata_utf16_big.dat"},
		{{utf16,little},"testdata_utf16_little.dat"},
		{utf32,"testdata_utf32_big.dat"},
		{{utf32,big},"testdata_utf32_big.dat"},
		{{utf32,little},"testdata_utf32_little.dat"}],
    ExternalBomFiles = ["external_utf8_bom.dat",
			"external_utf16_little_bom.dat",
			"external_utf16_big_bom.dat"],
    ReadBomFile = fun(File,Dir) ->
			  {ok,F} = file:open(filename:join([Dir,File]),
					     [read,binary]),
			  {ok,Bin} = file:read(F,4),
			  {Type,Bytes} = unicode:bom_to_encoding(Bin),
			  file:position(F,Bytes),
			  io:setopts(F,[{encoding,Type}]),
			  R = unicode:characters_to_list(
				io:get_chars(F,'',length(TestData)),unicode),
			  file:close(F),
			  R
		  end,
    ReadBomlessFile = fun({Type,File},DataLen,Dir) ->
			      {ok,F} = file:open(filename:join([Dir,File]),
						 [read,binary,
						  {encoding,Type}]),
			      R = unicode:characters_to_list(
				    io:get_chars(F,'',DataLen),unicode),
			      file:close(F),
			      R
		      end,
    ReadBomlessFileList = fun({Type,File},DataLen,Dir) ->
				  {ok,F} = file:open(filename:join([Dir,File]),
						     [read,
						      {encoding,Type}]),
				  R = io:get_chars(F,'',DataLen),
				  file:close(F),
				  R
			  end,
    ReadBomlessFileListLine = fun({Type,File},Dir) ->
				      {ok,F} = file:open(filename:join([Dir,File]),
							 [read,
							  {encoding,Type}]),
				      R = io:get_line(F,''),
				      file:close(F),
				      R
			      end,
    [TestData = ReadBomFile(F,DataDir) || F <- InternalBomFiles ],
    [ExternalTestData = ReadBomFile(F,DataDir) || F <- ExternalBomFiles ],
    [TestData = ReadBomlessFile(F,length(TestData),DataDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileList(F,length(TestData),DataDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileListLine(F,DataDir) || F <- AllNoBom ],

    BomDir = filename:join([PrivDir,"BOMDATA"]),
    BomlessDir = filename:join([PrivDir,"BOMLESSDATA"]),
    file:make_dir(BomDir),
    file:make_dir(BomlessDir),

    WriteBomFile = fun({Enc,File},Dir) ->
			   {ok,F} = file:open(filename:join([Dir,File]),
					      [write,binary]),
			   file:write(F,unicode:encoding_to_bom(Enc)),
			   io:setopts(F,[{encoding,Enc}]),
			   io:put_chars(F,TestData),
			   file:close(F),
			   ok
		   end,
    [ ok = WriteBomFile(F,BomDir) || F <- AllNoBom ],
    [TestData = ReadBomFile(F,BomDir) || {_,F} <- AllNoBom ],
    WriteBomlessFile = fun({Enc,File},TData,Dir) ->
			       {ok,F} = file:open(
					  filename:join([Dir,File]),
					  [write,binary,{encoding,Enc}]),
			       io:put_chars(F,TData),
			       file:close(F),
			       ok
		       end,
    [ ok = WriteBomlessFile(F,TestData,BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFile(F,length(TestData),BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileList(F,length(TestData),BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileListLine(F,BomlessDir) || F <- AllNoBom ],

    CannotReadFile = fun({Enc,File},Dir) ->
			     %%io:format(standard_error,"~s\r\n",[filename:join([Dir,File])]),
			     {ok,F} = file:open(
					filename:join([Dir,File]),
					[read,binary,{encoding,Enc}]),
			     Enc2 = case Enc of
					utf8 ->
					    unicode;
					Tpl when is_tuple(Tpl) ->
					    Tpl;
					Atom when is_atom(Atom) ->
					    {Atom, big}
				    end,
			     {error, {no_translation,Enc2,latin1}} = 
				 file:read(F,10),
			     {error,terminated} = io:get_chars(F,'',10),
			     ok
		     end,
    [ ok = CannotReadFile(F,DataDir) || F <- AllNoBom ],
    [ ok = CannotReadFile(F,BomlessDir) || F <- AllNoBom ],
    [ ok = CannotReadFile(F,BomDir) || F <- AllNoBom ],

    [ ok = WriteBomlessFile(F,TestData2,BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFile(F,length(TestData2),BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFileList(F,length(TestData2),BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFileListLine(F,BomlessDir) || F <- AllNoBom ],


    FailDir = filename:join([PrivDir,"FAIL"]),
    file:make_dir(FailDir),

    CannotWriteFile = fun({_Enc,File},Dir) ->
			      {ok,F} = file:open(
					 filename:join([Dir,File]),
					 [write,binary]),
			      {'EXIT', {no_translation,_}} =
				  (catch io:put_chars(F,TestData)),
			      {'EXIT', {terminated,_}} = (catch io:put_chars(F,TestData)),
			      ok
		      end,
    [ ok = CannotWriteFile(F,FailDir) || F <- AllNoBom ],

    case proplists:get_value(default_shell,Config) of
	new ->
	    %% OK, time for the group_leaders...
	    rtnode:run(
              [{putline,""},
               {putline, "2."},
               {expect, "[\n ]2[^.]"},
               {putline, "lists:keyfind(encoding,1,io:getopts())."},
               {expect, "{encoding,latin1}"},
               {putline, "io:format(\"~ts~n\",[[1024]])."},
               {expect, "\\Q\\x{400}\\E"},
               {putline, "io:setopts([unicode])."},
               {expect, "[\n ]ok"},
               {putline, "io:format(\"~ts~n\",[[1024]])."},
               {expect, "[\n ]"++[1024]}
              ],[],"",["-env","LC_ALL",get_lc_ctype()]);
        _ ->
            ok
    end,
    rtnode:run(
      [{putline,""},
       {putline, "2."},
       {expect, "[\n ]2[^.]"},
       {putline, "lists:keyfind(encoding,1,io:getopts())."},
       {expect, "[\n ]{encoding,latin1}"},
       {putline, "io:format(\"~ts~n\",[[1024]])."},
       {expect, "\\Q\\x{400}\\E"},
       {putline, "io:setopts([{encoding,unicode}])."},
       {expect, "[\n ]ok"},
       {putline, "io:format(\"~ts~n\",[[1024]])."},
       {expect, "[\n ]"++[1024]}
      ],[],"",
      ["-oldshell","-env","LC_ALL",get_lc_ctype()]),
    ok.

%% Tests various unicode options on random generated files.
unicode_options_gen(Config) when is_list(Config) ->
    ct:timetrap({minutes,30}), %% valgrind needs a alot of time
    rand:seed(default),
    io:format("*** SEED: ~p ***\n", [rand:export_seed()]),
    PrivDir = proplists:get_value(priv_dir, Config),
    AllModes = [utf8,utf16,{utf16,big},{utf16,little},
		utf32,{utf32,big},{utf32,little}],
    FSize = 9*1024,
    NumItersRead = 2,
    NumItersWrite = 2,
    Dir = filename:join(PrivDir, "GENDATA1"),
    file:make_dir(Dir),

    DoOneFile1 =
	fun(Encoding, N, M) ->
		?dbg({Encoding,M,N}),
		io:format("Read test: Encoding ~p, Chunk size ~p, Iteration ~p~n",[Encoding,M,N]),
		io:format(standard_error,
			  "Read test: Encoding ~p, Chunk size ~p, Iteration ~p\r\n",[Encoding,M,N]),
		Fname = filename:join(Dir,
				      "genfile_"++enc2str(Encoding)++
					  "_"++integer_to_list(N)),
		Ulist = random_unicode(FSize),
		Bin = unicode:characters_to_binary(Ulist, utf8, Encoding),
		ok = file:write_file(Fname, Bin),

		Read1 = fun(FD) -> io:get_line(FD, '') end,
		Res1 = read_whole_file(Fname,
				       [read,read_ahead,{encoding,Encoding}],
				       Read1),

		Read2 = fun(FD) -> io:get_chars(FD, '', M) end,
		Res2 = read_whole_file(Fname,
				       [read,binary,
					read_ahead,{encoding,Encoding}],
				       Read2),

		Read3 = fun(FD) ->
				case io:fread(FD, '', "~ts") of
				    {ok,D} -> D;
				    Other -> Other end
			end,
		Res3 = read_whole_file(Fname,
				       [read,binary,
					read_ahead,{encoding,Encoding}],
				       Read3),

		Read4 = fun(FD) ->
				case io:fread(FD, '', "~ts") of
				    {ok,D} -> D;
				    Other -> Other end
			end,
		Res4 = read_whole_file(Fname,
				       [read,read_ahead,{encoding,Encoding}],
				       Read4),

		Ulist2 = [X || X <- Ulist, X =/= $\n, X =/= $\s, X =/= $\t],
		Ulist = done(Res1),
		Ulist = done(Res2),
		Ulist2 = done(Res3),
		Ulist2 = done(Res4),

		file:delete(Fname)
	end,
    [ [ [ DoOneFile1(E, N, M) || E <- AllModes ] ||
	  M <- [10,1000,128,1024,8192,8193] ] ||
	N <- lists:seq(1, NumItersRead) ],

    DoOneFile2 =
	fun(Encoding,N,M) ->
		?dbg({Encoding,M,N}),
		io:format("Write test: Encoding ~p, Chunk size ~p, Iteration ~p~n",[Encoding,M,N]),
		io:format(standard_error,
			  "Write test: Encoding ~p, Chunk size ~p, Iteration ~p\r\n",[Encoding,M,N]),
		Fname = filename:join(Dir,
				      "genfile_"++enc2str(Encoding)++
					  "_"++integer_to_list(N)),
		Ulist = random_unicode(FSize),

		Res1 = write_read_file(Fname, 1,
				       [write],
				       Encoding,
				       fun(FD) -> io:put_chars(FD, Ulist) end),

		Res2 = write_read_file(Fname, 2,
				       [write,binary],
				       Encoding,
				       fun(FD) -> io:put_chars(FD, Ulist) end),

		Fun3 = fun(FD) ->
			       _ = [io:format(FD, "~tc", [C]) || C <- Ulist],
			       ok
		       end,
		Res3 = write_read_file(Fname, 3,
				       [write],
				       Encoding,
				       Fun3),

		Fun4 = fun(FD) ->
			       io:put_chars(FD,
					    unicode:characters_to_binary(Ulist))
		       end,
		Res4 = write_read_file(Fname, 4,
				       [write],
				       Encoding,
				       Fun4),

		LL = string:tokens(Ulist, "\n"),
		Fun5 = fun(FD) ->
			       _ = [io:format(FD, "~ts", [L]) || L <- LL],
			       ok
		       end,
		Res5 = write_read_file(Fname, 5,
				       [write],
				       Encoding,
				       Fun5),

		Ulist2 = lists:flatten(LL),
		ResBin = done(Res1),
		ResBin = done(Res2),
		ResBin = done(Res3),
		ResBin = done(Res4),
		Ulist = unicode:characters_to_list(ResBin, Encoding),

		ResBin2 = done(Res5),
		Ulist2 = unicode:characters_to_list(ResBin2, Encoding),

		ok
	end,
    [ [ [ DoOneFile2(E, N, M) || E <- AllModes ] ||
	  M <- [10,1000,128,1024,8192,8193] ] ||
	N <- lists:seq(1, NumItersWrite) ],
    ok.

read_whole_file(Fname, Options, Fun) ->
    do(fun() ->
	       do_read_whole_file(Fname, Options, Fun)
       end).

do_read_whole_file(Fname, Options, Fun) ->
    {ok,F} = file:open(Fname, Options),
    Res = do_read_whole_file_1(Fun, F),
    ok = file:close(F),
    unicode:characters_to_list(Res, unicode).

do_read_whole_file_1(Fun, F) ->
    case Fun(F) of
	eof ->
	    [];
	{error,Error} ->
	    receive after 10000 -> ok end,
	    exit(Error);
	Other ->
	    [Other|do_read_whole_file_1(Fun, F)]
    end.

write_read_file(Fname0, N, Options, Enc, Writer) ->
    Fname = Fname0 ++ "_" ++ integer_to_list(N),
    do(fun() ->
	       do_write_read_file(Fname, Options, Enc, Writer)
       end).

do_write_read_file(Fname, Options, Encoding, Writer) ->
    {ok,F} = file:open(Fname, [{encoding,Encoding}|Options]),
    Writer(F),
    ok = file:close(F),
    {ok,Bin} = file:read_file(Fname),
    ok = file:delete(Fname),
    Bin.

enc2str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
enc2str({A1,A2}) when is_atom(A1), is_atom(A2) ->
    atom_to_list(A1)++"_"++atom_to_list(A2).


random_unicode(0) ->
    %% io:fread(Device, Promp, "~ts") will return an error if the file
    %% ends with whitespace characters. This behavior seems to be
    %% deliberate. Therefore, be sure that we never end the file with
    %% a sequence of whitespace characters.
    [$! + rand:uniform(64) |
     case rand:uniform(20) of
         A when A =< 1 -> [$\n];
         _ -> []
     end];
random_unicode(N) ->
    %% Favor large unicode code points and make line breaks.
    X = case rand:uniform(20) of
	    A when A =< 1 -> $\n;
	    A when A =< 3 -> rand:uniform(16#10FFFF);
	    A when A =< 6 -> rand:uniform(16#10FFFF - 16#7F) + 16#7F;
	    A when A =< 12 -> rand:uniform(16#10FFFF - 16#7FF) + 16#7FF;
	    _ -> rand:uniform(16#10FFFF - 16#FFFF) + 16#FFFF
	end,
    if
        X =:= $\r;                            %Can be eaten by io:get_line/2.
        16#D800 =< X, X =< 16#DFFF;
        X =:= 16#FFFE;
        X =:= 16#FFFF ->
	    random_unicode(N);
	true ->
	    [X | random_unicode(N-1)]
    end.


%% Test variants with binary option.
binary_options(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    PrivDir = proplists:get_value(priv_dir,Config),
    TestData = unicode:characters_to_binary(
		 [1090,1093,1077,32,1073,1080,1075,32,
		  1088,1077,1076,32,1092,1086,1100,32,1093,
		  1072,1089,32,1089,1086,1100,32,932,951,949,
		  32,946,953,947,32,961,949,948,32,
		  963,959,967,32,945,961,949,32,966,959,967,949,963]),
    <<First10:10/binary,Second10:10/binary,_/binary>> = TestData,
    First10List = binary_to_list(First10),
    Second10List = binary_to_list(Second10),
    TestFile = filename:join([DataDir, "testdata_utf8.dat"]),
    {ok, F} = file:open(TestFile,[read]),
    {ok, First10List} = file:read(F,10),
    io:setopts(F,[binary]),
    {ok, Second10} = file:read(F,10),
    file:close(F),
    {ok, F2} = file:open(TestFile,[read,binary]),
    {ok, First10} = file:read(F2,10),
    io:setopts(F2,[list]),
    {ok, Second10List} = file:read(F2,10),
    file:position(F2,0),
    First10List = io:get_chars(F2,'',10),
    io:setopts(F2,[binary]),
    Second10 = unicode:characters_to_binary(io:get_chars(F2,'',10),unicode,latin1),
    file:close(F2),
    LineBreakFileName =  filename:join([PrivDir, "testdata.dat"]),
    LineBreakTestData = <<TestData/binary,$\n>>,
    LineBreakTestDataList = binary_to_list(LineBreakTestData),
    file:write_file(LineBreakFileName,[LineBreakTestData,LineBreakTestData,LineBreakTestData,TestData]),
    {ok, F3} = file:open(LineBreakFileName,[read]),
    LineBreakTestDataList = io:get_line(F3,''),
    io:setopts(F3,[binary]),
    LineBreakTestData =  unicode:characters_to_binary(io:get_line(F3,''),unicode,latin1),
    io:setopts(F3,[list]),
    LineBreakTestDataList = io:get_line(F3,''),
    io:setopts(F3,[binary]),
    TestData = unicode:characters_to_binary(io:get_line(F3,''),unicode,latin1),
    eof = io:get_line(F3,''),
    file:close(F3),

    %% OK, time for the group_leaders...
    case proplists:get_value(default_shell,Config) of
	new ->
	    rtnode:run(
              [{putline, "2."},
               {expect, "[\n ]2[^.]"},
               {putline, "lists:keyfind(binary,1,io:getopts())."},
               {expect, "[\n ]{binary,false}"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect, "\\Q\"hej\\n\"\\E"},
               {putline, "io:setopts([{binary,true},unicode])."},
               {expect, "[\n ]ok"},
               {putline, "io:get_line('')."},
               {putline, "hej"},
               {expect, "\\Q<<\"hej\\n\">>\\E"},
               {putline, "io:get_line('')."},
               {putline, binary_to_list(<<"\345\344\366"/utf8>>)},
               {expect, latin1, "[\n ]\\Q<<\""++binary_to_list(<<"\345\344\366"/utf8>>)++"\\n\"/utf8>>\\E"}
              ],[]);
        _ ->
            ok
    end,
    %% And one with oldshell
    rtnode:run(
      [{putline, "2."},
       {expect, "[\n ]2[^.]"},
       {putline, "lists:keyfind(binary,1,io:getopts())."},
       {expect, "[\n ]{binary,false}"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect, "[\n ]\\Q\"hej\\n\"\\E"},
       {putline, "io:setopts([{binary,true},unicode])."},
       {expect, "[\n ]ok"},
       {putline, "io:get_line('')."},
       {putline, "hej"},
       {expect, "\\Q<<\"hej\\n\">>\\E"},
       {putline, "io:get_line('')."},
       {putline, binary_to_list(<<"\345\344\366"/utf8>>)},
       {expect, latin1, "[\n ]\\Q<<\""++binary_to_list(<<"\345\344\366"/utf8>>)++"\\n\"/utf8>>\\E"}
      ],[],"",["-oldshell"]),
    ok.

answering_machine1(OthNode,OthReg,Me) ->
    TestDataLine1 = [229,228,246],
    TestDataUtf = binary_to_list(unicode:characters_to_binary(TestDataLine1)),
    TestDataLine1Oct = "\\\\345( \b)*\\\\344( \b)*\\\\366",
    rtnode:run(
      [{putline,""},
       {putline, "2."},
       {expect, "2"},
       {putline, "io:getopts()."},
       {expect, ">"},
       {putline, "{"++OthReg++","++OthNode++"} ! group_leader()."},
       {expect, "<"},
       %% get_line
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       %% get_chars
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       %% fread
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1Oct},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"}

      ],Me,"",["-env","LC_ALL",get_lc_ctype()]),
    O = list_to_atom(OthReg),
    O ! {self(),done},
    ok.

answering_machine2(OthNode,OthReg,Me) ->
    TestDataLine1 = [229,228,246],
    TestDataUtf = binary_to_list(unicode:characters_to_binary(TestDataLine1)),
    rtnode:run(
      [{putline,""},
       {putline, "2."},
       {expect, "2"},
       {putline, "{"++OthReg++","++OthNode++"} ! group_leader()."},
       {expect, "<[0-9].*"},
       %% get_line
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       %% get_chars
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       %% fread
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, "Hej"},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataLine1},
       {expect, latin1, "\n" ++ TestDataLine1},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"},
       {expect, "Prompt"},
       {putline, TestDataUtf},
       {expect, "Okej"}

      ],Me,"",["-oldshell","-env","LC_ALL",get_lc_ctype()]),
    O = list_to_atom(OthReg),
    O ! {self(),done},
    ok.


%% Test various modes when reading from the group leade from another machine.
read_modes_ogl(Config) when is_list(Config) -> 
    case proplists:get_value(default_shell,Config) of
	noshell ->
	    {skipped,"No run_erl"};
	_ ->
	    read_modes_gl_1(Config,answering_machine2)
    end.

%% Test various modes when reading from the group leade from another machine.
read_modes_gl(Config) when is_list(Config) -> 
    case proplists:get_value(default_shell,Config) of
	noshell ->
	    {skipped,"No run_erl"};
	old ->
	    {skipped,"No new shell"};
	_ ->
	    read_modes_gl_1(Config,answering_machine1)
    end.

read_modes_gl_1(_Config,Machine) ->
    TestDataLine1 = [229,228,246],
    TestDataLine1BinUtf = unicode:characters_to_binary(TestDataLine1),
    TestDataLine1BinLatin = list_to_binary(TestDataLine1),

    N2List = peer:random_name(?FUNCTION_NAME),
    MyNodeList = atom2list(node()),
    register(io_proto_suite,self()),
    AM1 = spawn(?MODULE,Machine,
		[MyNodeList, "io_proto_suite", N2List]),

    GL = receive X when is_pid(X) -> X end,
    ?dbg({group_leader,X}),
    %% get_line
    receive after 500 -> ok end, % Dont clash with the new shell...
    "Hej\n" = io:get_line(GL,"Prompt\n"),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    <<"Hej\n">> = io:get_line(GL,"Prompt\n"),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = chomp(io:request(GL,{get_line,latin1,"Prompt\n"})),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = chomp(io:get_line(GL,"Prompt\n")),
    io:setopts(GL,[{encoding,unicode}]),

    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = chomp(io:request(GL,{get_line,latin1,"Prompt\n"})),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = chomp(io:get_line(GL,"Prompt\n")),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),

    %%get_chars
    "Hej" = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    <<"Hej">> = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = io:request(GL,{get_chars,latin1,"Prompt\n",3}),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[{encoding,unicode}]),

    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = io:request(GL,{get_chars,latin1,"Prompt\n",3}),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),
    %%fread
    {ok,["Hej"]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    {ok,["Hej"]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[{encoding,unicode}]),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),


    receive
	{AM1,done} ->
	    ok
    after 5000 ->
	    exit(timeout)
    end,
    ok.


%% Test behaviour when reading broken Unicode files
broken_unicode(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir,Config),
    Latin1Name = filename:join([Dir,"latin1_data_file.dat"]),
    Utf8Name = filename:join([Dir,"utf8_data_file.dat"]),
    Latin1Data = iolist_to_binary(lists:duplicate(10,lists:seq(0,255)++[255,255,255])),
    Utf8Data = unicode:characters_to_binary(
		 lists:duplicate(10,lists:seq(0,255))),
    file:write_file(Latin1Name,Latin1Data),
    file:write_file(Utf8Name,Utf8Data),
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf8) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ utf8 = heuristic_encoding_file2(Utf8Name,N,utf8) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf16) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf32) || N <- lists:seq(1,100)++[1024,2048,10000]],
    ok.


%%
%% From the cookbook, more or less
heuristic_encoding_file2(FileName,Chunk,Enc) ->
    {ok,F} = file:open(FileName,[read,binary,{encoding,Enc}]),
    loop_through_file2(F,io:get_chars(F,'',Chunk),Chunk,Enc).

loop_through_file2(_,eof,_,Enc) ->
    Enc;
loop_through_file2(_,{error,_Err},_,_) ->
    latin1;
loop_through_file2(F,Bin,Chunk,Enc) when is_binary(Bin) ->
    loop_through_file2(F,io:get_chars(F,'',Chunk),Chunk,Enc).

%% Test eof before newline on stdin when erlang is in pipe.
eof_on_pipe(Config) when is_list(Config) ->
    case {ct:get_progname(),os:type()} of
	{Erl,{unix,linux}} ->
	    %% Not even Linux is reliable - echo can be both styles
	    try
		EchoLine = case os:cmd("echo -ne \"test\\ntest\"") of
			       "test\ntest" ->
				   "echo -ne \"a\\nbu\" | ";
			       _ ->
				   case os:cmd("echo \"test\\ntest\\c\"") of
				       "test\ntest" ->
					   "echo \"a\\nbu\\c\" | ";
				       _ ->
					   throw(skip)
				   end
			   end,
		CommandLine1 = EchoLine ++
		    "\""++Erl++"\" -noshell -eval  "
		    "'io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]).' -run init stop",
		case os:cmd(CommandLine1) of
		    "\"a\\n\"\"bu\"eof" ->
			ok;
		    Other1 ->
			exit({unexpected1,Other1})
		end,
		CommandLine2 = EchoLine ++
		    "\""++Erl++"\" -noshell -eval  "
		    "'io:setopts([binary]),io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]).' -run init stop",
		case os:cmd(CommandLine2) of
		    "<<\"a\\n\">><<\"bu\">>eof" ->
			ok;
		    Other2 ->
			exit({unexpected2,Other2})
		end
	    catch
		throw:skip ->
		    {skipped,"unsupported echo program"}
	    end;
	{_,_} ->
	    {skipped,"Only on linux"}
    end.

raw_stdout(Config) when is_list(Config) ->
    Cmd = lists:append(
            [ct:get_progname(),
             " -noshell -noinput",
             " -pa ", filename:dirname(code:which(?MODULE)),
             " -s ", atom_to_list(?MODULE), " write_raw_to_stdout"]),
    ct:log("~p~n", [Cmd]),
    Port = open_port({spawn, Cmd}, [stream, eof]),
    Expected = lists:seq(0,255),
    Expected = get_all_port_data(Port, []),
    Port ! {self(), close},
    ok.

get_all_port_data(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            get_all_port_data(Port, [Acc|Data]);
        {Port, eof} ->
            lists:flatten(Acc)
    end.

write_raw_to_stdout() ->
    try
        ok = io:setopts(standard_io, [{encoding, latin1}]),
        ok = file:write(standard_io, lists:seq(0,255)),
        halt(0)
    catch
        Class:Reason:StackTrace ->
            io:format(standard_error, "~p~p~p", [Class, Reason, StackTrace]),
            halt(17)
    end.

raw_stdout_isatty(Config) when is_list(Config) ->
    rtnode:run(
        [{putline,"io:setopts(group_leader(), [{encoding, latin1}])."},
         {putline,"file:write(group_leader(),[90, 127, 128, 255, 131, 90, 10])."},%
         {expect, "\\QZ^?\\200\\377\\203Z\\E"}
        ],[]),
        ok.
%%
%% Test I/O-server
%%

start_io_server_proxy() ->
    spawn_link(?MODULE,io_server_proxy,[#state{}]).

proxy_getall(Pid) ->
    req(Pid,{self(),getall}).
proxy_setnext(Pid,Data) when is_list(Data) ->
    req(Pid,{self(),next,Data}).
proxy_quit(Pid) ->
    req(Pid,{self(),quit}).

req(Pid,Mess) ->
    Pid ! Mess,
    receive
	{Pid, Answer} ->
	    Answer
    after 5000 ->
	    exit(timeout)
    end.

io_server_proxy(State) -> 
    receive
        {io_request, From, ReplyAs, Request} ->
            case request(Request,State) of
                {Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
                    reply(From, ReplyAs, Reply),
                    io_server_proxy(NewState);
                {stop, Reply, _NewState} ->
                    reply(From, ReplyAs, Reply),
                    exit(Reply)
            end;
        %% Private message
        {From, next, Data} ->
            From ! {self(), ok},
            io_server_proxy(State#state{nxt = Data});
        {From, getall} ->
            From ! {self(), lists:reverse(State#state.q)},
            io_server_proxy(State#state{q=[]});
        {From, quit} ->
            From ! {self(), lists:reverse(State#state.q)},
	    ok;
        _Unknown ->
            io_server_proxy(State)
    end.

reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    {ok, ok, State#state{q=[{put_chars, Encoding, Chars} | State#state.q ]}};
request({put_chars, Encoding, Module, Function, Args}, State) ->
    {ok, ok, State#state{q=[{put_chars, Encoding, Module, Function, Args} | 
			    State#state.q ]}};
request({get_until, Encoding, Prompt, M, F, As}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), State#state{nxt = eof, q = [{get_until, Encoding, Prompt, M, F, As} | State#state.q]}};
request({get_chars, Encoding, Prompt, N}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), State#state{nxt = eof, 
									   q = [{get_chars, Encoding, Prompt, N} |
										State#state.q]}};
request({get_line, Encoding, Prompt}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), 
     State#state{nxt = eof, 
		 q = [{get_line, Encoding, Prompt} | 
		      State#state.q]}};
request({get_geomentry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) when Opts =:= [{binary, false}]; Opts =:= [list] ->
    {ok, ok, State#state{q=[{setopts, Opts} | State#state.q ], mode = list}};
request({setopts, Opts}, State) when Opts =:= [{binary, true}]; Opts =:= [binary] ->
    {ok, ok, State#state{q=[{setopts, Opts} | State#state.q ], mode = binary}};
request(getopts, State) ->
    {ok, case State#state.mode of
	     list -> [{binary,false}];
	     binary -> [{binary, true}]
	 end, State#state{q=[getopts | State#state.q ]}};
request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, ok, State}).

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], State) ->
    State.

convert(Atom,_,_) when is_atom(Atom) ->
    Atom;
convert(Data, unicode, list) ->
    unicode:characters_to_list(Data,unicode);
convert(Data, latin1, list) ->
    try
	L = unicode:characters_to_list(Data, unicode),
	[ true = Ch =< 255 || Ch <- L ],
	L
    catch
	_:_ ->
	    {error, {cannot_convert, unicode, latin1}}
    end;
convert(Data, unicode, binary) ->
    unicode:characters_to_binary(Data,unicode,unicode);
convert(Data, latin1, binary) ->
    case unicode:characters_to_binary(Data, unicode, latin1) of
	Bin when is_binary(Bin) ->
	    Bin;
	_ ->
	    {error, {cannot_convert, unicode, latin1}}
    end.

atom2list(A) ->
    lists:flatten(io_lib:format("~w", [A])).

chomp([]) ->
    [];
chomp([$\n]) ->
    [];
chomp([H|T]) ->
    [H|chomp(T)];
chomp(<<>>) ->
    <<>>;
chomp(<<$\n>>) ->
    <<>>;
chomp(<<Ch,Rest/binary>>) ->
    X = chomp(Rest),
    <<Ch,X/binary>>;
chomp(Atom) ->
    Atom.

do(Fun) ->
    {_,Ref} = spawn_monitor(fun() ->
				    exit(Fun())
			    end),
    Ref.

done(Ref) ->
    receive
	{'DOWN',Ref,process,_,Result} ->
	    Result
    end.
