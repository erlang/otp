#!/usr/bin/env escript
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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
-module(markdown_to_man).
-export([main/1]).

main(Args) ->
    try
        case parse_args(Args, ".", []) of
            {_,[]} ->
                ok;
            {OutDir,[_|_]=Files} ->
                convert_files(Files, OutDir)
        end
    catch
        throw:{error,Error} ->
            io:put_chars(standard_error, Error)
    end.

parse_args(["-o",OutDir|As], _OutDir, FilesAcc) ->
    parse_args(As, OutDir, FilesAcc);
parse_args([F|Fs], OutDir, FilesAcc) ->
    parse_args(Fs, OutDir, [F|FilesAcc]);
parse_args([], OutDir, FilesAcc) ->
    {OutDir,lists:reverse(FilesAcc)}.

convert_files([F|Fs], OutDir) ->
    convert_file(F, OutDir),
    convert_files(Fs, OutDir);
convert_files([], _) ->
    ok.

convert_file(Name, OutDir) ->
    Base0 = filename:rootname(filename:basename(Name), ".md"),
    Base = case lists:reverse(Base0) of
               "dmc_" ++ Base1 ->
                   lists:reverse(Base1);
               _ ->
                   fail(~'~p: file name does not end in "_cmd.md"',
                        [Name])
           end,
    OutFile = filename:join(OutDir, Base ++ ".1"),
    _ = filelib:ensure_dir(OutFile),
    case file:read_file(Name) of
        {ok,Markdown} ->
            Man = convert(Markdown),
            case file:write_file(OutFile, Man) of
                ok ->
                    ok;
                {error,Reason0} ->
                    Reason = file:format_error(Reason0),
                    fail(io_lib:format("~ts: write failed: ~ts",
                                       [OutFile,Reason]))
            end;
        {error,Reason0} ->
            Reason = file:format_error(Reason0),
            fail(io_lib:format("~ts: ~ts", [Name,Reason]))
    end.

fail(Format, Args) ->
    fail(io_lib:format(Format, Args)).

fail(String) ->
    E = io_lib:format("~p: ~ts\n", [?MODULE,String]),
    throw({error,E}).

convert(Markdown) when is_binary(Markdown) ->
    Items = shell_docs_markdown:parse_md(Markdown),
    Name = get_name(Items),
    Prelude = [~'.TH ', Name, ~' 1 "erts ',
               erlang:system_info(version),
               ~'" "Ericsson AB" "User Commands"\n'],
    iolist_to_binary([Prelude|conv(Items)]).

get_name([{h1,_,[Name]}|_]) when is_binary(Name) ->
    Name.

conv([{h1,_,[Name]},
      {p,_,[ShortDesc0]},
      {h2,_,[~"Synopsis"]},
      Synopsis0,
      {h2,_,[~"Description"]}|T]) when is_binary(ShortDesc0) ->
    ShortDesc = string:trim(ShortDesc0, trailing, [$., $\s]),
    Synopsis = strip_formatting(Synopsis0),
    [~".SH NAME\n",
     Name,~B" \- ",ShortDesc,$\n,
     ~".SH SYNOPSIS\n",
     Synopsis,$\n,
     ~".SH DESCRIPTION\n"|format(T)];
conv([{h1,_,[Name]},
      {p,_,[ShortDesc0]},
      {h2,_,[~"Description"]}|T]) when is_binary(ShortDesc0) ->
    ShortDesc = string:trim(ShortDesc0, trailing, [$., $\s]),
    [~".SH NAME\n",
     Name,~B" \- ",ShortDesc,$\n,
     ~".SH DESCRIPTION\n"|format(T)];
conv([{h1,_,[Head]}|T]) ->
    Name = ~".SH NAME\n",
    Desc = ~".SH DESCRIPTION\n",
    [Name,Head,$\n,Desc|format(T)].

format(Is) ->
    [[format_one(I),$\n] || I <- Is].

format_one({blockquote,_,Qs0}) ->
    [{h4,_,Head0},{p,_,Text}|Ps] = Qs0,
    Head = string:uppercase(<<(string:trim(Head0))/binary,": ">>),
    L = [{p,[],[{em,[],Head}|Text]}|Ps],
    format(L);
format_one({h1,_,Hs}) ->
    [~'.SH "',Hs,~'"\n'];
format_one({h2,_,Hs}) ->
    [~'.SS "',Hs,~'"\n'];
format_one({p,_,Ps}) ->
    format_p(Ps);
format_one({pre,_,Ps}) ->
    format_pre(Ps);
format_one({ul,_,Ul}) ->
    format_ul(Ul).

format_p(Is0) ->
    Is = [format_p_item(I) || I <- Is0],
    [~".PP\n",Is,$\n].

format_p_item({code,_,Text}) ->
    [~B"\fI",Text,~B"\fR"];
format_p_item({em,_,Text}) ->
    [~B"\fB",format_p_item(Text),~B"\fR"];
format_p_item({i,_,Text}) ->
    [~B"\fI",format_p_item(Text),~B"\fR"];
format_p_item([H|T]) ->
    [format_p_item(H)|format_p_item(T)];
format_p_item([]) ->
    [];
format_p_item(Text) when is_binary(Text) ->
    Text.

format_pre(Ps0) ->
    Ps = [format_pre_item(P) || P <- Ps0],
    [~".IP\n.nf\n",Ps,$\n,~".fi\n"].

format_pre_item({code,_,Text}) ->
    Text.

format_ul(UL) ->
    [format_ul_item(I) || I <- UL].

format_ul_item({li,_,Ps0}) ->
    case Ps0 of
        [{p,_,[Head,<<" - ",Text0/binary>>|Items]}|T] ->
            Text = string:trim(Text0, leading),
            [strip_formatting(Head),$\n,
             ~".RS 2\n",
             Text,$\n,
             [format_p_item(I) || I <- Items],
             $\n,
             format(T),$\n,
             ~".RE\n"
            ];
        _ ->
            B = ~"""

                 .sp
                 .RS 4
                 .ie n \{\
                 \h'-04'\(bu\h'+03'\c
                 .\}
                 .el \{\
                 .sp -1
                 .IP \(bu 2.3
                 .\}
                 """,
            [B,format(Ps0),~".RE\n"]
    end.

strip_formatting({_,_,[_|_]=L}) ->
    strip_formatting(L);
strip_formatting([H|T]) ->
    [strip_formatting(H),strip_formatting(T)];
strip_formatting([]) ->
    [];
strip_formatting(<<".",_/binary>> = Bin) ->
    [~B"\&",Bin];
strip_formatting(Bin) when is_binary(Bin) ->
    Bin.
