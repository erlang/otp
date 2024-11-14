#!/usr/bin/env escript
%% -*- erlang -*-

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

main(Args) ->
    argparse:run(Args, cli(), #{progname => scancode}).

cli() ->
    #{ help =>
           """
           Run 'scancode' with multiple options
           """,
       arguments => [ scan_option(),
                      template_option(),
                      prefix_option(),
                      file_or_dir() ],
       handler => fun scancode/1}.

approved() ->
    [ "mit", "agpl-3.0", "apache-2.0", "boost-1.0", "llvm-exception",
      "lgpl-2.1-plus", "cc0-1.0", "bsd-simplified", "bsd-new", "pcre",
      "fsf-free", "autoconf-exception-3.0", "mpl-1.1", "public-domain",
      "autoconf-simple-exception", "unicode", "tcl", "gpl-2.0 WITH classpath-exception-2.0",
      "zlib", "lgpl-2.0-plus WITH wxwindows-exception-3.1", "lgpl-2.0-plus",
      "openssl-ssleay", "cc-by-sa-3.0", "cc-by-4.0", "dco-1.1", "fsf-ap",
      "agpl-1.0-plus", "agpl-1.0", "agpl-3.0-plus", "classpath-exception-2.0",
      "ietf-trust"].

not_approved() ->
    ["gpl", "gpl-3.0-plus", "gpl-2.0", "gpl-1.0-plus", "unlicense",
     "erlangpl-1.1", "gpl-2.0-plus", "null"].


scan_option() ->
    #{name => scan_option,
      type => string,
      default => "cli",
      long => "-scan-option"}.

prefix_option() ->
    #{name => prefix,
      type => string,
      default => "",
      long => "-prefix"}.


file_or_dir() ->
    #{name => file_or_dir,
      type => string,
      required => true,
      long => "-file-or-dir"}.

template_option() ->
    #{name => template_path,
      type => string,
      default => "scripts/scan-code/template.txt",
      long => "-template-path"}.


scancode(#{ file_or_dir := FilesOrDirs}=Config) ->
    Files = string:split(FilesOrDirs, " ", all),
    scancode0(Files, Config).

scancode0([[]], _) ->
    ok;
scancode0(Files, Config) ->
    Results = lists:foldl(fun (File, Errors) ->
                                  Command = scancode(Config, File),
                                  case execute(Command, File) of
                                      {error, Err} ->
                                          [Err | Errors];
                                      ok ->
                                          Errors
                                  end
                          end, [], Files),
    case Results of
        [] ->
            ok;
        Errors ->
            error(Errors)
    end.

scancode(#{scan_option   := Options,
           prefix        := Prefix,
           template_path := TemplatePath}, File) ->
    "scancode -" ++ Options ++ " --custom-output - --custom-template " ++ TemplatePath ++  " " ++ Prefix ++ File.

execute(Command, File) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    Result = loop(Port, []),
    Ls = string:split(string:trim(Result, both), ",", all),

    case lists:filter(fun ([]) -> false; (_) -> true end, Ls) of
        [] ->
            {error, {File, no_license_found}};
        Ls1 ->
            NotApproved = lists:any(fun (License) -> lists:member(License, not_approved()) end, Ls1),
            case NotApproved of
                true ->
                    {error, {File, license_not_approved}};
                false ->
                    InPolicy = lists:all(fun (License) -> lists:member(License, approved()) end, Ls1),
                    case InPolicy of
                        false ->
                            %% this can happen if a license is
                            %% not in the approve/not_approved list
                            {error, {File, license_not_approved}};
                        true ->
                            ok
                    end
            end
    end.

loop(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            loop(Port, [Data|Acc]);
        {Port,{exit_status, _ExitStatus}} ->
            lists:flatten(lists:reverse(Acc))
    end.
