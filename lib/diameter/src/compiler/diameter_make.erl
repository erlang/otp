%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Driver for the encoder generator utility.
%%

-module(diameter_make).

-export([spec/0,
         hrl/0,
         erl/0]).

-spec spec() -> no_return().
-spec hrl()  -> no_return().
-spec erl()  -> no_return().

spec() ->
    make(spec).

hrl() ->
    make(hrl).

erl() ->
    make(erl).

%% make/1

make(Mode) ->
    Args = init:get_plain_arguments(),
    Opts = try options(Args) catch throw: help -> help(Mode) end,
    Files = proplists:get_value(files, Opts, []),
    lists:foreach(fun(F) -> from_file(F, Mode, Opts) end, Files),
    halt(0).

%% from_file/3

from_file(F, Mode, Opts) ->
    try to_spec(F, Mode, Opts) of
        Spec ->
            from_spec(F, Spec, Mode, Opts)
    catch
        error: Reason ->
            io:format("==> ~p parse failure:~n~p~n",
                      [F, {Reason, erlang:get_stacktrace()}]),
            halt(1)
    end.

%% to_spec/2

%% Try to read the input as an already parsed file or else parse it.
to_spec(F, spec, Opts) ->
    diameter_spec_util:parse(F, Opts);
to_spec(F, _, _) ->
    {ok, [Spec]} = file:consult(F),
    Spec.

%% from_spec/4

from_spec(File, Spec, Mode, Opts) ->
    try
        diameter_codegen:from_spec(File, Spec, Opts, Mode)
    catch
        error: Reason ->
            io:format("==> ~p codegen failure:~n~p~n~p~n",
                      [Mode, File, {Reason, erlang:get_stacktrace()}]),
            halt(1)
    end.

%% options/1

options(["-v" | Rest]) ->
    [verbose | options(Rest)];

options(["-o", Outdir | Rest]) ->
    [{outdir, Outdir} | options(Rest)];

options(["-i", Incdir | Rest]) ->
    [{include, Incdir} | options(Rest)];

options(["-h" | _]) ->
    throw(help);

options(["--" | Fs]) ->
    [{files, Fs}];

options(["-" ++ _ = Opt | _]) ->
    io:fwrite("==> unknown option: ~s~n", [Opt]),
    throw(help);

options(Fs) ->  %% trailing arguments
    options(["--" | Fs]).

%% help/1

help(M) ->
    io:fwrite("Usage: ~p ~p [Options] [--] File ...~n"
              "Options:~n"
              " -v              verbose output~n"
              " -h              shows this help message~n"
              " -o OutDir       where to put the output files~n"
              " -i IncludeDir   where to search for beams to import~n",
              [?MODULE, M]),
    halt(1).
