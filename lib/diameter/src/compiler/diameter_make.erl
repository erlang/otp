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
%% Module alternative to diameterc for dictionary compilation.
%%
%% Eg. 1> diameter_make:dict("mydict.dia").
%%
%%     $ erl -noshell \
%%           -boot start_clean \
%%           -s diameter_make dict mydict.dia \
%%           -s init stop
%%

-module(diameter_make).

-export([dict/1,
         dict/2,
         spec/1,
         spec/2]).

-type opt() :: {outdir|include|name|prefix|inherits, string()}
             | verbose
             | debug.

%% dict/1-2

-spec dict(string(), [opt()])
   -> ok.

dict(File, Opts) ->
    make(File,
         Opts,
         spec(File, Opts),
         [spec || _ <- [1], lists:member(debug, Opts)] ++ [erl, hrl]).

dict(File) ->
    dict(File, []).

%% spec/2

-spec spec(string(), [opt()])
   -> orddict:orddict().

spec(File, Opts) ->
    diameter_spec_util:parse(File, Opts).

spec(File) ->
    spec(File, []).

%% ===========================================================================

make(_, _, _, []) ->
    ok;
make(File, Opts, Spec, [Mode | Rest]) ->
    try diameter_codegen:from_spec(File, Spec, Opts, Mode) of
        ok ->
            make(File, Opts, Spec, Rest)
    catch
        error: Reason ->
            {error, {Reason, Mode, erlang:get_stacktrace()}}
    end.
