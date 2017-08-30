%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%%% Purpose : The GS object

-module(gstk_gs).

-export([mk_create_opts_for_child/4,
	 config/3,
	 read/3,
	 read_option/5,
	 option/5]).

-include("gstk.hrl").

%%----------------------------------------------------------------------
%% The GS object implementation
%%----------------------------------------------------------------------

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

config(DB, Gstkid, Opts) ->
    Cmd=gstk_generic:make_command(Opts,Gstkid,"",DB),
    gstk:exec(Cmd),
    ok.

read(DB, Gstkid, Opt) ->
    gstk_generic:read_option(DB, Gstkid, Opt).

% No options of my own
read_option(Option,Gstkid, _TkW,_DB,_) -> 
    {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}.

option(_Option, _Gstkid, _TkW, _DB,_) ->
    invalid_option.
