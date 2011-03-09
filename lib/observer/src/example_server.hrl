%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @doc Configuration for trace cases
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: 
%%% * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. 
%%% * Neither the name of the Erlang Training & Consulting nor the names of its
%%% contributors may be used to endorse or promote products
%%% derived from this software without specific prior written permission.
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-type proc_spec() :: pid() | list(pid()) | all | existing | new.
-type flag_spec() :: atom() | list(atom).
-type extended_match_spec() :: [] | return | caller | {codestr, string()} | list().
-type merge_spec() :: {function(), term()} | undefined.

-record(pattern,
        {module         :: atom(),
         function = '_' :: '_' | atom(), 
         arity = '_'    :: '_' | list(term()), 
         matchspec = [] :: extended_match_spec()
                           }).

-record(flags, 
        {scope          :: flag_spec(),
         flags          :: proc_spec()
                           }).

-record(merge_conf, 
        {function        :: merge_spec(),
         comment         :: string()
                            }).
                  
-record(trace_case,
        {case_id        :: undefined | integer(),
	 all_traces = []:: [tuple()],
         patterns       :: list(#pattern{}), 
         trace_flag     :: #flags{},
         nodes = []     :: list(atom()),
         merge_confs    :: list(#merge_conf{}),
         fetch_dir      :: undefined | string(), 
	 trace_opts = []:: list(),
         comment        :: string()
        }).
