%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(ic_codegen).

-include_lib("ic/src/ic.hrl").
-include_lib("ic/src/icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([emit/2, emit/3]).
-export([emit_c_enc_rpt/4, emit_c_dec_rpt/4]).
-export([comment/2, comment/3, comment/4, comment_inlined/5, comment_prefixed/4]).
-export([mcomment/2, mcomment/3, mcomment_inlined/5, mcomment_prefixed/3]).
-export([mcomment_light/2, mcomment_light/3, mcomment_light_inlined/5, mcomment_light_prefixed/3]).
-export([nl/1, export/2]).
-export([record/5]).
-export([emit_stub_head/4, emit_hrl_head/4, emit_hrl_foot/2]).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%% Emit output as a formatted string, (old emit)
%%--------------------------------------------------------------------
emit(nil, _) -> ok;
emit(Fd, Str) ->
    file:write(Fd, Str).

emit(nil, _, _) -> ok;
emit(Fd, Fmt, Args) ->
    file:write(Fd, io_lib:format(Fmt, Args)).

emit_c_enc_rpt(Fd, Prefix, Fmt, Args) ->
    emit(Fd, Prefix ++ "OE_RPT_ERR(\"Encode error: " ++ Fmt ++ "\");\n", Args).

emit_c_dec_rpt(Fd, Prefix, Fmt, Args) ->
    emit(Fd, Prefix ++ "OE_RPT_ERR(\"Decode error: " ++ Fmt ++ "\");\n", Args).

%%--------------------------------------------------------------------
%% Emit comments
%%--------------------------------------------------------------------
comment(Fd, C) ->
    comment_prefixed(Fd, C, [], "%%").

comment(Fd, C, A) ->
    comment_prefixed(Fd, C, A, "%%").

comment(Fd, C, A, c) -> 
    comment_inlined(Fd, C, A, "/*", "*/");
comment(Fd, C, A, erl) -> 
    comment_prefixed(Fd, C, A, "%%");
comment(Fd, C, A, java) -> 
    comment_prefixed(Fd, C, A, "//");
%% Should be removed after a check if it's used !!!!! (LTH)
comment(Fd, C, A, CommentSequence) when is_list(CommentSequence) ->
    comment_prefixed(Fd, C, A, CommentSequence).

comment_inlined(Fd, C, A, Start, End) ->
     emit(Fd, Start ++ " " ++ C ++ " " ++ End ++"\n", A).

comment_prefixed(Fd, C, A, Prefix) ->
     emit(Fd, Prefix ++ " " ++ C ++ "\n", A).

%%--------------------------------------------------------------------
%% Emit multiline comments with nice delimiters
%%--------------------------------------------------------------------
mcomment(Fd, List) ->
    mcomment_prefixed(Fd, List, "%%").

mcomment(Fd, List, c) ->
    mcomment_inlined(Fd, List, "/*", "*/", " *");
mcomment(Fd, List, erl) ->
    mcomment_prefixed(Fd, List, "%%");
mcomment(Fd, List, java) ->
    mcomment_prefixed(Fd, List, "//").

mcomment_inlined(Fd, List, Start, End, Intermediate) ->
    emit(Fd, Start ++
	 "------------------------------------------------------------\n"),
    emit(Fd, Intermediate ++ "\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], Intermediate) end, List),
    emit(Fd, Intermediate ++ "\n"),
    emit(Fd, Intermediate ++
	 "------------------------------------------------------------" ++ End ++ "\n"),
    ok.
mcomment_prefixed(Fd, List, Prefix) ->
    emit(Fd, Prefix ++
	 "------------------------------------------------------------\n"),
    emit(Fd, Prefix ++ "\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], Prefix) end, List),
    emit(Fd, Prefix ++ "\n"),
    emit(Fd, Prefix ++
	 "------------------------------------------------------------\n"),
    ok.


%%--------------------------------------------------------------------
%% Emit multiline comments with nice delimiters as above but a 
%% little lighter
%%--------------------------------------------------------------------
mcomment_light(Fd, List) ->
    mcomment_light_prefixed(Fd, List, "%%").

mcomment_light(Fd, List, c) ->
    mcomment_light_inlined(Fd, List, "/*", " */", " *");
mcomment_light(Fd, List, erl) ->
    mcomment_light_prefixed(Fd, List, "%%");
mcomment_light(Fd, List, java) ->
    mcomment_light_prefixed(Fd, List, "//");
%% Should be removed after a check if it's used !!!!! (LTH)
mcomment_light(Fd, List, Prefix) when is_list(Prefix) ->
    mcomment_light_prefixed(Fd, List, Prefix).
    
mcomment_light_inlined(Fd, List, Start, End, Intermediate) ->
    emit(Fd, "\n" ++ Start ++ "\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], Intermediate) end, List),
    emit(Fd, End ++ "\n"),
    ok.

mcomment_light_prefixed(Fd, List, Prefix) ->
    emit(Fd, Prefix),
    lists:foreach(fun(C) -> comment(Fd, C, [], Prefix) end, List),
    emit(Fd, Prefix ++ "\n"),
    ok.

%%--------------------------------------------------------------------
%% New line
%%--------------------------------------------------------------------
nl(Fd) ->
    emit(Fd, "\n").


%%--------------------------------------------------------------------
-define(IFRIDFIELD(G), ic_util:mk_name(G, "ID")).

%%--------------------------------------------------------------------
%% Emit record definitions for erlang
%%--------------------------------------------------------------------
record(G, X, Name, _IFRID, Recs) when is_record(X, struct) ->
    F = ic_genobj:hrlfiled(G),
    emit(F, "-record(~p, {~p", [ic_util:to_atom(Name),hd(Recs)]),
    lists:foreach(fun(Y) -> emit(F, ", ~p", [Y]) end, tl(Recs)),
    emit(F, "}).\n");
record(G, X, Name, _IFRID, _Recs) when is_record(X, union) ->
    F = ic_genobj:hrlfiled(G),
    emit(F, "-record(~p, {label, value}).\n",[ic_util:to_atom(Name)]);
record(G, _X, Name, IFRID, Recs) when length(Recs) > 3 ->
    F = ic_genobj:hrlfiled(G),
    emit(F, "-record(~p,~n        {~p=~p", 
	 [ic_util:to_atom(Name), ic_util:to_atom(?IFRIDFIELD(G)), IFRID]),
    rec2(F, "", ", ", Recs),
    emit(F, "}).\n");
record(G, _X, Name, IFRID, Recs) ->
    F = ic_genobj:hrlfiled(G),
    emit(F, "-record(~p, {~p=~p", [ic_util:to_atom(Name),
				   ic_util:to_atom(?IFRIDFIELD(G)),
				     IFRID]),
    lists:foreach(fun(Y) -> emit(F, ", ~p", [Y]) end, Recs),
    emit(F, "}).\n").


rec2(F, Align, Delim, [M1 , M2, M3 | Ms]) ->
    emit(F, "~s~s~p, ~p, ~p", [Delim, Align, M1, M2, M3]),
    rec2(F, "         ", ",\n", Ms);
rec2(F, Align, Delim, [M1 , M2]) ->
    emit(F, "~s~s~p, ~p", [Delim, Align, M1, M2]);
rec2(F, Align, Delim, [M]) ->
    emit(F, "~s~s~p", [Delim, Align, M]);
rec2(_F, _Align, _Delim, []) ->
    ok.


%%--------------------------------------------------------------------
%% Emit export lists for erlang
%%--------------------------------------------------------------------
export(F, [E1, E2, E3 | Exports]) ->
    emit(F, "-export([~s]).\n", [exp_list([E1, E2, E3])]),
    export(F, Exports);
export(_F, []) -> ok;
export(F, Exports) ->
    emit(F, "-export([~s]).\n", [exp_list(Exports)]).

exp_list([E1 | L]) ->
    exp_to_string(E1) ++ 
	lists:map(fun(E) -> ", " ++ exp_to_string(E) end, L).


exp_to_string({F,N}) -> io_lib:format("~p/~p", [ic_util:to_atom(F), N]).


%%--------------------------------------------------------------------
%% Emit Stub file header
%%--------------------------------------------------------------------
emit_stub_head(_G, ignore, _Name, _) -> ignore;
emit_stub_head(G, F1, Name, erlang) ->
    comment(F1, " coding: latin-1", []),
    mcomment(F1, stub_header(G, Name)),
    nl(F1),
    emit(F1, "-module(~p).\n", [list_to_atom(Name)]),
    emit(F1, "-ic_compiled(~p).\n", [compiler_vsn(?COMPILERVSN)]),
    emit(F1, "\n\n"), F1;
emit_stub_head(G, F1, Name, erlang_template) ->
    comment(F1, " coding: latin-1", []),
    ic_erl_template:emit_header(G, F1, Name),
    F1;
emit_stub_head(_G, F1, _Name, erlang_template_no_gen) ->
    F1;
emit_stub_head(G, F1, Name, c) ->
    mcomment(F1, stub_header(G, Name), c),
    emit(F1, "int ic_compiled_~s_~s;\n", [compiler_vsn(?COMPILERVSN), Name]),
    emit(F1, "\n\n"), F1;
emit_stub_head(G, F1, Name, c_server) ->
    CSName = [Name, "__s"],
    mcomment(F1, stub_header(G, CSName), c),
    emit(F1, "int ic_compiled_~s_~s;\n", [compiler_vsn(?COMPILERVSN), CSName]),
    emit(F1, "\n\n"), F1;
emit_stub_head(G, F1, Name, java) ->
    mcomment(F1, stub_header(G, Name), java),
    emit(F1, "\n\n"), F1.

stub_header(G, Name) ->
    ["Implementation stub file",
     "",
     io_lib:format("Target: ~s", [Name]),
     io_lib:format("Source: ~s", [ic_genobj:idlfile(G)]),
     io_lib:format("IC vsn: ~s", [?COMPILERVSN]),
     "",
     "This file is automatically generated. DO NOT EDIT IT."].

compiler_vsn(Vsn) ->
    lists:map(fun($.) -> $_;
		 (C) -> C
	      end, Vsn).

%%--------------------------------------------------------------------
%% Emit include file header
%%--------------------------------------------------------------------
%% Name is Fully scoped (undescore) name of interface or module    
emit_hrl_head(_G, ignore, _Name, _) -> ignore;
emit_hrl_head(G, Fd, Name, erlang) ->
    comment(Fd, " coding: latin-1", []),
    mcomment(Fd, ["Erlang header file" |
		  hrl_header(G, Name)]),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"_HRL"),
    emit(Fd, "-ifndef(~s).~n", [IfdefName]),
    emit(Fd, "-define(~s, true).~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd;
emit_hrl_head(G, Fd, Name, c) ->
    mcomment(Fd, ["C header file" |
		  hrl_header(G, Name)], c),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"_H"),
    emit(Fd, "#ifndef ~s~n", [IfdefName]),
    emit(Fd, "#define ~s ~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd;
emit_hrl_head(G, Fd, Name, c_server) ->
    mcomment(Fd, ["C header file" |
 		  hrl_header(G, [Name, "__s"])], c),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"__S_H"),
    emit(Fd, "#ifndef ~s~n", [IfdefName]),
    emit(Fd, "#define ~s ~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd.

hrl_header(G, Name) ->
    ["",
     io_lib:format("Target: ~s", [Name]),
     io_lib:format("Source: ~s", [ic_genobj:idlfile(G)]),
     io_lib:format("IC vsn: ~s", [?COMPILERVSN]),
     "",
     "This file is automatically generated. DO NOT EDIT IT."].




%%--------------------------------------------------------------------
%% Emit include file footer
%%--------------------------------------------------------------------
emit_hrl_foot(_G, erlang_template) ->
    ok;
emit_hrl_foot(_G, erlang_template_no_gen) ->
    ok;
emit_hrl_foot(G, erlang) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "-endif.\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, erlang_no_stub) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "-endif.\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_server) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_no_stub) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_server_no_stub) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end.










%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
