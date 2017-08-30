%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(ic_file).

-include_lib("ic/src/ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([filename_push/4, filename_pop/2, open/2, close/1, remove_ext/1, join/2,
	 add_dot_erl/1, add_dot_hrl/1, add_dot_c/1, add_dot_h/1, add_dot_java/1, 
	 add_dot_idl/1, javaInterfaceFilePush/3, javaInterfaceFilePop/1, 
	 createDirectory/2, createJavaDirectory/2, open_java_file/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: filename_push
%%
%% Pushes a file name, can also push ignore in which case means that
%% no files should ever be opened at this scope. Note that empty in
%% the file descriptor entries means that the file just isn't open
%% yet.
%%-----------------------------------------------------------------
filename_push(G, _N, ignore, _) ->
    G#genobj{stubfile=[ignore | G#genobj.stubfile],
	     stubfiled=[ignore | G#genobj.stubfiled],
	     skelfile=[ignore | G#genobj.skelfile],
	     skelfiled=[ignore | G#genobj.skelfiled],
    	     includefile=[ignore | G#genobj.includefile],
	     includefiled=[ignore | G#genobj.includefiled]};

filename_push(G, N, X, Lang) ->
    Fullname = [ic_forms:get_id2(X) | N],
    EName0 = ic_util:to_undersc(Fullname),
    
    DoGen = ic_genobj:do_gen(G),

    ImplName = find_impl_name(G, Fullname),

    {StubName, EName} =
	case Lang of
	    erlang ->
		{join(ic_options:get_opt(G, stubdir), add_dot_erl(EName0)), 
		 EName0};
	    erlang_template ->
		{join(ic_options:get_opt(G, stubdir), add_dot_erl(ImplName)), 
		 ImplName};
	    c ->
		{join(ic_options:get_opt(G, stubdir), add_dot_c(EName0)), 
		 EName0};
	    c_server ->
		{join(ic_options:get_opt(G, stubdir), add_dot_c(EName0++"__s")), 
		 EName0};
	    erlang_template_no_gen ->
		{undefined, EName0};
	    erlang_no_stub ->
		{undefined, EName0};
	    c_no_stub ->
		{undefined, EName0}; 
	    c_server_no_stub ->
		{undefined, EName0}
	end,
    Stub = if DoGen==true -> 
		   case StubName of
		       undefined ->
			   ignore;
		       _ ->
			   ic_codegen:emit_stub_head(G, open(empty, StubName), EName, Lang)
		   end;
	      true -> ignore end,
    
    HrlName = case Lang of
		  erlang_template ->
		      ignore;
		  erlang_template_no_gen ->
		      ignore;
		  erlang ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir), add_dot_hrl(EName)),
			      ignore);
		  c ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir), add_dot_h(EName)),
			      ignore);
		  c_server ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir),
				   add_dot_h(EName++"__s")),
			      ignore);
		  erlang_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir), add_dot_hrl(EName)),
			      ignore);
		  c_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir), add_dot_h(EName)),
			      ignore);
		  c_server_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(ic_options:get_opt(G, stubdir),
				   add_dot_h(EName++"__s")),
			      ignore)
	      end,
    Hrl = if  DoGen==true -> 
		  case Lang of
		      erlang_template ->
			  ignore;
		      erlang_template_no_gen ->
			  ignore;
		      erlang_no_stub ->
			  ic_codegen:emit_hrl_head(G, open(empty, HrlName),
						   EName, erlang);
		      c_no_stub ->
			  ic_codegen:emit_hrl_head(G, open(empty, HrlName),
						   EName, c);
		      c_server_no_stub ->
			  ic_codegen:emit_hrl_head(G, open(empty, HrlName),
						   EName, c_server);
		      _ ->
			  ic_codegen:emit_hrl_head(G, open(empty, HrlName),
						   EName, Lang)
		  end;
	      true -> ignore end,
    
    G#genobj{impl=ImplName,
	     stubfile=[StubName | G#genobj.stubfile],
	     stubfiled=[Stub | G#genobj.stubfiled],
	     includefile=[HrlName | G#genobj.includefile],
	     includefiled=[Hrl | G#genobj.includefiled]}.

%%-----------------------------------------------------------------
%% Func: join/2
%%
%% Special version of filename join.
%%-----------------------------------------------------------------
join([], File) ->
    File;
join(Path, File) ->
    filename:join(Path, File).


%%-----------------------------------------------------------------
%% Func: filename_pop/2
%%-----------------------------------------------------------------
filename_pop(G, Lang) ->
%%    io:format("Popped file names: ~p~n", [hd(G#genobj.stubfile)]),
%%    case is_skelfile_open(G) of
%%	true -> emit_skel_footer(G);
%%	false -> ok end,
%%    close(hd(G#genobj.skelfiled)),
    close(hd(G#genobj.stubfiled)),
    ic_codegen:emit_hrl_foot(G, Lang),
    close(hd(G#genobj.includefiled)),
    G#genobj{stubfile=tl(G#genobj.stubfile),
	     stubfiled=tl(G#genobj.stubfiled),
%%	     skelfile=tl(G#genobj.skelfile),
%%	     skelfiled=tl(G#genobj.skelfiled),
    	     includefile=tl(G#genobj.includefile),
	     includefiled=tl(G#genobj.includefiled)}.



%%-----------------------------------------------------------------
%% Func: javaInterfaceFilePush/3
%%-----------------------------------------------------------------
javaInterfaceFilePush(G, N, X) ->
    Name = ic_forms:get_java_id(X),
    {InterfaceFd, InterfaceFileName} = open_java_file(G, N, Name),
    
    StubClassName = "_" ++ Name ++ "Stub",
    {StubFd, StubFileName} = open_java_file(G, N, StubClassName),
    
    SkelClassName = "_" ++ Name ++ "ImplBase",
    {SkelFd, SkelFileName} = open_java_file(G, N, SkelClassName),
    
    HelperClassName = Name ++ "Helper",
    {HelperFd, HelperFileName} = open_java_file(G, N, HelperClassName),
    
    HolderClassName = Name ++ "Holder",
    {HolderFd, HolderFileName} = open_java_file(G, N, HolderClassName),
    
    G#genobj{
      interfacefile=[InterfaceFileName | G#genobj.interfacefile],
      interfacefiled=[InterfaceFd | G#genobj.interfacefiled],
      stubfile=[StubFileName | G#genobj.stubfile],
      stubfiled=[StubFd | G#genobj.stubfiled],
      skelfile=[SkelFileName | G#genobj.skelfile],
      skelfiled=[SkelFd | G#genobj.skelfiled],
      helperfile=[HelperFileName | G#genobj.helperfile],
      helperfiled=[HelperFd | G#genobj.helperfiled],
      holderfile=[HolderFileName | G#genobj.holderfile],
      holderfiled=[HolderFd | G#genobj.holderfiled]}.





%%-----------------------------------------------------------------
%% Func: javaInterfaceFilePop/1
%%-----------------------------------------------------------------
javaInterfaceFilePop(G) ->
    close(hd(G#genobj.interfacefiled)),
    close(hd(G#genobj.stubfiled)),
    close(hd(G#genobj.skelfiled)),
    close(hd(G#genobj.helperfiled)),
    close(hd(G#genobj.holderfiled)),
    G#genobj{
      interfacefile=tl(G#genobj.interfacefile),
      interfacefiled=tl(G#genobj.interfacefiled),
      stubfile=tl(G#genobj.stubfile),
      stubfiled=tl(G#genobj.stubfiled),
      skelfile=tl(G#genobj.skelfile),
      skelfiled=tl(G#genobj.skelfiled),
      helperfile=tl(G#genobj.helperfile),
      helperfiled=tl(G#genobj.helperfiled),
      holderfile=tl(G#genobj.holderfile),
      holderfiled=tl(G#genobj.holderfiled)}.

%%-----------------------------------------------------------------
%% Func: createDirectory/2 
%%-----------------------------------------------------------------
createDirectory(_G, []) ->
    ok;
createDirectory(G, Scope) ->
    Path = ic_file:join(ic_options:get_opt(G, stubdir), ic_pragma:slashify(Scope)),
    case file:make_dir(Path) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	{error, Reason} ->
	    ic_error:fatal_error(G, {create_dir, Path, Reason})
    end.


%%-----------------------------------------------------------------
%% Func: createJavaDirectory/2 
%%-----------------------------------------------------------------
createJavaDirectory(_G, []) ->
    ok;
createJavaDirectory(G, Scope) ->
    JavaScope = ic_util:adjustScopeToJava(G,Scope),
    Path = ic_file:join(ic_options:get_opt(G, stubdir), ic_pragma:slashify(JavaScope)),
    case file:make_dir(Path) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	{error, Reason} ->
	    ic_error:fatal_error(G, {create_dir, Path, Reason})
    end.
    



%%-----------------------------------------------------------------
%% Func: createJavaFileName/3
%%-----------------------------------------------------------------
createJavaFileName(G, Scope, FName) ->
    JavaScope = ic_util:adjustScopeToJava(G,Scope),
    join(ic_options:get_opt(G, stubdir), 
	 ic_pragma:slashify([FName++".java"|JavaScope])).

%%-----------------------------------------------------------------
%% Func: close/2 (used to be file_close)
%%-----------------------------------------------------------------
close(empty) -> ok;
close(ignore) -> ok;
close(Fd) -> 
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func: remove_ext/1
%%-----------------------------------------------------------------
remove_ext(File) ->
    filename:rootname(filename:basename(File)).

%%-----------------------------------------------------------------
%% Func: open/2 (used to be file_open)
%%-----------------------------------------------------------------
open(_, ignore) -> ignore;
open(empty, Name) -> 
    case file:open(Name, [raw, binary, write]) of
	{ok, Fd} ->
	    Fd;
	{error, Reason} ->
	    exit({error, Reason})
%%		ic_error:fatal_error(G, {open_file, Name, Reason})
    end.

%%-----------------------------------------------------------------
%% Func: open_java_file/3
%%-----------------------------------------------------------------
open_java_file(G, N, Name) ->
    createJavaDirectory(G, N),
    FName =  createJavaFileName(G, N, Name),
    case file:open(FName, [raw, binary, write]) of
        {ok, Fd} ->
	    ic_codegen:emit_stub_head(G, Fd, Name, java),
	    emit_package(G, N, Fd),
            {Fd, FName};
        {error, Reason} ->
	    ic_error:fatal_error(G, {open_file, FName, Reason})
    end.

%%-----------------------------------------------------------------
%% Func: emit_package/3
%%-----------------------------------------------------------------
emit_package(_G, [], _Fd) ->
    ok;
emit_package(G, N, Fd) ->
    ic_codegen:emit(Fd, "package ~s;\n", [ic_util:to_dot(G,N)]),
    ic_codegen:nl(Fd).

%%-----------------------------------------------------------------
%% Func: add_dot_erl/1 
%%-----------------------------------------------------------------
add_dot_erl(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $r, $e, $. | _Rest] -> 
	    File;
	_ ->
	    File ++ ".erl"
    end.

%%-----------------------------------------------------------------
%% Func: add_dot_hrl/1 
%%-----------------------------------------------------------------
add_dot_hrl(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $r, $h, $. | _Rest] -> 
	    File;
	_ -> 
	    File ++ ".hrl"
    end.

%%-----------------------------------------------------------------
%% Func: add_dot_c/1 
%%-----------------------------------------------------------------
add_dot_c(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$c, $. | _Rest] -> 
	    File;
	_ -> 
	    File ++ ".c"
    end.

%%-----------------------------------------------------------------
%% Func: add_dot_h/1 
%%-----------------------------------------------------------------
add_dot_h(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$h, $. | _Rest] -> 
	    File;
	_ -> 
	    File ++ ".h"
    end.

%%-----------------------------------------------------------------
%% Func: add_dot_java/1 
%%-----------------------------------------------------------------
add_dot_java(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$a, $v, $a, $j, $. | _Rest] -> 
	    File;
	_ ->
	    File ++ ".java"
    end.

%%-----------------------------------------------------------------
%% Func: add_dot_idl/1 
%%-----------------------------------------------------------------
add_dot_idl(F) ->
    File = ic_util:to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $d, $i, $. | _Rest] -> 
	    File;
	_ ->
	    File ++ ".idl"
    end.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% File handling stuff
%%
%% 
%%	Shall open a file for writing. Also sets up the generator with
%%	usefull bits of information
%%
%%--------------------------------------------------------------------
find_impl_name(G, Name) ->
    N1 = ic_util:to_colon(Name),
    N2 = ic_util:to_undersc(Name),
    case {ic_options:get_opt(G, {impl, N1}),
	  ic_options:get_opt(G, {impl, N2})} of
	{false, false} ->
	    case {ic_options:get_opt(G, {impl, "::"++N1}),
		  ic_options:get_opt(G, {impl, N2})} of
		{false, false} -> N2 ++ "_impl";
		{X, _Y} when X /= false -> ic_util:to_list(X);
		{_X, Y} when Y /= false -> ic_util:to_list(Y)
	    end;
	{X, _Y} when X /= false -> ic_util:to_list(X);
	{_X, Y} when Y /= false -> ic_util:to_list(Y)
    end.
