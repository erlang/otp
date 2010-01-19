-module(cc).
-export([epp/1, epp/2, dbg/1, dbg/2, cvr/1, cvr/2]).
-export([p/2, pp/2]).

%% epp(Module) - Creates Module.epp which contains all forms of Module
%%               as obtained by using epp.
%%
%% dbg(Module) - Creates Module.dbg which contains all forms of Module
%%               as obtained by using beam_lib:chunks/2.
%%
%% cvr(Module) - Creates Module.cvr which contains all forms of Module
%%               as obtained by using cover:transform/3.
%%

epp(Module) ->
    epp(Module, p).
epp(Module, P) ->
    File = atom_to_list(Module)++".erl",
    {ok,Cwd} = file:get_cwd(),
    {ok, Fd1} = epp:open(File, [Cwd], []),
    {ok, Fd2} = file:open(atom_to_list(Module)++".epp", write),

    epp(Fd1, Fd2, P),
    
    epp:close(Fd1),
    file:close(Fd2),
    ok.

epp(Fd1, Fd2, P) ->
    case epp:parse_erl_form(Fd1) of
	{ok, {attribute,Line,Attr,Data}} ->
	    epp(Fd1, Fd2, P);
	{ok, Form} when P==p ->
	    io:format(Fd2, "~p.~n", [Form]),
	    epp(Fd1, Fd2, P);
	{ok, Form} when P==pp ->
	    io:format(Fd2, "~p.~n", [erl_pp:form(Form)]),
	    epp(Fd1, Fd2, P);
	{eof, Line} ->
	    ok
    end.

cvr(Module) ->
    cvr(Module, p).
cvr(Module, P) ->
    case beam_lib:chunks(Module, [abstract_code]) of
	{ok, {Module, [{abstract_code, no_abstract_code}]}} ->
	    {error, {no_debug_info,Module}};
	{ok, {Module, [{abstract_code, {Vsn, Forms}}]}} ->
	    Vars = {vars,Module,Vsn, [],
		    undefined, undefined, undefined, undefined, undefined,
		    undefined,
		    false},
	    {ok, TForms, _Vars2} = cover:transform(Forms, [], Vars),
	    File = atom_to_list(Module)++".cvr",
	    apply(?MODULE, P, [File, TForms]);
	Error ->
	    Error
    end.

dbg(Module) ->
    dbg(Module, p).
dbg(Module, P) ->
    case beam_lib:chunks(Module, [abstract_code]) of
	{ok, {Module, [{abstract_code, no_abstract_code}]}} ->
	    {error, {no_debug_info,Module}};
	{ok, {Module, [{abstract_code, {Vsn, Forms}}]}} ->
	    File = atom_to_list(Module)++".dbg",
	    apply(?MODULE, P, [File, Forms]);
	Error ->
	    Error
    end.

p(File, Forms) ->
    {ok, Fd} = file:open(File, write),
    lists:foreach(fun(Form) ->
			  io:format(Fd, "~p.~n", [Form])
		  end,
		  Forms),
    file:close(Fd).

pp(File, Forms) ->
    {ok, Fd} = file:open(File, write),
    lists:foreach(fun(Form) ->
			  io:format(Fd, "~s", [erl_pp:form(Form)])
		  end,
		  Forms),
    file:close(Fd).
