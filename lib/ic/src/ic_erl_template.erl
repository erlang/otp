%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(ic_erl_template).


-export([do_gen/3, emit_header/3]).

-import(ic_codegen, [emit/2, emit/3, nl/1]).

-include("icforms.hrl").
-include("ic.hrl").

-include_lib("stdlib/include/erl_compile.hrl").

-define(TAB, "             ").
-define(TAB2, "%             ").

-define(TEMPLATE_1_A,
	"%%----------------------------------------------------------------------\n"
	"%% <LICENSE>\n"
	"%% \n"
	"%%     $Id$\n"
	"%%\n"
	"%%----------------------------------------------------------------------\n"
	"%% Module       : ~s.erl\n"
	"%% \n"
	"%% Source       : ~s\n"
	"%% \n"
	"%% Description  : \n"
	"%% \n"
	"%% Creation date: ~s\n"
	"%%\n"
	"%%----------------------------------------------------------------------\n"
	"-module(~p).\n\n").

-define(TEMPLATE_1_B,
	"%%----------------------------------------------------------------------\n"
	"%% Internal Exports\n"
	"%%----------------------------------------------------------------------\n"
	"-export([init/1,\n"
	"         terminate/2,\n"
	"         code_change/3,\n"
	"         handle_info/2]).\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Include Files\n"
	"%%----------------------------------------------------------------------\n"
	"\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Macros\n"
	"%%----------------------------------------------------------------------\n"
	"\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Records\n"
	"%%----------------------------------------------------------------------\n"
	"-record(state, {}).\n\n"
	"%%======================================================================\n"
	"%% API Functions\n"
	"%%======================================================================\n").

-define(TEMPLATE_1_C,
	"%%======================================================================\n"
	"%% Internal Functions\n"
	"%%======================================================================\n"
	"%%----------------------------------------------------------------------\n"
	"%% Function   : init/1\n"
	"%% Arguments  : Env = term()\n"
	"%% Returns    : {ok, State}          |\n"
	"%%              {ok, State, Timeout} |\n"
	"%%              ignore               |\n"
	"%%              {stop, Reason}\n"
	"%% Raises     : -\n"
	"%% Description: Initiates the server\n"
	"%%----------------------------------------------------------------------\n"
	"init(_Env) ->\n"
	"\t{ok, #state{}}.\n\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Function   : terminate/2\n"
	"%% Arguments  : Reason = normal | shutdown | term()\n"
	"%%              State = term()\n"
	"%% Returns    : ok\n"
	"%% Raises     : -\n"
	"%% Description: Invoked when the object is terminating.\n"
	"%%----------------------------------------------------------------------\n"
	"terminate(_Reason, _State) ->\n"
	"\tok.\n\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Function   : code_change/3\n"
	"%% Arguments  : OldVsn = undefined | term()\n"
	"%%              State = NewState = term()\n"
	"%%              Extra = term()\n"
	"%% Returns    : {ok, NewState}\n"
	"%% Raises     : -\n"
	"%% Description: Invoked when the object should update its internal state\n"
	"%%              due to code replacement.\n"
	"%%----------------------------------------------------------------------\n"
	"code_change(_OldVsn, State, _Extra) ->\n"
	"\t{ok, State}.\n\n\n"
	"%%----------------------------------------------------------------------\n"
	"%% Function   : handle_info/2\n"
	"%% Arguments  : Info = normal | shutdown | term()\n"
	"%%              State = NewState = term()\n"
	"%% Returns    : {noreply, NewState}          |\n"
	"%%              {noreply, NewState, Timeout} |\n"
	"%%              {stop, Reason, NewState}\n"
	"%% Raises     : -\n"
	"%% Description: Invoked when, for example, the server traps exits.\n"
	"%%----------------------------------------------------------------------\n"
	"handle_info(_Info, State) ->\n"
	"\t{noreply, State}.\n\n\n").

-define(TEMPLATE_2_A,
	"%%% #0.    BASIC INFORMATION\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% %CCaseFile  : ~s.erl %\n"
	"%%% Author      : \n"
	"%%% Description : \n"
	"%%%\n"
	"%%% Modules used: \n"
	"%%%\n"
	"%%%\n"
	"%%% ----------------------------------------------------------------------\n"
	"-module(~p).\n"
	"-author('unknown').\n"
	"-id('').\n"
	"-vsn('').\n"
	"-date('~s').\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% Template Id: <ID>\n"
	"%%%\n"
	"%%% #Copyright (C) 2004\n"
	"%%% by <COMPANY>\n"
	"%%% <ADDRESS>\n"
	"%%% <OTHER INFORMATION>\n"
	"%%% \n"
	"%%% <LICENSE>\n"
	"%%% \n"
	"%%% \n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #1.    REVISION LOG\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% Rev      Date       Name        What\n"
	"%%% -----    -------    --------    --------------------------\n"
	"%%% \n"
	"%%% ----------------------------------------------------------------------\n"
	"%%%\n"
	"%%% \n"
	"%%% #2.    EXPORT LISTS\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #2.1   EXPORTED INTERFACE FUNCTIONS\n"
	"%%% ----------------------------------------------------------------------\n").

-define(TEMPLATE_2_B,
	"%%% ----------------------------------------------------------------------\n"
	"%%% #2.2   EXPORTED INTERNAL FUNCTIONS\n"
	"%%% ----------------------------------------------------------------------\n"
	"-export([init/1,\n"
	"         terminate/2,\n"
	"         code_change/3,\n"
	"         handle_info/2]).\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #2.3   INCLUDE FILES\n"
	"%%% ----------------------------------------------------------------------\n"
	"\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #2.4   MACROS\n"
	"%%% ----------------------------------------------------------------------\n"
	"\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #2.5   RECORDS\n"
	"%%% ----------------------------------------------------------------------\n"
	"-record(state, {}).\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #3.    CODE\n"
	"%%% #---------------------------------------------------------------------\n"
	"%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS\n"
	"%%% #---------------------------------------------------------------------\n").

-define(TEMPLATE_2_C,
	"%%% ----------------------------------------------------------------------\n"
	"%%% #3.3   CODE FOR INTERNAL FUNCTIONS\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #   init/1\n"
	"%%% Input      : Env = term()\n"
	"%%% Output     : {ok, State}          |\n"
	"%%%              {ok, State, Timeout} |\n"
	"%%%              ignore               |\n"
	"%%%              {stop, Reason}\n"
	"%%% Exceptions : -\n"
	"%%% Description: Initiates the server\n"
	"%%% ----------------------------------------------------------------------\n"
	"init(_Env) ->\n"
	"\t{ok, #state{}}.\n\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #   terminate/2\n"
	"%%% Input      : Reason = normal | shutdown | term()\n"
	"%%%              State = term()\n"
	"%%% Output     : ok\n"
	"%%% Exceptions : -\n"
	"%%% Description: Invoked when the object is terminating.\n"
	"%%% ----------------------------------------------------------------------\n"
	"terminate(_Reason, _State) ->\n"
	"\tok.\n\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #   code_change/3\n"
	"%%% Input      : OldVsn = undefined | term()\n"
	"%%%              State = NewState = term()\n"
	"%%%              Extra = term()\n"
	"%%% Output     : {ok, NewState}\n"
	"%%% Exceptions : -\n"
	"%%% Description: Invoked when the object should update its internal state\n"
	"%%%              due to code replacement.\n"
	"%%% ----------------------------------------------------------------------\n"
	"code_change(_OldVsn, State, _Extra) ->\n"
	"\t{ok, State}.\n\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #   handle_info/2\n"
	"%%% Input      : Info = normal | shutdown | term()\n"
	"%%%              State = NewState = term()\n"
	"%%% Output     : {noreply, NewState}          |\n"
	"%%%              {noreply, NewState, Timeout} |\n"
	"%%%              {stop, Reason, NewState}\n"
	"%%% Exceptions : -\n"
	"%%% Description: Invoked when, for example, the server traps exits.\n"
	"%%% ----------------------------------------------------------------------\n"
	"handle_info(_Info, State) ->\n"
	"\t{noreply, State}.\n\n\n"
	"%%% ----------------------------------------------------------------------\n"
	"%%% #4     CODE FOR TEMPORARY CORRECTIONS\n"
	"%%% ----------------------------------------------------------------------\n\n").


%%------------------------------------------------------------
%%
%% Generate the client side Erlang stubs.
%%
%% Each module is generated to a separate file.
%%
%% Export declarations for all interface functions must be
%% generated. Each function then needs to generate a function head and
%% a body. IDL parameters must be converted into Erlang parameters
%% (variables, capitalised) and a type signature list must be
%% generated (for later encode/decode).
%%
%%------------------------------------------------------------
do_gen(G, _File, Form) -> 
    gen_head(G, [], Form),
    gen(G, [], Form).


gen(G, N, [X|Xs]) when is_record(X, preproc) ->
    NewG = ic:handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);
gen(G, N, [X|Xs]) when is_record(X, module) ->
    G2 = ic_file:filename_push(G, N, X, erlang_template_no_gen),
    N2 = [ic_forms:get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, ic_forms:get_body(X)),
    G3 = ic_file:filename_pop(G2, erlang_template_no_gen),
    gen(G3, N, Xs);
gen(G, N, [X|Xs]) when is_record(X, interface) ->
    G2 = ic_file:filename_push(G, N, X, erlang_template),
    N2 = [ic_forms:get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, ic_forms:get_body(X)),
    lists:foreach(fun({_Name, Body}) -> gen(G2, N2, Body) end, 
		  X#interface.inherit_body),
    Fd	= ic_genobj:stubfiled(G2),
    case get_template_version(G2) of
	?IC_FLAG_TEMPLATE_2 ->
	    emit(Fd, ?TEMPLATE_2_C, []);
	_ ->
	    emit(Fd, ?TEMPLATE_1_C, [])
    end,
    G3 = ic_file:filename_pop(G2, erlang_template),
    gen(G3, N, Xs);
gen(G, N, [X|Xs]) when is_record(X, op) ->
    {Name, InArgNames, OutArgNames, Reply} = extract_info(X),
    emit_function(G, N, X, ic_genobj:is_stubfile_open(G), 
		   ic_forms:is_oneway(X), Name, InArgNames, OutArgNames, Reply),
    gen(G, N, Xs);
gen(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, ic_genobj:is_stubfile_open(G), fun emit_function/9),
    gen(G, N, Xs);
gen(G, N, [_X|Xs]) ->
    gen(G, N, Xs);
gen(_G, _N, []) -> 
    ok.

%% Module Header
emit_header(G, Fd, Name) ->
    Date = get_date(),
    case get_template_version(G) of
	?IC_FLAG_TEMPLATE_2 ->
	    emit(Fd, ?TEMPLATE_2_A, [Name, list_to_atom(Name), Date]);
	_ ->
	    IDLFile = ic_genobj:idlfile(G),
	    emit(Fd, ?TEMPLATE_1_A, [Name, IDLFile, Date, list_to_atom(Name)])
    end.


emit_attr(G, N, X, Open, F) ->
    XX = #id_of{type=X},
    lists:foreach(fun(Id) ->
			  X2 = XX#id_of{id=Id},
			  IsOneWay = ic_forms:is_oneway(X2),
			  {Get, Set} = mk_attr_func_names(N, ic_forms:get_id(Id)),
			  F(G, N, X2, Open, IsOneWay, Get, [], [], 
			    [{ic_util:mk_var(ic_forms:get_id(Id)), 
			      ic_forms:get_tk(X)}]),
			  case X#attr.readonly of
			      {readonly, _} -> 
				  ok;
			      _ -> 
				  F(G, N, X2, Open, IsOneWay, Set, 
				    [{ic_util:mk_var(ic_forms:get_id(Id)), 
				      ic_forms:get_tk(X)}], [], ["ok"])
			  end 
		  end, ic_forms:get_idlist(X)).


%% The automaticly generated get and set operation names for an
%% attribute.
mk_attr_func_names(_Scope, Name) ->
    {"_get_" ++ Name, "_set_" ++ Name}.


extract_info(X) when is_record(X, op) ->
    Name	= ic_forms:get_id2(X),
    InArgs	= ic:filter_params([in,inout], X#op.params),
    OutArgs	= ic:filter_params([out,inout], X#op.params),
    Reply       = case ic_forms:get_tk(X) of
		      tk_void ->
			  ["ok"];
		      Type ->
			  [{"OE_Reply", Type}]
		  end,
    InArgsTypeList = 
	[{ic_util:mk_var(ic_forms:get_id(InArg#param.id)),
	  ic_forms:get_tk(InArg)} || InArg <- InArgs ],
    OutArgsTypeList = 
	[{ic_util:mk_var(ic_forms:get_id(OutArg#param.id)),
	  ic_forms:get_tk(OutArg)} || OutArg <- OutArgs ], 
    {Name, InArgsTypeList, OutArgsTypeList, Reply}.

get_template_version(G) ->
    case ic_options:get_opt(G, flags) of
	Flags when is_integer(Flags) ->
	    case ?IC_FLAG_TEST(Flags, ?IC_FLAG_TEMPLATE_2) of
		true ->
		    ?IC_FLAG_TEMPLATE_2;
		false ->
		    ?IC_FLAG_TEMPLATE_1
	    end;
	_ ->
	    ?IC_FLAG_TEMPLATE_1
    end.


get_date() ->
    {{Y,M,D}, _} = calendar:now_to_datetime(now()),
    if
	M < 10, D < 10 ->
	    lists:concat([Y, "-0", M, "-0",D]);
	M < 10 ->
	    lists:concat([Y, "-0", M, "-", D]);
	D < 10 ->
	    lists:concat([Y, "-", M, "-0", D]);
	true ->
	    lists:concat([Y, "-", M, "-", D])
    end.
   
  
%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when is_record(X, interface) ->
    Fd = ic_genobj:stubfiled(G),
    lists:foreach(fun({_Name, Body}) ->
			  ic_codegen:export(Fd, exp_top(G, N, Body, []))
		  end, X#interface.inherit_body),
    nl(Fd),
    ok;
gen_head_special(_G, _N, _X) ->
    ok.
    

%% Generate all export declarations
gen_head(G, N, X) -> 
    case ic_genobj:is_stubfile_open(G) of
	true -> 
	    Fd = ic_genobj:stubfiled(G),
	    ic_codegen:export(Fd, exp_top(G, N, X, [])),
	    gen_head_special(G, N, X),
	    case get_template_version(G) of
		?IC_FLAG_TEMPLATE_2 ->
		    emit(Fd, ?TEMPLATE_2_B, []);
		_ ->
		    emit(Fd, ?TEMPLATE_1_B, [])
	    end;
	false -> 
	    ok
    end.

exp_top(_G, _N, X, Acc)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, Acc) when is_list(L) ->
    exp_list(G, N, L, Acc);
exp_top(G, N, M, Acc)  when is_record(M, module) ->
    exp_list(G, N, ic_forms:get_body(M), Acc);
exp_top(G, N, I, Acc)  when is_record(I, interface) ->
    exp_list(G, N, ic_forms:get_body(I), Acc);
exp_top(G, N, X, Acc) ->
    exp3(G, N, X, Acc).

exp3(G, N, Op, Acc)  when is_record(Op, op) ->
    FuncName = ic_forms:get_id(Op#op.id),
    Arity = length(ic:filter_params([in, inout], Op#op.params)) + 1 +
	count_extras(G, N, Op),
    [{FuncName, Arity} | Acc];
exp3(G, N, A, Acc)  when is_record(A, attr) ->
    Extra = count_extras(G, N, A),
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], ic_forms:get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> 
				[{Get, 1 + Extra} | Acc2];
			    _ -> 
				[{Get, 1 + Extra}, {Set, 2 + Extra} | Acc2]
			end 
		end, Acc, ic_forms:get_idlist(A));
exp3(_G, _N, _X, Acc) -> 
    Acc.

exp_list(G, N, L, OrigAcc) -> 
    lists:foldr(fun(X, Acc) -> 
			exp3(G, N, X, Acc) 
		end, OrigAcc, L).

count_extras(G, N, Op) ->
    case {use_this(G, N, Op), use_from(G, N, Op)} of
	{[], []} ->
	    0;
	{[], _} ->
	    1;
	{_, []} ->
	    1;
	_ ->
	    2
    end.

%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%

emit_function(_G, _N, _X, false, _, _, _, _, _) ->
    ok;
emit_function(G, N, X, true, false, Name, InArgs, OutArgs, Reply) ->
    Fd = ic_genobj:stubfiled(G),
    This = use_this(G, N, Name),
    From = use_from(G, N, Name),
    State = ["State"], 
    Vers = get_template_version(G),
    case OutArgs of
	[] ->
	    ReplyString = create_string(Reply),
	    emit_function_header(G, Fd, X, N, Name, create_extra(This, From, Vers), 
				 InArgs, length(InArgs), OutArgs, Reply, 
				 ReplyString, Vers),
	    emit(Fd, "~p(~s) ->\n\t{reply, ~s, State}.\n\n",
		 [ic_util:to_atom(Name), create_string(This ++ From ++ State ++ InArgs), 
		  ReplyString]);
	_ ->
	    ReplyString = "{" ++ create_string(Reply ++ OutArgs) ++ "}",
	    emit_function_header(G, Fd, X, N, Name, create_extra(This, From, Vers), 
				 InArgs, length(InArgs), OutArgs, Reply, 
				 ReplyString, Vers),
	    emit(Fd, "~p(~s) ->\n\t{reply, ~s, State}.\n\n",
		 [ic_util:to_atom(Name), create_string(This ++ From ++ State ++ InArgs), 
		  ReplyString])
    end;
emit_function(G, N, X, true, true, Name, InArgs, _OutArgs, _Reply) ->
    Fd = ic_genobj:stubfiled(G),
    This = use_this(G, N, Name),
    State = ["State"], 
    Vers = get_template_version(G),
    emit_function_header(G, Fd, X, N, Name, create_extra(This, [], Vers), 
			 InArgs, length(InArgs), "", "", "", Vers),
    emit(Fd, "~p(~s) ->\n\t{noreply, State}.\n\n",
	 [ic_util:to_atom(Name), create_string(This ++ State ++ InArgs)]).

create_string([]) ->
    "";
create_string([{Name, _Type}|T]) ->
    Name ++ create_string2(T);
create_string([Name|T]) ->
    Name ++ create_string2(T).

create_string2([{Name, _Type}|T]) ->
    ", " ++ Name ++ create_string2(T);
create_string2([Name|T]) ->
    ", " ++ Name ++ create_string2(T);
create_string2([]) ->
    "".

create_extra([], [], _Vers) ->
    {"State - term()", 1};
create_extra([], _From, ?IC_FLAG_TEMPLATE_2) ->
    {"OE_From - term()\n%%% " ++ ?TAB ++ "State - term()", 2};
create_extra([], _From, _Vers) ->
    {"OE_From - term()\n%% " ++ ?TAB ++ "State - term()", 2};
create_extra(_This, [], ?IC_FLAG_TEMPLATE_2) ->
    {"OE_This - #objref{} (i.e., self())\n%%% " ++ ?TAB ++ "State - term()", 2};
create_extra(_This, [], _Vers) ->
    {"OE_This - #objref{} (i.e., self())\n%% " ++ ?TAB ++ "State - term()", 2};
create_extra(_This, _From, ?IC_FLAG_TEMPLATE_2) ->
    {"OE_This - #objref{} (i.e., self())\n%%% " ++ ?TAB ++ 
	"OE_From - term()\n%%% " ++ ?TAB ++ "State - term()", 3};
create_extra(_This, _From, _Vers) ->
    {"OE_This - #objref{} (i.e., self())\n%% " ++ ?TAB ++ 
	"OE_From - term()\n%% " ++ ?TAB ++ "State - term()", 3}.

use_this(G, N, OpName) ->
    FullOp = ic_util:to_colon([OpName|N]),
    FullIntf = ic_util:to_colon(N),
    case {ic_options:get_opt(G, {this, FullIntf}), 
	  ic_options:get_opt(G, {this, FullOp}), 
	  ic_options:get_opt(G, {this, true})} of
	{_, force_false, _} -> 
	    [];
	{force_false, false, _} -> 
	    [];
	{false, false, false} -> 
	    [];
	_ -> 
	    ["OE_This"]
    end.

use_from(G, N, OpName) ->
    FullOp = ic_util:to_colon([OpName|N]),
    FullIntf = ic_util:to_colon(N),
    case {ic_options:get_opt(G, {from, FullIntf}), 
	  ic_options:get_opt(G, {from, FullOp}), 
	  ic_options:get_opt(G, {from, true})} of
	{_, force_false, _} -> 
	    [];
	{force_false, false, _} ->
	    [];
	{false, false, false} -> 
	    [];
	_ ->
	    ["OE_From"]
    end.


emit_function_header(G, Fd, X, N, Name, {Extra, ExtraNo}, InP, Arity, OutP, 
		     Reply, ReplyString, ?IC_FLAG_TEMPLATE_2) ->
    emit(Fd, 
	 "%%% ----------------------------------------------------------------------\n"
	 "%%% #   ~p/~p\n"
	 "%%% Input      : ~s\n",
	 [ic_util:to_atom(Name), (ExtraNo+Arity), Extra]),
    ic_code:type_expand_all(G, N, X, Fd, ?TAB2, InP),
    case Reply of
	["ok"] ->
	    emit(Fd, "%%% Output     : ReturnValue = ~s\n", [ReplyString]);
	_ ->
	    emit(Fd, "%%% Output     : ReturnValue = ~s\n", [ReplyString]),
	    ic_code:type_expand_all(G, N, X, Fd, "%             ", Reply)
    end,
    ic_code:type_expand_all(G, N, X, Fd, ?TAB2, OutP),
    emit(Fd, 
	 "%%% Exceptions : ~s\n"
	 "%%% Description: \n"
	 "%%% ----------------------------------------------------------------------\n",
	 [get_raises(X, ?IC_FLAG_TEMPLATE_2)]);
emit_function_header(G, Fd, X, N, Name, {Extra, ExtraNo}, InP, Arity, OutP, 
		     Reply, ReplyString, Vers) ->
    emit(Fd, 
	 "%%----------------------------------------------------------------------\n"
	 "%% Function   : ~p/~p\n"
	 "%% Arguments  : ~s\n",
	 [ic_util:to_atom(Name), (ExtraNo+Arity), Extra]),
    ic_code:type_expand_all(G, N, X, Fd, ?TAB, InP),
    case Reply of
	["ok"] ->
	    emit(Fd, "%% Returns    : ReturnValue = ~s\n", [ReplyString]);
	_ ->
	    emit(Fd, "%% Returns    : ReturnValue = ~s\n", [ReplyString]),
	    ic_code:type_expand_all(G, N, X, Fd, "             ", Reply)
    end,
    ic_code:type_expand_all(G, N, X, Fd, ?TAB, OutP),
    emit(Fd, 
	 "%% Raises     : ~s\n"
	 "%% Description: \n"
	 "%%----------------------------------------------------------------------\n",
	 [get_raises(X, Vers)]).

get_raises(#op{raises = []}, _Vers) ->
    "";
get_raises(#op{raises = ExcList}, Vers) ->
    get_raises2(ExcList, [], Vers);
get_raises(_X, _Vers) -> 
    [].

get_raises2([H], Acc, _Vers) ->
    lists:flatten(lists:reverse([ic_util:to_colon(H)|Acc]));
get_raises2([H|T], Acc, ?IC_FLAG_TEMPLATE_2) ->
    get_raises2(T, ["\n%%%              ", ic_util:to_colon(H) |Acc], 
		?IC_FLAG_TEMPLATE_2);
get_raises2([H|T], Acc, _Vers) ->
    get_raises2(T, ["\n%%              ", ic_util:to_colon(H) |Acc], _Vers).

