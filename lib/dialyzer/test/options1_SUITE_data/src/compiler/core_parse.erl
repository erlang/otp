-module(core_parse).
-define(THIS_MODULE, core_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).

-export([abstract/1,abstract/2,normalise/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("core_parse.hrl").

tok_val(T) -> element(3, T).
tok_line(T) -> element(2, T).

abstract(T, _N) -> abstract(T).

abstract(Term) -> core_lib:make_literal(Term).

normalise(Core) -> core_lib:literal_value(Core).

%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: core_parse.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?THIS_MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, Endline} ->
            {error, {Endline, ?THIS_MODULE, "end_of_file"}};
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, _Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?THIS_MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'module', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, 'module', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 313, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, 'attributes', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 276, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(13, __Cat, [8 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(9, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(17, __Cat, [12 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(module_defs, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_function_name, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(15, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_module{name = #c_atom{val = tok_val(__2)}, exports = __3, attrs = __4, defs = __5},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(module_definition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__2],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(function_definitions, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(18, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_fname{id = tok_val(__1), arity = tok_val(__3)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_name, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(20, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_def{name = __1, val = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_definition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_fun, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(25, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 265, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 263, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(anno_variables, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(29, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_var{name = tok_val(__1)},
 yeccpars2(yeccgoto(variable, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_variable, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(32, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 247, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 240, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 149, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_fun{vars = __3, body = __6},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(40, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_atom{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(44, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(46, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(47, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(48, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_char{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(52, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_float{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(57, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_int{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(58, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(60, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(210, __Cat, [60 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_nil{},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(63, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(65, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(68, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_string{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(70, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(71, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(72, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(73, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(74, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(anno_expressions, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(76, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = []},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = __2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(79, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_expressions, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(81, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(let_vars, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(85, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(91, __Cat, __Ss,  [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  if length(__8) == 2 -> #c_try{arg = __2, vars = __4, body = __6, evars = __8, handler = __10}; true -> return_error(tok_line(__7),"expected 2 exception variables in 'try'") end,
 __Nss = lists:nthtail(9, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(92, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(let_vars, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(93, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(let_vars, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(95, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(96, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(97, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 182, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(98, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 149, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(anno_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(101, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(clause_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(103, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_atom{val = tok_val(__1)},
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(106, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(107, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_clause, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(109, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(110, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(112, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   {T,A} = __2, #c_receive{clauses = [], timeout = T, action = A}
  end,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(113, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(114, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(115, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(116, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(anno_patterns, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(117, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = []},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = __2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(120, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(121, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_patterns, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(122, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 159, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(124, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_variable, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(125, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(127, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(128, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_variable, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(129, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 149, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(annotation, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(131, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  tok_val(__1),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(132, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(133, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  tok_val(__1),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(134, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(135, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(constants, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(136, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(137, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  tok_val(__1),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(138, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  tok_val(__1),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(139, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(140, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  tok_val(__1),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(141, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(142, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(143, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(144, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_constant, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(145, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  list_to_tuple(__2),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_constant, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(146, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(annotation, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(147, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(148, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(constants, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(149, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {nil,tok_line(__1)},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(nil, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(150, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(151, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(152, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(yeccgoto(tail_constant, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(153, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__2|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_constant, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(154, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(155, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(156, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_constant, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(157, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(158, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__2|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_constant, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(159, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(160, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 161, [160 | __Ss], [__T | __Stack]);
yeccpars2(160, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(161, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(162, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(163, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_alias{var = __1, pat = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(other_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(164, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(165, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 166, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(166, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(167, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_clause{pats = __1, guard = __3, body = __5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(168, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   {T,A} = __3, #c_receive{clauses = __2, timeout = T, action = A}
  end,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(169, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__2],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(anno_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(170, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [170 | __Ss], [__T | __Stack]);
yeccpars2(170, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(171, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__2,__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(timeout, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(173, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 174, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 177, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(174, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(175, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_nil{},
 yeccpars2(yeccgoto(tail_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(176, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(177, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(178, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(179, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(180, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 174, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 177, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(181, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(182, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(183, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 184, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(184, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(clause_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(185, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [185 | __Ss], [__T | __Stack]);
yeccpars2(185, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(186, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 159, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(187, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [187 | __Ss], [__T | __Stack]);
yeccpars2(187, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(188, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 189, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(189, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(190, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 194, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(191, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 200, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 198, [192 | __Ss], [__T | __Stack]);
yeccpars2(192, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(segment_patterns, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(193, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 196, [193 | __Ss], [__T | __Stack]);
yeccpars2(193, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(194, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 195, [194 | __Ss], [__T | __Stack]);
yeccpars2(194, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(195, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_binary{segments = []},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(binary_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(196, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 197, [196 | __Ss], [__T | __Stack]);
yeccpars2(196, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(197, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_binary{segments = __3},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(binary_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(198, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(199, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(segment_patterns, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(200, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(201, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 202, [201 | __Ss], [__T | __Stack]);
yeccpars2(201, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(202, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [202 | __Ss], [__T | __Stack]);
yeccpars2(202, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(203, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 205, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  case __5 of [S,U,T,Fs] -> #c_bitstr{val = __3, size = S, unit = U, type = T, flags = Fs}; true -> return_error(tok_line(__1),"expected 4 arguments in binary segment") end,
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(segment_pattern, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(205, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(arg_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(206, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 207, [206 | __Ss], [__T | __Stack]);
yeccpars2(206, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(207, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(arg_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(208, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [208 | __Ss], [__T | __Stack]);
yeccpars2(208, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(209, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   Name = #c_atom{val = tok_val(__2)}, #c_primop{name = Name, args = __3}
  end,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primop_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(210, 'in', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [210 | __Ss], [__T | __Stack]);
yeccpars2(210, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(211, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(212, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_letrec{defs = __2, body = __4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(letrec_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(213, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(214, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(215, 'in', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 216, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(216, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(217, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_let{vars = __2, arg = __4, body = __6},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(let_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(218, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_seq{arg = __2, body = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(sequence, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(220, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_catch{body = __2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(catch_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(221, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [221 | __Ss], [__T | __Stack]);
yeccpars2(221, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(222, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(223, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 224, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(224, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_case{arg = __2, clauses = __4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(case_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(225, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 226, [225 | __Ss], [__T | __Stack]);
yeccpars2(225, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(226, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(227, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(228, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_call{module = __2, name = __4, args = __5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(call_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(229, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [229 | __Ss], [__T | __Stack]);
yeccpars2(229, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(230, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_apply{op = __2, args = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(application_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(231, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 232, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 235, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 233, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(232, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(233, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_nil{},
 yeccpars2(yeccgoto(tail, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(234, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(235, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(236, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 237, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(237, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(238, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 232, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 235, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 233, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(239, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(240, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_values{es = []},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expression, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(241, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 242, [241 | __Ss], [__T | __Stack]);
yeccpars2(241, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(242, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_values{es = __2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expression, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(243, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 244, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(244, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [244 | __Ss], [__T | __Stack]);
yeccpars2(244, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(245, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [245 | __Ss], [__T | __Stack]);
yeccpars2(245, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(246, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_expression, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(247, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 248, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 251, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(248, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 257, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(249, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 255, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(segments, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(250, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 253, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(251, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 252, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(252, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_binary{segments = []},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(253, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 254, [253 | __Ss], [__T | __Stack]);
yeccpars2(253, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(254, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_binary{segments = __3},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(255, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 248, [255 | __Ss], [__T | __Stack]);
yeccpars2(255, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(256, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(segments, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(257, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(258, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 259, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(259, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 260, [259 | __Ss], [__T | __Stack]);
yeccpars2(259, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(260, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(261, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, __Cat, __Ss,  [__7,__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  case __6 of [S,U,T,Fs] -> #c_bitstr{val = __3, size = S, unit = U, type = T, flags = Fs}; true -> return_error(tok_line(__1),"expected 4 arguments in binary segment") end,
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(segment, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(263, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(264, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_variables, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(265, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'primop', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'call', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'apply', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'letrec', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'do', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(266, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_fun{vars = [], body = __5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(267, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [267 | __Ss], [__T | __Stack]);
yeccpars2(267, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(268, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 269, [268 | __Ss], [__T | __Stack]);
yeccpars2(268, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(269, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [269 | __Ss], [__T | __Stack]);
yeccpars2(269, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(270, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 271, [270 | __Ss], [__T | __Stack]);
yeccpars2(270, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(271, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_fun, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(272, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 273, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(273, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(274, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [274 | __Ss], [__T | __Stack]);
yeccpars2(274, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(275, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  core_lib:set_anno(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_function_name, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(276, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 278, [276 | __Ss], [__T | __Stack]);
yeccpars2(276, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 277, [276 | __Ss], [__T | __Stack]);
yeccpars2(276, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(277, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(module_attribute, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(278, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 284, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(279, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 282, [279 | __Ss], [__T | __Stack]);
yeccpars2(279, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(attribute_list, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(280, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 281, [280 | __Ss], [__T | __Stack]);
yeccpars2(280, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(281, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __3,
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(module_attribute, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(282, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 278, [282 | __Ss], [__T | __Stack]);
yeccpars2(282, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(283, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(attribute_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(284, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(285, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 149, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(286, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(287, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(288, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_def{name = #c_atom{val = tok_val(__1)}, val = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(attribute, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(289, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(290, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 293, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(291, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(literals, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(292, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 294, [292 | __Ss], [__T | __Stack]);
yeccpars2(292, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(293, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = []},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_literal, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(294, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_tuple{es = __2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_literal, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(295, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(296, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(literals, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(297, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 298, [297 | __Ss], [__T | __Stack]);
yeccpars2(297, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [297 | __Ss], [__T | __Stack]);
yeccpars2(297, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [297 | __Ss], [__T | __Stack]);
yeccpars2(297, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(298, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(299, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_nil{},
 yeccpars2(yeccgoto(tail_literal, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(300, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_literal, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(301, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(302, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [302 | __Ss], [__T | __Stack]);
yeccpars2(302, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(303, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_literal, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(304, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 298, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(305, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_cons{hd = __2, tl = __3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_literal, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(306, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(module_export, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(307, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 311, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(exported_names, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(308, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(309, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(exported_name, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(310, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(module_export, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(311, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [311 | __Ss], [__T | __Stack]);
yeccpars2(311, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(312, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(exported_names, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(313, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 314, [313 | __Ss], [__T | __Stack]);
yeccpars2(313, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(314, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [314 | __Ss], [__T | __Stack]);
yeccpars2(314, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(315, 'attributes', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [315 | __Ss], [__T | __Stack]);
yeccpars2(315, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(316, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(13, __Cat, [316 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(317, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 318, [317 | __Ss], [__T | __Stack]);
yeccpars2(317, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(318, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 319, [318 | __Ss], [__T | __Stack]);
yeccpars2(318, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(319, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(320, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 321, [320 | __Ss], [__T | __Stack]);
yeccpars2(320, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(321, __Cat, __Ss,  [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  #c_module{anno = __9, name = tok_val(__3), exports = __4, attrs = __5, defs = __6},
 __Nss = lists:nthtail(9, __Ss),
 yeccpars2(yeccgoto(module_definition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(anno_clause, 65) ->
 100;
yeccgoto(anno_clause, 100) ->
 100;
yeccgoto(anno_clause, 222) ->
 100;
yeccgoto(anno_clauses, 65) ->
 101;
yeccgoto(anno_clauses, 100) ->
 169;
yeccgoto(anno_clauses, 222) ->
 223;
yeccgoto(anno_expression, 33) ->
 38;
yeccgoto(anno_expression, 36) ->
 75;
yeccgoto(anno_expression, 37) ->
 231;
yeccgoto(anno_expression, 40) ->
 229;
yeccgoto(anno_expression, 44) ->
 225;
yeccgoto(anno_expression, 46) ->
 221;
yeccgoto(anno_expression, 48) ->
 220;
yeccgoto(anno_expression, 52) ->
 218;
yeccgoto(anno_expression, 70) ->
 81;
yeccgoto(anno_expression, 74) ->
 75;
yeccgoto(anno_expression, 79) ->
 75;
yeccgoto(anno_expression, 86) ->
 87;
yeccgoto(anno_expression, 90) ->
 91;
yeccgoto(anno_expression, 99) ->
 170;
yeccgoto(anno_expression, 164) ->
 165;
yeccgoto(anno_expression, 166) ->
 167;
yeccgoto(anno_expression, 171) ->
 172;
yeccgoto(anno_expression, 203) ->
 75;
yeccgoto(anno_expression, 211) ->
 212;
yeccgoto(anno_expression, 214) ->
 215;
yeccgoto(anno_expression, 216) ->
 217;
yeccgoto(anno_expression, 218) ->
 219;
yeccgoto(anno_expression, 226) ->
 227;
yeccgoto(anno_expression, 232) ->
 238;
yeccgoto(anno_expression, 235) ->
 236;
yeccgoto(anno_expression, 257) ->
 258;
yeccgoto(anno_expression, 260) ->
 75;
yeccgoto(anno_expression, 265) ->
 266;
yeccgoto(anno_expressions, 36) ->
 241;
yeccgoto(anno_expressions, 74) ->
 76;
yeccgoto(anno_expressions, 79) ->
 80;
yeccgoto(anno_expressions, 203) ->
 206;
yeccgoto(anno_expressions, 260) ->
 261;
yeccgoto(anno_fun, 20) ->
 22;
yeccgoto(anno_function_name, 8) ->
 10;
yeccgoto(anno_function_name, 12) ->
 10;
yeccgoto(anno_function_name, 60) ->
 10;
yeccgoto(anno_function_name, 316) ->
 10;
yeccgoto(anno_pattern, 65) ->
 102;
yeccgoto(anno_pattern, 96) ->
 102;
yeccgoto(anno_pattern, 97) ->
 116;
yeccgoto(anno_pattern, 98) ->
 173;
yeccgoto(anno_pattern, 100) ->
 102;
yeccgoto(anno_pattern, 114) ->
 116;
yeccgoto(anno_pattern, 120) ->
 116;
yeccgoto(anno_pattern, 162) ->
 163;
yeccgoto(anno_pattern, 174) ->
 180;
yeccgoto(anno_pattern, 177) ->
 178;
yeccgoto(anno_pattern, 200) ->
 201;
yeccgoto(anno_pattern, 222) ->
 102;
yeccgoto(anno_patterns, 97) ->
 183;
yeccgoto(anno_patterns, 114) ->
 117;
yeccgoto(anno_patterns, 120) ->
 121;
yeccgoto(anno_variable, 25) ->
 28;
yeccgoto(anno_variable, 58) ->
 84;
yeccgoto(anno_variable, 65) ->
 103;
yeccgoto(anno_variable, 82) ->
 84;
yeccgoto(anno_variable, 83) ->
 28;
yeccgoto(anno_variable, 88) ->
 84;
yeccgoto(anno_variable, 96) ->
 103;
yeccgoto(anno_variable, 97) ->
 103;
yeccgoto(anno_variable, 98) ->
 103;
yeccgoto(anno_variable, 100) ->
 103;
yeccgoto(anno_variable, 114) ->
 103;
yeccgoto(anno_variable, 115) ->
 122;
yeccgoto(anno_variable, 120) ->
 103;
yeccgoto(anno_variable, 162) ->
 103;
yeccgoto(anno_variable, 174) ->
 103;
yeccgoto(anno_variable, 177) ->
 103;
yeccgoto(anno_variable, 200) ->
 103;
yeccgoto(anno_variable, 222) ->
 103;
yeccgoto(anno_variable, 263) ->
 28;
yeccgoto(anno_variables, 25) ->
 29;
yeccgoto(anno_variables, 83) ->
 93;
yeccgoto(anno_variables, 263) ->
 264;
yeccgoto(annotation, 125) ->
 127;
yeccgoto(annotation, 159) ->
 160;
yeccgoto(annotation, 187) ->
 188;
yeccgoto(annotation, 244) ->
 245;
yeccgoto(annotation, 269) ->
 270;
yeccgoto(annotation, 273) ->
 274;
yeccgoto(annotation, 319) ->
 320;
yeccgoto(application_expr, 33) ->
 39;
yeccgoto(application_expr, 35) ->
 39;
yeccgoto(application_expr, 36) ->
 39;
yeccgoto(application_expr, 37) ->
 39;
yeccgoto(application_expr, 40) ->
 39;
yeccgoto(application_expr, 44) ->
 39;
yeccgoto(application_expr, 46) ->
 39;
yeccgoto(application_expr, 48) ->
 39;
yeccgoto(application_expr, 52) ->
 39;
yeccgoto(application_expr, 70) ->
 39;
yeccgoto(application_expr, 74) ->
 39;
yeccgoto(application_expr, 79) ->
 39;
yeccgoto(application_expr, 86) ->
 39;
yeccgoto(application_expr, 90) ->
 39;
yeccgoto(application_expr, 99) ->
 39;
yeccgoto(application_expr, 164) ->
 39;
yeccgoto(application_expr, 166) ->
 39;
yeccgoto(application_expr, 171) ->
 39;
yeccgoto(application_expr, 203) ->
 39;
yeccgoto(application_expr, 211) ->
 39;
yeccgoto(application_expr, 214) ->
 39;
yeccgoto(application_expr, 216) ->
 39;
yeccgoto(application_expr, 218) ->
 39;
yeccgoto(application_expr, 226) ->
 39;
yeccgoto(application_expr, 232) ->
 39;
yeccgoto(application_expr, 235) ->
 39;
yeccgoto(application_expr, 257) ->
 39;
yeccgoto(application_expr, 260) ->
 39;
yeccgoto(application_expr, 265) ->
 39;
yeccgoto(arg_list, 202) ->
 204;
yeccgoto(arg_list, 208) ->
 209;
yeccgoto(arg_list, 227) ->
 228;
yeccgoto(arg_list, 229) ->
 230;
yeccgoto(atomic_constant, 126) ->
 132;
yeccgoto(atomic_constant, 129) ->
 132;
yeccgoto(atomic_constant, 142) ->
 132;
yeccgoto(atomic_constant, 147) ->
 132;
yeccgoto(atomic_constant, 151) ->
 132;
yeccgoto(atomic_constant, 154) ->
 132;
yeccgoto(atomic_literal, 33) ->
 42;
yeccgoto(atomic_literal, 35) ->
 42;
yeccgoto(atomic_literal, 36) ->
 42;
yeccgoto(atomic_literal, 37) ->
 42;
yeccgoto(atomic_literal, 40) ->
 42;
yeccgoto(atomic_literal, 44) ->
 42;
yeccgoto(atomic_literal, 46) ->
 42;
yeccgoto(atomic_literal, 48) ->
 42;
yeccgoto(atomic_literal, 52) ->
 42;
yeccgoto(atomic_literal, 65) ->
 105;
yeccgoto(atomic_literal, 70) ->
 42;
yeccgoto(atomic_literal, 74) ->
 42;
yeccgoto(atomic_literal, 79) ->
 42;
yeccgoto(atomic_literal, 86) ->
 42;
yeccgoto(atomic_literal, 90) ->
 42;
yeccgoto(atomic_literal, 96) ->
 105;
yeccgoto(atomic_literal, 97) ->
 105;
yeccgoto(atomic_literal, 98) ->
 105;
yeccgoto(atomic_literal, 99) ->
 42;
yeccgoto(atomic_literal, 100) ->
 105;
yeccgoto(atomic_literal, 114) ->
 105;
yeccgoto(atomic_literal, 115) ->
 105;
yeccgoto(atomic_literal, 120) ->
 105;
yeccgoto(atomic_literal, 162) ->
 105;
yeccgoto(atomic_literal, 164) ->
 42;
yeccgoto(atomic_literal, 166) ->
 42;
yeccgoto(atomic_literal, 171) ->
 42;
yeccgoto(atomic_literal, 174) ->
 105;
yeccgoto(atomic_literal, 177) ->
 105;
yeccgoto(atomic_literal, 200) ->
 105;
yeccgoto(atomic_literal, 203) ->
 42;
yeccgoto(atomic_literal, 211) ->
 42;
yeccgoto(atomic_literal, 214) ->
 42;
yeccgoto(atomic_literal, 216) ->
 42;
yeccgoto(atomic_literal, 218) ->
 42;
yeccgoto(atomic_literal, 222) ->
 105;
yeccgoto(atomic_literal, 226) ->
 42;
yeccgoto(atomic_literal, 232) ->
 42;
yeccgoto(atomic_literal, 235) ->
 42;
yeccgoto(atomic_literal, 257) ->
 42;
yeccgoto(atomic_literal, 260) ->
 42;
yeccgoto(atomic_literal, 265) ->
 42;
yeccgoto(atomic_literal, 284) ->
 286;
yeccgoto(atomic_literal, 285) ->
 286;
yeccgoto(atomic_literal, 290) ->
 286;
yeccgoto(atomic_literal, 295) ->
 286;
yeccgoto(atomic_literal, 298) ->
 286;
yeccgoto(atomic_literal, 301) ->
 286;
yeccgoto(atomic_pattern, 65) ->
 106;
yeccgoto(atomic_pattern, 96) ->
 106;
yeccgoto(atomic_pattern, 97) ->
 106;
yeccgoto(atomic_pattern, 98) ->
 106;
yeccgoto(atomic_pattern, 100) ->
 106;
yeccgoto(atomic_pattern, 114) ->
 106;
yeccgoto(atomic_pattern, 115) ->
 106;
yeccgoto(atomic_pattern, 120) ->
 106;
yeccgoto(atomic_pattern, 162) ->
 106;
yeccgoto(atomic_pattern, 174) ->
 106;
yeccgoto(atomic_pattern, 177) ->
 106;
yeccgoto(atomic_pattern, 200) ->
 106;
yeccgoto(atomic_pattern, 222) ->
 106;
yeccgoto(attribute, 276) ->
 279;
yeccgoto(attribute, 282) ->
 279;
yeccgoto(attribute_list, 276) ->
 280;
yeccgoto(attribute_list, 282) ->
 283;
yeccgoto(binary, 33) ->
 43;
yeccgoto(binary, 35) ->
 43;
yeccgoto(binary, 36) ->
 43;
yeccgoto(binary, 37) ->
 43;
yeccgoto(binary, 40) ->
 43;
yeccgoto(binary, 44) ->
 43;
yeccgoto(binary, 46) ->
 43;
yeccgoto(binary, 48) ->
 43;
yeccgoto(binary, 52) ->
 43;
yeccgoto(binary, 70) ->
 43;
yeccgoto(binary, 74) ->
 43;
yeccgoto(binary, 79) ->
 43;
yeccgoto(binary, 86) ->
 43;
yeccgoto(binary, 90) ->
 43;
yeccgoto(binary, 99) ->
 43;
yeccgoto(binary, 164) ->
 43;
yeccgoto(binary, 166) ->
 43;
yeccgoto(binary, 171) ->
 43;
yeccgoto(binary, 203) ->
 43;
yeccgoto(binary, 211) ->
 43;
yeccgoto(binary, 214) ->
 43;
yeccgoto(binary, 216) ->
 43;
yeccgoto(binary, 218) ->
 43;
yeccgoto(binary, 226) ->
 43;
yeccgoto(binary, 232) ->
 43;
yeccgoto(binary, 235) ->
 43;
yeccgoto(binary, 257) ->
 43;
yeccgoto(binary, 260) ->
 43;
yeccgoto(binary, 265) ->
 43;
yeccgoto(binary_pattern, 65) ->
 107;
yeccgoto(binary_pattern, 96) ->
 107;
yeccgoto(binary_pattern, 97) ->
 107;
yeccgoto(binary_pattern, 98) ->
 107;
yeccgoto(binary_pattern, 100) ->
 107;
yeccgoto(binary_pattern, 114) ->
 107;
yeccgoto(binary_pattern, 115) ->
 107;
yeccgoto(binary_pattern, 120) ->
 107;
yeccgoto(binary_pattern, 162) ->
 107;
yeccgoto(binary_pattern, 174) ->
 107;
yeccgoto(binary_pattern, 177) ->
 107;
yeccgoto(binary_pattern, 200) ->
 107;
yeccgoto(binary_pattern, 222) ->
 107;
yeccgoto(call_expr, 33) ->
 45;
yeccgoto(call_expr, 35) ->
 45;
yeccgoto(call_expr, 36) ->
 45;
yeccgoto(call_expr, 37) ->
 45;
yeccgoto(call_expr, 40) ->
 45;
yeccgoto(call_expr, 44) ->
 45;
yeccgoto(call_expr, 46) ->
 45;
yeccgoto(call_expr, 48) ->
 45;
yeccgoto(call_expr, 52) ->
 45;
yeccgoto(call_expr, 70) ->
 45;
yeccgoto(call_expr, 74) ->
 45;
yeccgoto(call_expr, 79) ->
 45;
yeccgoto(call_expr, 86) ->
 45;
yeccgoto(call_expr, 90) ->
 45;
yeccgoto(call_expr, 99) ->
 45;
yeccgoto(call_expr, 164) ->
 45;
yeccgoto(call_expr, 166) ->
 45;
yeccgoto(call_expr, 171) ->
 45;
yeccgoto(call_expr, 203) ->
 45;
yeccgoto(call_expr, 211) ->
 45;
yeccgoto(call_expr, 214) ->
 45;
yeccgoto(call_expr, 216) ->
 45;
yeccgoto(call_expr, 218) ->
 45;
yeccgoto(call_expr, 226) ->
 45;
yeccgoto(call_expr, 232) ->
 45;
yeccgoto(call_expr, 235) ->
 45;
yeccgoto(call_expr, 257) ->
 45;
yeccgoto(call_expr, 260) ->
 45;
yeccgoto(call_expr, 265) ->
 45;
yeccgoto(case_expr, 33) ->
 47;
yeccgoto(case_expr, 35) ->
 47;
yeccgoto(case_expr, 36) ->
 47;
yeccgoto(case_expr, 37) ->
 47;
yeccgoto(case_expr, 40) ->
 47;
yeccgoto(case_expr, 44) ->
 47;
yeccgoto(case_expr, 46) ->
 47;
yeccgoto(case_expr, 48) ->
 47;
yeccgoto(case_expr, 52) ->
 47;
yeccgoto(case_expr, 70) ->
 47;
yeccgoto(case_expr, 74) ->
 47;
yeccgoto(case_expr, 79) ->
 47;
yeccgoto(case_expr, 86) ->
 47;
yeccgoto(case_expr, 90) ->
 47;
yeccgoto(case_expr, 99) ->
 47;
yeccgoto(case_expr, 164) ->
 47;
yeccgoto(case_expr, 166) ->
 47;
yeccgoto(case_expr, 171) ->
 47;
yeccgoto(case_expr, 203) ->
 47;
yeccgoto(case_expr, 211) ->
 47;
yeccgoto(case_expr, 214) ->
 47;
yeccgoto(case_expr, 216) ->
 47;
yeccgoto(case_expr, 218) ->
 47;
yeccgoto(case_expr, 226) ->
 47;
yeccgoto(case_expr, 232) ->
 47;
yeccgoto(case_expr, 235) ->
 47;
yeccgoto(case_expr, 257) ->
 47;
yeccgoto(case_expr, 260) ->
 47;
yeccgoto(case_expr, 265) ->
 47;
yeccgoto(catch_expr, 33) ->
 49;
yeccgoto(catch_expr, 35) ->
 49;
yeccgoto(catch_expr, 36) ->
 49;
yeccgoto(catch_expr, 37) ->
 49;
yeccgoto(catch_expr, 40) ->
 49;
yeccgoto(catch_expr, 44) ->
 49;
yeccgoto(catch_expr, 46) ->
 49;
yeccgoto(catch_expr, 48) ->
 49;
yeccgoto(catch_expr, 52) ->
 49;
yeccgoto(catch_expr, 70) ->
 49;
yeccgoto(catch_expr, 74) ->
 49;
yeccgoto(catch_expr, 79) ->
 49;
yeccgoto(catch_expr, 86) ->
 49;
yeccgoto(catch_expr, 90) ->
 49;
yeccgoto(catch_expr, 99) ->
 49;
yeccgoto(catch_expr, 164) ->
 49;
yeccgoto(catch_expr, 166) ->
 49;
yeccgoto(catch_expr, 171) ->
 49;
yeccgoto(catch_expr, 203) ->
 49;
yeccgoto(catch_expr, 211) ->
 49;
yeccgoto(catch_expr, 214) ->
 49;
yeccgoto(catch_expr, 216) ->
 49;
yeccgoto(catch_expr, 218) ->
 49;
yeccgoto(catch_expr, 226) ->
 49;
yeccgoto(catch_expr, 232) ->
 49;
yeccgoto(catch_expr, 235) ->
 49;
yeccgoto(catch_expr, 257) ->
 49;
yeccgoto(catch_expr, 260) ->
 49;
yeccgoto(catch_expr, 265) ->
 49;
yeccgoto(clause, 65) ->
 108;
yeccgoto(clause, 96) ->
 185;
yeccgoto(clause, 100) ->
 108;
yeccgoto(clause, 222) ->
 108;
yeccgoto(clause_pattern, 65) ->
 109;
yeccgoto(clause_pattern, 96) ->
 109;
yeccgoto(clause_pattern, 100) ->
 109;
yeccgoto(clause_pattern, 222) ->
 109;
yeccgoto(cons, 33) ->
 51;
yeccgoto(cons, 35) ->
 51;
yeccgoto(cons, 36) ->
 51;
yeccgoto(cons, 37) ->
 51;
yeccgoto(cons, 40) ->
 51;
yeccgoto(cons, 44) ->
 51;
yeccgoto(cons, 46) ->
 51;
yeccgoto(cons, 48) ->
 51;
yeccgoto(cons, 52) ->
 51;
yeccgoto(cons, 70) ->
 51;
yeccgoto(cons, 74) ->
 51;
yeccgoto(cons, 79) ->
 51;
yeccgoto(cons, 86) ->
 51;
yeccgoto(cons, 90) ->
 51;
yeccgoto(cons, 99) ->
 51;
yeccgoto(cons, 164) ->
 51;
yeccgoto(cons, 166) ->
 51;
yeccgoto(cons, 171) ->
 51;
yeccgoto(cons, 203) ->
 51;
yeccgoto(cons, 211) ->
 51;
yeccgoto(cons, 214) ->
 51;
yeccgoto(cons, 216) ->
 51;
yeccgoto(cons, 218) ->
 51;
yeccgoto(cons, 226) ->
 51;
yeccgoto(cons, 232) ->
 51;
yeccgoto(cons, 235) ->
 51;
yeccgoto(cons, 257) ->
 51;
yeccgoto(cons, 260) ->
 51;
yeccgoto(cons, 265) ->
 51;
yeccgoto(cons_constant, 126) ->
 134;
yeccgoto(cons_constant, 129) ->
 134;
yeccgoto(cons_constant, 142) ->
 134;
yeccgoto(cons_constant, 147) ->
 134;
yeccgoto(cons_constant, 151) ->
 134;
yeccgoto(cons_constant, 154) ->
 134;
yeccgoto(cons_literal, 284) ->
 287;
yeccgoto(cons_literal, 285) ->
 287;
yeccgoto(cons_literal, 290) ->
 287;
yeccgoto(cons_literal, 295) ->
 287;
yeccgoto(cons_literal, 298) ->
 287;
yeccgoto(cons_literal, 301) ->
 287;
yeccgoto(cons_pattern, 65) ->
 110;
yeccgoto(cons_pattern, 96) ->
 110;
yeccgoto(cons_pattern, 97) ->
 110;
yeccgoto(cons_pattern, 98) ->
 110;
yeccgoto(cons_pattern, 100) ->
 110;
yeccgoto(cons_pattern, 114) ->
 110;
yeccgoto(cons_pattern, 115) ->
 110;
yeccgoto(cons_pattern, 120) ->
 110;
yeccgoto(cons_pattern, 162) ->
 110;
yeccgoto(cons_pattern, 174) ->
 110;
yeccgoto(cons_pattern, 177) ->
 110;
yeccgoto(cons_pattern, 200) ->
 110;
yeccgoto(cons_pattern, 222) ->
 110;
yeccgoto(constant, 126) ->
 135;
yeccgoto(constant, 129) ->
 150;
yeccgoto(constant, 142) ->
 135;
yeccgoto(constant, 147) ->
 135;
yeccgoto(constant, 151) ->
 157;
yeccgoto(constant, 154) ->
 155;
yeccgoto(constants, 126) ->
 136;
yeccgoto(constants, 142) ->
 143;
yeccgoto(constants, 147) ->
 148;
yeccgoto(exported_name, 5) ->
 307;
yeccgoto(exported_name, 311) ->
 307;
yeccgoto(exported_names, 5) ->
 308;
yeccgoto(exported_names, 311) ->
 312;
yeccgoto(expression, 33) ->
 53;
yeccgoto(expression, 35) ->
 243;
yeccgoto(expression, 36) ->
 53;
yeccgoto(expression, 37) ->
 53;
yeccgoto(expression, 40) ->
 53;
yeccgoto(expression, 44) ->
 53;
yeccgoto(expression, 46) ->
 53;
yeccgoto(expression, 48) ->
 53;
yeccgoto(expression, 52) ->
 53;
yeccgoto(expression, 70) ->
 53;
yeccgoto(expression, 74) ->
 53;
yeccgoto(expression, 79) ->
 53;
yeccgoto(expression, 86) ->
 53;
yeccgoto(expression, 90) ->
 53;
yeccgoto(expression, 99) ->
 53;
yeccgoto(expression, 164) ->
 53;
yeccgoto(expression, 166) ->
 53;
yeccgoto(expression, 171) ->
 53;
yeccgoto(expression, 203) ->
 53;
yeccgoto(expression, 211) ->
 53;
yeccgoto(expression, 214) ->
 53;
yeccgoto(expression, 216) ->
 53;
yeccgoto(expression, 218) ->
 53;
yeccgoto(expression, 226) ->
 53;
yeccgoto(expression, 232) ->
 53;
yeccgoto(expression, 235) ->
 53;
yeccgoto(expression, 257) ->
 53;
yeccgoto(expression, 260) ->
 53;
yeccgoto(expression, 265) ->
 53;
yeccgoto(fun_expr, 20) ->
 24;
yeccgoto(fun_expr, 21) ->
 268;
yeccgoto(fun_expr, 33) ->
 55;
yeccgoto(fun_expr, 35) ->
 55;
yeccgoto(fun_expr, 36) ->
 55;
yeccgoto(fun_expr, 37) ->
 55;
yeccgoto(fun_expr, 40) ->
 55;
yeccgoto(fun_expr, 44) ->
 55;
yeccgoto(fun_expr, 46) ->
 55;
yeccgoto(fun_expr, 48) ->
 55;
yeccgoto(fun_expr, 52) ->
 55;
yeccgoto(fun_expr, 70) ->
 55;
yeccgoto(fun_expr, 74) ->
 55;
yeccgoto(fun_expr, 79) ->
 55;
yeccgoto(fun_expr, 86) ->
 55;
yeccgoto(fun_expr, 90) ->
 55;
yeccgoto(fun_expr, 99) ->
 55;
yeccgoto(fun_expr, 164) ->
 55;
yeccgoto(fun_expr, 166) ->
 55;
yeccgoto(fun_expr, 171) ->
 55;
yeccgoto(fun_expr, 203) ->
 55;
yeccgoto(fun_expr, 211) ->
 55;
yeccgoto(fun_expr, 214) ->
 55;
yeccgoto(fun_expr, 216) ->
 55;
yeccgoto(fun_expr, 218) ->
 55;
yeccgoto(fun_expr, 226) ->
 55;
yeccgoto(fun_expr, 232) ->
 55;
yeccgoto(fun_expr, 235) ->
 55;
yeccgoto(fun_expr, 257) ->
 55;
yeccgoto(fun_expr, 260) ->
 55;
yeccgoto(fun_expr, 265) ->
 55;
yeccgoto(function_definition, 8) ->
 12;
yeccgoto(function_definition, 12) ->
 12;
yeccgoto(function_definition, 60) ->
 12;
yeccgoto(function_definition, 316) ->
 12;
yeccgoto(function_definitions, 8) ->
 13;
yeccgoto(function_definitions, 12) ->
 17;
yeccgoto(function_definitions, 60) ->
 210;
yeccgoto(function_definitions, 316) ->
 13;
yeccgoto(function_name, 5) ->
 309;
yeccgoto(function_name, 8) ->
 14;
yeccgoto(function_name, 9) ->
 272;
yeccgoto(function_name, 12) ->
 14;
yeccgoto(function_name, 33) ->
 56;
yeccgoto(function_name, 35) ->
 56;
yeccgoto(function_name, 36) ->
 56;
yeccgoto(function_name, 37) ->
 56;
yeccgoto(function_name, 40) ->
 56;
yeccgoto(function_name, 44) ->
 56;
yeccgoto(function_name, 46) ->
 56;
yeccgoto(function_name, 48) ->
 56;
yeccgoto(function_name, 52) ->
 56;
yeccgoto(function_name, 60) ->
 14;
yeccgoto(function_name, 70) ->
 56;
yeccgoto(function_name, 74) ->
 56;
yeccgoto(function_name, 79) ->
 56;
yeccgoto(function_name, 86) ->
 56;
yeccgoto(function_name, 90) ->
 56;
yeccgoto(function_name, 99) ->
 56;
yeccgoto(function_name, 164) ->
 56;
yeccgoto(function_name, 166) ->
 56;
yeccgoto(function_name, 171) ->
 56;
yeccgoto(function_name, 203) ->
 56;
yeccgoto(function_name, 211) ->
 56;
yeccgoto(function_name, 214) ->
 56;
yeccgoto(function_name, 216) ->
 56;
yeccgoto(function_name, 218) ->
 56;
yeccgoto(function_name, 226) ->
 56;
yeccgoto(function_name, 232) ->
 56;
yeccgoto(function_name, 235) ->
 56;
yeccgoto(function_name, 257) ->
 56;
yeccgoto(function_name, 260) ->
 56;
yeccgoto(function_name, 265) ->
 56;
yeccgoto(function_name, 311) ->
 309;
yeccgoto(function_name, 316) ->
 14;
yeccgoto(let_expr, 33) ->
 59;
yeccgoto(let_expr, 35) ->
 59;
yeccgoto(let_expr, 36) ->
 59;
yeccgoto(let_expr, 37) ->
 59;
yeccgoto(let_expr, 40) ->
 59;
yeccgoto(let_expr, 44) ->
 59;
yeccgoto(let_expr, 46) ->
 59;
yeccgoto(let_expr, 48) ->
 59;
yeccgoto(let_expr, 52) ->
 59;
yeccgoto(let_expr, 70) ->
 59;
yeccgoto(let_expr, 74) ->
 59;
yeccgoto(let_expr, 79) ->
 59;
yeccgoto(let_expr, 86) ->
 59;
yeccgoto(let_expr, 90) ->
 59;
yeccgoto(let_expr, 99) ->
 59;
yeccgoto(let_expr, 164) ->
 59;
yeccgoto(let_expr, 166) ->
 59;
yeccgoto(let_expr, 171) ->
 59;
yeccgoto(let_expr, 203) ->
 59;
yeccgoto(let_expr, 211) ->
 59;
yeccgoto(let_expr, 214) ->
 59;
yeccgoto(let_expr, 216) ->
 59;
yeccgoto(let_expr, 218) ->
 59;
yeccgoto(let_expr, 226) ->
 59;
yeccgoto(let_expr, 232) ->
 59;
yeccgoto(let_expr, 235) ->
 59;
yeccgoto(let_expr, 257) ->
 59;
yeccgoto(let_expr, 260) ->
 59;
yeccgoto(let_expr, 265) ->
 59;
yeccgoto(let_vars, 58) ->
 213;
yeccgoto(let_vars, 82) ->
 85;
yeccgoto(let_vars, 88) ->
 89;
yeccgoto(letrec_expr, 33) ->
 61;
yeccgoto(letrec_expr, 35) ->
 61;
yeccgoto(letrec_expr, 36) ->
 61;
yeccgoto(letrec_expr, 37) ->
 61;
yeccgoto(letrec_expr, 40) ->
 61;
yeccgoto(letrec_expr, 44) ->
 61;
yeccgoto(letrec_expr, 46) ->
 61;
yeccgoto(letrec_expr, 48) ->
 61;
yeccgoto(letrec_expr, 52) ->
 61;
yeccgoto(letrec_expr, 70) ->
 61;
yeccgoto(letrec_expr, 74) ->
 61;
yeccgoto(letrec_expr, 79) ->
 61;
yeccgoto(letrec_expr, 86) ->
 61;
yeccgoto(letrec_expr, 90) ->
 61;
yeccgoto(letrec_expr, 99) ->
 61;
yeccgoto(letrec_expr, 164) ->
 61;
yeccgoto(letrec_expr, 166) ->
 61;
yeccgoto(letrec_expr, 171) ->
 61;
yeccgoto(letrec_expr, 203) ->
 61;
yeccgoto(letrec_expr, 211) ->
 61;
yeccgoto(letrec_expr, 214) ->
 61;
yeccgoto(letrec_expr, 216) ->
 61;
yeccgoto(letrec_expr, 218) ->
 61;
yeccgoto(letrec_expr, 226) ->
 61;
yeccgoto(letrec_expr, 232) ->
 61;
yeccgoto(letrec_expr, 235) ->
 61;
yeccgoto(letrec_expr, 257) ->
 61;
yeccgoto(letrec_expr, 260) ->
 61;
yeccgoto(letrec_expr, 265) ->
 61;
yeccgoto(literal, 284) ->
 288;
yeccgoto(literal, 285) ->
 297;
yeccgoto(literal, 290) ->
 291;
yeccgoto(literal, 295) ->
 291;
yeccgoto(literal, 298) ->
 304;
yeccgoto(literal, 301) ->
 302;
yeccgoto(literals, 290) ->
 292;
yeccgoto(literals, 295) ->
 296;
yeccgoto(module_attribute, 6) ->
 8;
yeccgoto(module_attribute, 315) ->
 316;
yeccgoto(module_definition, 0) ->
 3;
yeccgoto(module_defs, 8) ->
 15;
yeccgoto(module_defs, 316) ->
 317;
yeccgoto(module_export, 4) ->
 6;
yeccgoto(module_export, 314) ->
 315;
yeccgoto(nil, 33) ->
 62;
yeccgoto(nil, 35) ->
 62;
yeccgoto(nil, 36) ->
 62;
yeccgoto(nil, 37) ->
 62;
yeccgoto(nil, 40) ->
 62;
yeccgoto(nil, 44) ->
 62;
yeccgoto(nil, 46) ->
 62;
yeccgoto(nil, 48) ->
 62;
yeccgoto(nil, 52) ->
 62;
yeccgoto(nil, 65) ->
 62;
yeccgoto(nil, 70) ->
 62;
yeccgoto(nil, 74) ->
 62;
yeccgoto(nil, 79) ->
 62;
yeccgoto(nil, 86) ->
 62;
yeccgoto(nil, 90) ->
 62;
yeccgoto(nil, 96) ->
 62;
yeccgoto(nil, 97) ->
 62;
yeccgoto(nil, 98) ->
 62;
yeccgoto(nil, 99) ->
 62;
yeccgoto(nil, 100) ->
 62;
yeccgoto(nil, 114) ->
 62;
yeccgoto(nil, 115) ->
 62;
yeccgoto(nil, 120) ->
 62;
yeccgoto(nil, 126) ->
 139;
yeccgoto(nil, 129) ->
 139;
yeccgoto(nil, 142) ->
 139;
yeccgoto(nil, 147) ->
 139;
yeccgoto(nil, 151) ->
 139;
yeccgoto(nil, 154) ->
 139;
yeccgoto(nil, 162) ->
 62;
yeccgoto(nil, 164) ->
 62;
yeccgoto(nil, 166) ->
 62;
yeccgoto(nil, 171) ->
 62;
yeccgoto(nil, 174) ->
 62;
yeccgoto(nil, 177) ->
 62;
yeccgoto(nil, 200) ->
 62;
yeccgoto(nil, 203) ->
 62;
yeccgoto(nil, 211) ->
 62;
yeccgoto(nil, 214) ->
 62;
yeccgoto(nil, 216) ->
 62;
yeccgoto(nil, 218) ->
 62;
yeccgoto(nil, 222) ->
 62;
yeccgoto(nil, 226) ->
 62;
yeccgoto(nil, 232) ->
 62;
yeccgoto(nil, 235) ->
 62;
yeccgoto(nil, 257) ->
 62;
yeccgoto(nil, 260) ->
 62;
yeccgoto(nil, 265) ->
 62;
yeccgoto(nil, 284) ->
 62;
yeccgoto(nil, 285) ->
 62;
yeccgoto(nil, 290) ->
 62;
yeccgoto(nil, 295) ->
 62;
yeccgoto(nil, 298) ->
 62;
yeccgoto(nil, 301) ->
 62;
yeccgoto(other_pattern, 65) ->
 111;
yeccgoto(other_pattern, 96) ->
 186;
yeccgoto(other_pattern, 97) ->
 111;
yeccgoto(other_pattern, 98) ->
 111;
yeccgoto(other_pattern, 100) ->
 111;
yeccgoto(other_pattern, 114) ->
 111;
yeccgoto(other_pattern, 115) ->
 123;
yeccgoto(other_pattern, 120) ->
 111;
yeccgoto(other_pattern, 162) ->
 111;
yeccgoto(other_pattern, 174) ->
 111;
yeccgoto(other_pattern, 177) ->
 111;
yeccgoto(other_pattern, 200) ->
 111;
yeccgoto(other_pattern, 222) ->
 111;
yeccgoto(primop_expr, 33) ->
 64;
yeccgoto(primop_expr, 35) ->
 64;
yeccgoto(primop_expr, 36) ->
 64;
yeccgoto(primop_expr, 37) ->
 64;
yeccgoto(primop_expr, 40) ->
 64;
yeccgoto(primop_expr, 44) ->
 64;
yeccgoto(primop_expr, 46) ->
 64;
yeccgoto(primop_expr, 48) ->
 64;
yeccgoto(primop_expr, 52) ->
 64;
yeccgoto(primop_expr, 70) ->
 64;
yeccgoto(primop_expr, 74) ->
 64;
yeccgoto(primop_expr, 79) ->
 64;
yeccgoto(primop_expr, 86) ->
 64;
yeccgoto(primop_expr, 90) ->
 64;
yeccgoto(primop_expr, 99) ->
 64;
yeccgoto(primop_expr, 164) ->
 64;
yeccgoto(primop_expr, 166) ->
 64;
yeccgoto(primop_expr, 171) ->
 64;
yeccgoto(primop_expr, 203) ->
 64;
yeccgoto(primop_expr, 211) ->
 64;
yeccgoto(primop_expr, 214) ->
 64;
yeccgoto(primop_expr, 216) ->
 64;
yeccgoto(primop_expr, 218) ->
 64;
yeccgoto(primop_expr, 226) ->
 64;
yeccgoto(primop_expr, 232) ->
 64;
yeccgoto(primop_expr, 235) ->
 64;
yeccgoto(primop_expr, 257) ->
 64;
yeccgoto(primop_expr, 260) ->
 64;
yeccgoto(primop_expr, 265) ->
 64;
yeccgoto(receive_expr, 33) ->
 66;
yeccgoto(receive_expr, 35) ->
 66;
yeccgoto(receive_expr, 36) ->
 66;
yeccgoto(receive_expr, 37) ->
 66;
yeccgoto(receive_expr, 40) ->
 66;
yeccgoto(receive_expr, 44) ->
 66;
yeccgoto(receive_expr, 46) ->
 66;
yeccgoto(receive_expr, 48) ->
 66;
yeccgoto(receive_expr, 52) ->
 66;
yeccgoto(receive_expr, 70) ->
 66;
yeccgoto(receive_expr, 74) ->
 66;
yeccgoto(receive_expr, 79) ->
 66;
yeccgoto(receive_expr, 86) ->
 66;
yeccgoto(receive_expr, 90) ->
 66;
yeccgoto(receive_expr, 99) ->
 66;
yeccgoto(receive_expr, 164) ->
 66;
yeccgoto(receive_expr, 166) ->
 66;
yeccgoto(receive_expr, 171) ->
 66;
yeccgoto(receive_expr, 203) ->
 66;
yeccgoto(receive_expr, 211) ->
 66;
yeccgoto(receive_expr, 214) ->
 66;
yeccgoto(receive_expr, 216) ->
 66;
yeccgoto(receive_expr, 218) ->
 66;
yeccgoto(receive_expr, 226) ->
 66;
yeccgoto(receive_expr, 232) ->
 66;
yeccgoto(receive_expr, 235) ->
 66;
yeccgoto(receive_expr, 257) ->
 66;
yeccgoto(receive_expr, 260) ->
 66;
yeccgoto(receive_expr, 265) ->
 66;
yeccgoto(segment, 247) ->
 249;
yeccgoto(segment, 255) ->
 249;
yeccgoto(segment_pattern, 190) ->
 192;
yeccgoto(segment_pattern, 198) ->
 192;
yeccgoto(segment_patterns, 190) ->
 193;
yeccgoto(segment_patterns, 198) ->
 199;
yeccgoto(segments, 247) ->
 250;
yeccgoto(segments, 255) ->
 256;
yeccgoto(sequence, 33) ->
 67;
yeccgoto(sequence, 35) ->
 67;
yeccgoto(sequence, 36) ->
 67;
yeccgoto(sequence, 37) ->
 67;
yeccgoto(sequence, 40) ->
 67;
yeccgoto(sequence, 44) ->
 67;
yeccgoto(sequence, 46) ->
 67;
yeccgoto(sequence, 48) ->
 67;
yeccgoto(sequence, 52) ->
 67;
yeccgoto(sequence, 70) ->
 67;
yeccgoto(sequence, 74) ->
 67;
yeccgoto(sequence, 79) ->
 67;
yeccgoto(sequence, 86) ->
 67;
yeccgoto(sequence, 90) ->
 67;
yeccgoto(sequence, 99) ->
 67;
yeccgoto(sequence, 164) ->
 67;
yeccgoto(sequence, 166) ->
 67;
yeccgoto(sequence, 171) ->
 67;
yeccgoto(sequence, 203) ->
 67;
yeccgoto(sequence, 211) ->
 67;
yeccgoto(sequence, 214) ->
 67;
yeccgoto(sequence, 216) ->
 67;
yeccgoto(sequence, 218) ->
 67;
yeccgoto(sequence, 226) ->
 67;
yeccgoto(sequence, 232) ->
 67;
yeccgoto(sequence, 235) ->
 67;
yeccgoto(sequence, 257) ->
 67;
yeccgoto(sequence, 260) ->
 67;
yeccgoto(sequence, 265) ->
 67;
yeccgoto(single_expression, 33) ->
 68;
yeccgoto(single_expression, 35) ->
 68;
yeccgoto(single_expression, 36) ->
 68;
yeccgoto(single_expression, 37) ->
 68;
yeccgoto(single_expression, 40) ->
 68;
yeccgoto(single_expression, 44) ->
 68;
yeccgoto(single_expression, 46) ->
 68;
yeccgoto(single_expression, 48) ->
 68;
yeccgoto(single_expression, 52) ->
 68;
yeccgoto(single_expression, 70) ->
 68;
yeccgoto(single_expression, 74) ->
 68;
yeccgoto(single_expression, 79) ->
 68;
yeccgoto(single_expression, 86) ->
 68;
yeccgoto(single_expression, 90) ->
 68;
yeccgoto(single_expression, 99) ->
 68;
yeccgoto(single_expression, 164) ->
 68;
yeccgoto(single_expression, 166) ->
 68;
yeccgoto(single_expression, 171) ->
 68;
yeccgoto(single_expression, 203) ->
 68;
yeccgoto(single_expression, 211) ->
 68;
yeccgoto(single_expression, 214) ->
 68;
yeccgoto(single_expression, 216) ->
 68;
yeccgoto(single_expression, 218) ->
 68;
yeccgoto(single_expression, 226) ->
 68;
yeccgoto(single_expression, 232) ->
 68;
yeccgoto(single_expression, 235) ->
 68;
yeccgoto(single_expression, 257) ->
 68;
yeccgoto(single_expression, 260) ->
 68;
yeccgoto(single_expression, 265) ->
 68;
yeccgoto(tail, 231) ->
 234;
yeccgoto(tail, 238) ->
 239;
yeccgoto(tail_constant, 150) ->
 153;
yeccgoto(tail_constant, 157) ->
 158;
yeccgoto(tail_literal, 297) ->
 300;
yeccgoto(tail_literal, 304) ->
 305;
yeccgoto(tail_pattern, 173) ->
 176;
yeccgoto(tail_pattern, 180) ->
 181;
yeccgoto(timeout, 65) ->
 112;
yeccgoto(timeout, 101) ->
 168;
yeccgoto(try_expr, 33) ->
 71;
yeccgoto(try_expr, 35) ->
 71;
yeccgoto(try_expr, 36) ->
 71;
yeccgoto(try_expr, 37) ->
 71;
yeccgoto(try_expr, 40) ->
 71;
yeccgoto(try_expr, 44) ->
 71;
yeccgoto(try_expr, 46) ->
 71;
yeccgoto(try_expr, 48) ->
 71;
yeccgoto(try_expr, 52) ->
 71;
yeccgoto(try_expr, 70) ->
 71;
yeccgoto(try_expr, 74) ->
 71;
yeccgoto(try_expr, 79) ->
 71;
yeccgoto(try_expr, 86) ->
 71;
yeccgoto(try_expr, 90) ->
 71;
yeccgoto(try_expr, 99) ->
 71;
yeccgoto(try_expr, 164) ->
 71;
yeccgoto(try_expr, 166) ->
 71;
yeccgoto(try_expr, 171) ->
 71;
yeccgoto(try_expr, 203) ->
 71;
yeccgoto(try_expr, 211) ->
 71;
yeccgoto(try_expr, 214) ->
 71;
yeccgoto(try_expr, 216) ->
 71;
yeccgoto(try_expr, 218) ->
 71;
yeccgoto(try_expr, 226) ->
 71;
yeccgoto(try_expr, 232) ->
 71;
yeccgoto(try_expr, 235) ->
 71;
yeccgoto(try_expr, 257) ->
 71;
yeccgoto(try_expr, 260) ->
 71;
yeccgoto(try_expr, 265) ->
 71;
yeccgoto(tuple, 33) ->
 72;
yeccgoto(tuple, 35) ->
 72;
yeccgoto(tuple, 36) ->
 72;
yeccgoto(tuple, 37) ->
 72;
yeccgoto(tuple, 40) ->
 72;
yeccgoto(tuple, 44) ->
 72;
yeccgoto(tuple, 46) ->
 72;
yeccgoto(tuple, 48) ->
 72;
yeccgoto(tuple, 52) ->
 72;
yeccgoto(tuple, 70) ->
 72;
yeccgoto(tuple, 74) ->
 72;
yeccgoto(tuple, 79) ->
 72;
yeccgoto(tuple, 86) ->
 72;
yeccgoto(tuple, 90) ->
 72;
yeccgoto(tuple, 99) ->
 72;
yeccgoto(tuple, 164) ->
 72;
yeccgoto(tuple, 166) ->
 72;
yeccgoto(tuple, 171) ->
 72;
yeccgoto(tuple, 203) ->
 72;
yeccgoto(tuple, 211) ->
 72;
yeccgoto(tuple, 214) ->
 72;
yeccgoto(tuple, 216) ->
 72;
yeccgoto(tuple, 218) ->
 72;
yeccgoto(tuple, 226) ->
 72;
yeccgoto(tuple, 232) ->
 72;
yeccgoto(tuple, 235) ->
 72;
yeccgoto(tuple, 257) ->
 72;
yeccgoto(tuple, 260) ->
 72;
yeccgoto(tuple, 265) ->
 72;
yeccgoto(tuple_constant, 126) ->
 141;
yeccgoto(tuple_constant, 129) ->
 141;
yeccgoto(tuple_constant, 142) ->
 141;
yeccgoto(tuple_constant, 147) ->
 141;
yeccgoto(tuple_constant, 151) ->
 141;
yeccgoto(tuple_constant, 154) ->
 141;
yeccgoto(tuple_literal, 284) ->
 289;
yeccgoto(tuple_literal, 285) ->
 289;
yeccgoto(tuple_literal, 290) ->
 289;
yeccgoto(tuple_literal, 295) ->
 289;
yeccgoto(tuple_literal, 298) ->
 289;
yeccgoto(tuple_literal, 301) ->
 289;
yeccgoto(tuple_pattern, 65) ->
 113;
yeccgoto(tuple_pattern, 96) ->
 113;
yeccgoto(tuple_pattern, 97) ->
 113;
yeccgoto(tuple_pattern, 98) ->
 113;
yeccgoto(tuple_pattern, 100) ->
 113;
yeccgoto(tuple_pattern, 114) ->
 113;
yeccgoto(tuple_pattern, 115) ->
 113;
yeccgoto(tuple_pattern, 120) ->
 113;
yeccgoto(tuple_pattern, 162) ->
 113;
yeccgoto(tuple_pattern, 174) ->
 113;
yeccgoto(tuple_pattern, 177) ->
 113;
yeccgoto(tuple_pattern, 200) ->
 113;
yeccgoto(tuple_pattern, 222) ->
 113;
yeccgoto(variable, 25) ->
 31;
yeccgoto(variable, 26) ->
 267;
yeccgoto(variable, 33) ->
 73;
yeccgoto(variable, 35) ->
 73;
yeccgoto(variable, 36) ->
 73;
yeccgoto(variable, 37) ->
 73;
yeccgoto(variable, 40) ->
 73;
yeccgoto(variable, 44) ->
 73;
yeccgoto(variable, 46) ->
 73;
yeccgoto(variable, 48) ->
 73;
yeccgoto(variable, 52) ->
 73;
yeccgoto(variable, 58) ->
 31;
yeccgoto(variable, 65) ->
 31;
yeccgoto(variable, 70) ->
 73;
yeccgoto(variable, 74) ->
 73;
yeccgoto(variable, 79) ->
 73;
yeccgoto(variable, 82) ->
 31;
yeccgoto(variable, 83) ->
 31;
yeccgoto(variable, 86) ->
 73;
yeccgoto(variable, 88) ->
 31;
yeccgoto(variable, 90) ->
 73;
yeccgoto(variable, 96) ->
 124;
yeccgoto(variable, 97) ->
 31;
yeccgoto(variable, 98) ->
 31;
yeccgoto(variable, 99) ->
 73;
yeccgoto(variable, 100) ->
 31;
yeccgoto(variable, 114) ->
 31;
yeccgoto(variable, 115) ->
 124;
yeccgoto(variable, 120) ->
 31;
yeccgoto(variable, 162) ->
 31;
yeccgoto(variable, 164) ->
 73;
yeccgoto(variable, 166) ->
 73;
yeccgoto(variable, 171) ->
 73;
yeccgoto(variable, 174) ->
 31;
yeccgoto(variable, 177) ->
 31;
yeccgoto(variable, 200) ->
 31;
yeccgoto(variable, 203) ->
 73;
yeccgoto(variable, 211) ->
 73;
yeccgoto(variable, 214) ->
 73;
yeccgoto(variable, 216) ->
 73;
yeccgoto(variable, 218) ->
 73;
yeccgoto(variable, 222) ->
 31;
yeccgoto(variable, 226) ->
 73;
yeccgoto(variable, 232) ->
 73;
yeccgoto(variable, 235) ->
 73;
yeccgoto(variable, 257) ->
 73;
yeccgoto(variable, 260) ->
 73;
yeccgoto(variable, 263) ->
 31;
yeccgoto(variable, 265) ->
 73;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).
