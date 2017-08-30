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
-module(ic_constant_java).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([gen/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: gen/3
%%-----------------------------------------------------------------
gen(G, N, X) when is_record(X, const) ->
    ConstantName = ic_forms:get_java_id(X),
    case inInterface(G, N) of
	true ->
	    emit_constant(G, N, X, ConstantName);
	false ->
	    emit_constant_interface(G, N, X, ConstantName)
    end;
gen(_G, _N, _X) -> 
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: emit_constant/4
%%-----------------------------------------------------------------
emit_constant(G, N, X, ConstantName) -> 
    Fd = ic_genobj:interfacefiled(G),
    %%?PRINTDEBUG2("~p", [Fd]),
    Type = ic_java_type:getType(G, N, ic_forms:get_type(X)),
    ic_codegen:emit(Fd, "    public static final ~s ~s = (~s) ~p;\n",
		    [Type, ConstantName, Type, X#const.val]),
    ic_codegen:nl(Fd).

%%-----------------------------------------------------------------
%% Func: emit_constant_interface/4
%%-----------------------------------------------------------------
emit_constant_interface(G, N, X, ConstantName) ->
    {Fd, _} = ic_file:open_java_file(G, N, ConstantName), 
    
    ic_codegen:emit(Fd, "final public class ~s {\n",[ConstantName]),

    Type = ic_java_type:getType(G, N, ic_forms:get_type(X)),
    ic_codegen:emit(Fd, "   public static final ~s value = (~s) ~p;\n",
		    [Type, Type, X#const.val]),
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func: emit_constant_interface/4
%%-----------------------------------------------------------------
inInterface(_G, []) -> % Global constant
    false;
inInterface(G, N) -> 
    [N1 |Ns] = N,
    {_FullScopedName, T, _TK, _} =
	ic_symtab:get_full_scoped_name(G, Ns, ic_symtab:scoped_id_new(N1)),
    case T of
	interface -> % Constant declare in an interface
	    true;
	_ ->         % Constant declared in a module
	    false
    end.

