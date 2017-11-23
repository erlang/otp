%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(add_fun_trans).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    Export = {attribute,1,export,[{e, 0}]},
    Function = {function, 1, e, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    NForms = insert_export(Export, Forms, []),
    insert_form(Function, NForms, []).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_export(Export, [{attribute,_Line,module,_Mod} = Module|T], Acc) ->
    lists:reverse(Acc) ++ [Module,Export] ++ T;
insert_export(Export, [Form|Forms], Acc) ->
    insert_export(Export, Forms, [Form|Acc]).

insert_form(Fun, [{eof, _ELine} = EOF|T], Heads) ->
    lists:reverse(Heads) ++ [Fun,EOF|T];
insert_form(Fun, [H|T], Heads) ->
    insert_form(Fun, T, [H|Heads]).
