-module(toth).
-export([sys_table_view/1]).

%%% Constants
-define(sysTabETS,1).
-define(sysTabMnesia,2).
-define(sysTabBoth,3).

sys_table_view([CpId,{match,Pattern},TableType, ViewType]) ->
    AllTableList =
	case TableType of
	    ?sysTabMnesia ->
		lists:sort(mnesia:system_info(tables));
	    ?sysTabBoth ->
		lists:sort(rpc:call(CpId,ets,all,[]));
	    ?sysTabETS ->
		lists:sort(rpc:call(CpId,ets,all,[]) --
			   mnesia:system_info(tables));
	    _ -> %%% Happens at registration only
		[ok]
    end,
    %% Filter the matching table names, skip unnamed tables first:
    NamedTableList = lists:filter(fun (X) -> is_atom(X) end, AllTableList),
    TablesShown =
	case Pattern of
	    "" ->
		NamedTableList;
	    _ ->
		%% Filter the ones whose name begins with the Pattern:
		Filter = fun(T) ->
				 lists:prefix(Pattern, atom_to_list(T))
			 end,
		lists:filter(Filter, NamedTableList)
	end,

    Fields = [{text, [{value,"CpId: " ++ atom_to_list(CpId)}]},
	      {text, [{value,"TabSpec=" ++ Pattern},
		      {value_format, term}]},
	      {text, [{value,"Table type: " ++ formatTableType(TableType)},
		      {value_format, term}]}],

    Template = [[{type, index},
		 {link, {?MODULE, sys_table_browse,
			 [{"CpId",CpId},{"TableType",TableType},
			  {"View", ViewType},
			  {"FirstKey",1}, {"KeyPattern",""}]}}],

		[{type, data},
		 {title, "Table name"},
		 {display_value, {erlang, atom_to_list}}], %%% else crash

                [{type,data},
		 {title, "No of rows"},
		 {display_value, term}],

                [{type,data},
		 {title, "Memory"},
		 {display_value, term}]
	       ],

    TableAttr = [{rows, [[T,T|tableSize(T,TableType,CpId)] ||
			    T <- TablesShown]},
		 {template,Template}],

    Page = [{header, {"Filter tables", "Selected tables"}},
	    {buttons, [reload, back]},
	    {layout, [{form, Fields},
		      {table, TableAttr}]}
	   ],
    Page.

%%--------------------------------------------------------------------
%% tableSize/3
%% @spec tableSize(T::atom(),TableType::integer(),CpId::atom()) ->
%%                 list(integer())
%% @doc Return the table size and memory size of the table.
%% @end
%%---------------------------------------------------------------------

tableSize(T, TableType, CpId) ->
    case TableType of
        ?sysTabETS ->
	    [rpc:call(CpId, ets, info, [T, size]),
	     rpc:call(CpId, ets, info, [T, memory])];
	?sysTabMnesia ->
            [mnesia:table_info(T, size),mnesia:table_info(T, memory)];
	_ -> %%% Registration
	    [0,0]
    end.

formatTableType(T) ->
    case T of
	?sysTabETS ->
	    "ETS";
	?sysTabMnesia ->
            "mnesia";
	_ -> %%% Registration !
	    "ETS + mnesia"
    end.
