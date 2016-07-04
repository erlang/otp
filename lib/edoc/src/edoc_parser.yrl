%% ========================== -*-Erlang-*- =============================
%% EDoc type specification grammar for the Yecc parser generator,
%% adapted from Sven-Olof Nyström's type specification parser.
%%
%% Also contains entry points for parsing things like typedefs,
%% references, and throws-declarations.
%%
%% Copyright (C) 2002-2005 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: carlsson.richard@gmail.com
%% =====================================================================

Nonterminals
start spec func_type utype_list utype_tuple utypes utype ptypes ptype
nutype function_name where_defs defs defs2 def typedef etype
throws qname ref aref mref lref var_list vars fields field
utype_map utype_map_fields utype_map_field
futype_list bin_base_type bin_unit_type.

Terminals
atom float integer var an_var string start_spec start_typedef start_throws
start_ref

'(' ')' ',' '.' '=>' ':=' '->' '{' '}' '[' ']' '|' '+' ':' '::' '=' '/' '//'
'*' '#' 'where' '<<' '>>' '..' '...'.

Rootsymbol start.

start -> start_spec spec: '$2'.
start -> start_throws throws: '$2'.
start -> start_typedef typedef: '$2'.
start -> start_ref ref: '$2'.

%% Produced in reverse order.
qname -> atom: [tok_val('$1')].
qname -> qname '.' atom: [tok_val('$3') | '$1'].

spec -> func_type where_defs:
    #t_spec{type = '$1', defs = '$2'}.
spec -> function_name func_type where_defs:
    #t_spec{name = '$1', type = '$2', defs = '$3'}.

where_defs -> 'where' defs: '$2'.
where_defs -> defs: '$1'.

function_name -> atom: #t_name{name = tok_val('$1')}.

func_type -> utype_list '->' utype:
    #t_fun{args = element(1, '$1'), range = '$3'}.


%% Paired with line number, for later error reporting
utype_list -> '(' utypes ')' : {lists:reverse('$2'), tok_line('$1')}.

futype_list -> utype_list : '$1'.
futype_list -> '(' '...' ')' : {[#t_var{name = '...'}], tok_line('$1')}.

utype_map -> '#' '{' utype_map_fields '}' : lists:reverse('$3').

utype_map_fields -> '$empty' : [].
utype_map_fields -> utype_map_field : ['$1'].
utype_map_fields -> utype_map_fields ',' utype_map_field : ['$3' | '$1'].

utype_map_field -> utype '=>' utype : #t_map_field{assoc_type = assoc,
                                                   k_type = '$1',
                                                   v_type = '$3'}.
utype_map_field -> utype ':=' utype : #t_map_field{assoc_type = exact,
                                                   k_type = '$1',
                                                   v_type = '$3'}.

utype_tuple -> '{' utypes '}' : lists:reverse('$2').

%% Produced in reverse order.
utypes -> '$empty' : [].
utypes -> utype : ['$1'].
utypes -> utypes ',' utype : ['$3' | '$1'].

utype -> nutype string: annotate('$1', tok_val('$2')).
utype -> nutype: '$1'.

nutype -> var '::' ptypes: annotate(union('$3'), tok_val('$1')).
nutype -> ptypes: union('$1').

%% Produced in reverse order.
ptypes -> ptype : ['$1'].
ptypes -> ptypes '+' ptype : ['$3' | '$1'].
ptypes -> ptypes '|' ptype : ['$3' | '$1'].

ptype -> var : #t_var{name = tok_val('$1')}.
ptype -> atom : #t_atom{val = tok_val('$1')}.
ptype -> integer: #t_integer{val = tok_val('$1')}.
ptype -> integer '..' integer: #t_integer_range{from = tok_val('$1'),
                                                  to = tok_val('$3')}.
ptype -> float: #t_float{val = tok_val('$1')}.
ptype -> utype_tuple : #t_tuple{types = '$1'}.
ptype -> utype_map : #t_map{types = '$1'}.
ptype -> '[' ']' : #t_nil{}.
ptype -> '[' utype ']' : #t_list{type = '$2'}.
ptype -> '[' utype ',' '...' ']' : #t_nonempty_list{type = '$2'}.
ptype -> utype_list:
	if length(element(1, '$1')) == 1 ->
		%% there must be exactly one utype in the list
                #t_paren{type = hd(element(1, '$1'))};
	   length(element(1, '$1')) == 0 ->
		return_error(element(2, '$1'), "syntax error before: ')'");
	   true ->
		return_error(element(2, '$1'), "syntax error before: ','")
	end.
ptype -> futype_list '->' ptype:
	#t_fun{args = element(1, '$1'), range = '$3'}.
ptype -> '#' atom '{' '}' :
        #t_record{name = #t_atom{val = tok_val('$2')}}.
ptype -> '#' atom '{' fields '}' :
	#t_record{name = #t_atom{val = tok_val('$2')},
		  fields = lists:reverse('$4')}.
ptype -> atom utype_list:
             case {tok_val('$1'), element(1, '$2')} of
                 {nil, []} ->
                     %% Prefer '[]' before 'nil(). Due to
                     %% compatibility with Erlang types, which do not
                     %% separate '[]' from 'nil()'.
                     #t_nil{};
                 {list, [T]} ->
                     %% Prefer '[T]' before 'list(T). Due to
                     %% compatibility with Erlang types, which do not
                     %% separate '[T]' from 'list(T)'.
                     #t_list{type = T};
                 {'fun', [#t_fun{}=Fun]} ->
                     %% An incompatible change as compared to EDOc 0.7.6.6.
                     %% Due to compatibility with Erlang types.
                     Fun;
                 {'fun', []} ->
                     #t_type{name = #t_name{name = function}};
                 {Name, Args} ->
                     #t_type{name = #t_name{name = Name},
                             args = Args}
             end.
ptype -> qname ':' atom utype_list :
	#t_type{name = #t_name{module = qname('$1'),
			       name = tok_val('$3')},
		args = element(1, '$4')}.
ptype -> '//' atom '/' qname ':' atom utype_list :
	#t_type{name = #t_name{app = tok_val('$2'),
			       module = qname('$4'),
			       name = tok_val('$6')},
		args = element(1, '$7')}.
ptype -> '<<' '>>' : #t_binary{}.
ptype -> '<<' bin_base_type '>>' : #t_binary{base_size = '$2'}.
ptype -> '<<' bin_unit_type '>>' : #t_binary{unit_size = '$2'}.
ptype -> '<<' bin_base_type ',' bin_unit_type '>>' :
        #t_binary{base_size = '$2', unit_size = '$4'}.

bin_base_type -> an_var ':' integer: tok_val('$3').

bin_unit_type -> an_var ':' an_var '*' integer : tok_val('$5').

%% Produced in reverse order.
fields -> field : ['$1'].
fields -> fields ',' field : ['$3' | '$1'].

field -> atom '=' utype :
	#t_field{name = #t_atom{val = tok_val('$1')}, type = '$3'}.

defs -> '$empty' : [].
defs -> def defs2 : ['$1' | lists:reverse('$2')].

%% Produced in reverse order.
defs2 -> '$empty' : [].
defs2 -> defs2 def : ['$2' | '$1'].
defs2 -> defs2 ',' def : ['$3' | '$1'].

def -> var '=' utype:
       #t_def{name =  #t_var{name = tok_val('$1')},
	      type = '$3'}.
def -> atom '(' utypes ')' '=' utype:
       build_def(tok_val('$1'), '$2', '$3', '$6').

var_list -> '(' ')' : [].
var_list -> '(' vars ')' : lists:reverse('$2').

%% Produced in reverse order.
vars -> var : [#t_var{name = tok_val('$1')}].
vars -> vars ',' var : [#t_var{name = tok_val('$3')} | '$1'].

typedef -> atom var_list where_defs:
       #t_typedef{name = #t_name{name = tok_val('$1')},
		  args = '$2',
		  defs = '$3'}.
typedef -> atom var_list '=' utype where_defs:
       #t_typedef{name = #t_name{name = tok_val('$1')},
		  args = '$2',
		  type = '$4',
		  defs = '$5'}.

%% References

ref -> aref: '$1'.
ref -> mref: '$1'.
ref -> lref: '$1'.

aref -> '//' atom:
    edoc_refs:app(tok_val('$2')).
aref -> '//' atom '/' mref:
    edoc_refs:app(tok_val('$2'), '$4').

mref -> qname ':' atom '/' integer:
    edoc_refs:function(qname('$1'), tok_val('$3'), tok_val('$5')).
mref -> qname ':' atom '(' ')':
    edoc_refs:type(qname('$1'), tok_val('$3')).
mref -> qname:
    edoc_refs:module(qname('$1')).

lref -> atom '/' integer:
    edoc_refs:function(tok_val('$1'), tok_val('$3')).
lref -> atom '(' ')':
    edoc_refs:type(tok_val('$1')).

%% Exception declarations

etype -> utype: '$1'.

throws -> etype where_defs:
	#t_throws{type = '$1',
		  defs = '$2'}.

%% (commented out for now)
%% Header
%% "%% ========================== -*-Erlang-*- ============================="
%% "%% EDoc function specification parser, generated from the file"
%% "%% \"edoc_parser.yrl\" by the Yecc parser generator."
%% "%%"
%% "%% Copyright (C) 2002-2005 Richard Carlsson"
%% "%%"
%% "%% This library is free software; you can redistribute it and/or modify"
%% "%% it under the terms of the GNU Lesser General Public License as"
%% "%% published by the Free Software Foundation; either version 2 of the"
%% "%% License, or (at your option) any later version."
%% "%%"
%% "%% This library is distributed in the hope that it will be useful, but"
%% "%% WITHOUT ANY WARRANTY; without even the implied warranty of"
%% "%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU"
%% "%% Lesser General Public License for more details."
%% "%%"
%% "%% You should have received a copy of the GNU Lesser General Public"
%% "%% License along with this library; if not, write to the Free Software"
%% "%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307"
%% "%% USA"
%% "%%"
%% "%% @private"
%% "%% @author Richard Carlsson <carlsson.richard@gmail.com>"
%% "%% ===================================================================="
%% .

Erlang code.

%% ========================== -*-Erlang-*- =============================
%% EDoc function specification parser, generated from the file
%% "edoc_parser.yrl" by the Yecc parser generator.
%%
%% Copyright (C) 2002-2005 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%% ====================================================================

-export([parse_spec/2, parse_typedef/2, parse_throws/2, parse_ref/2,
	 parse_see/2, parse_param/2]).

-include("edoc_types.hrl").

%% Multiple entry point hack:

start_spec(Ts, L) -> run_parser(Ts, L, start_spec).

start_typedef(Ts, L) -> run_parser(Ts, L, start_typedef).

start_throws(Ts, L) -> run_parser(Ts, L, start_throws).

start_ref(Ts, L) -> run_parser(Ts, L, start_ref).

%% Error reporting fix

run_parser(Ts, L, Start) ->
    case parse([{Start,L} | Ts]) of
	{error, {999999,?MODULE,_}} ->
	    What = case Start of
		       start_spec -> "specification";
		       start_typedef -> "type definition";
		       start_throws -> "exception declaration";
		       start_ref -> "reference"
		   end,
	    {error, {L,?MODULE,["unexpected end of ", What]}};
	Other -> Other
    end.

%% Utility functions:

tok_val(T) -> element(3, T).

tok_line(T) -> element(2, T).

qname([A]) -> A.

union(Ts) ->
    case Ts of
	[T] -> T;
	_ -> #t_union{types = lists:reverse(Ts)}
    end.

annotate(T, A) -> ?add_t_ann(T, A).

build_def(S, P, As, T) ->
    case all_vars(As) of
        true ->
            #t_def{name = #t_type{name = #t_name{name = S},
                                  args = lists:reverse(As)},
                   type = T};
        false ->
            return_error(tok_line(P), "variable expected after '('")
    end.

all_vars([#t_var{} | As]) ->
    all_vars(As);
all_vars(As) ->
    As =:= [].

%% ---------------------------------------------------------------------

%% @doc EDoc type specification parsing. Parses the content of
%% <a href="overview-summary.html#ftag-spec">`@spec'</a> declarations.

parse_spec(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_spec(Ts, L) of
		{ok, Spec} ->
		    Spec;
		{error, E} ->
		    throw_error({parse_spec, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_spec, E}, L)
    end.

%% ---------------------------------------------------------------------

%% @doc EDoc type definition parsing. Parses the content of
%% <a href="overview-summary.html#gtag-type">`@type'</a> declarations.

parse_typedef(S, L) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    N = edoc_lib:count($\n, S1),
    L1 = L + N,
    Text = edoc_lib:strip_space(S2),
    {parse_typedef_1(S1, L), edoc_wiki:parse_xml(Text, L1)}.

parse_typedef_1(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_typedef(Ts, L) of
		{ok, T} ->
		    T;
		{error, E} ->
		    throw_error({parse_typedef, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_typedef, E}, L)
    end.

%% ---------------------------------------------------------------------

%% @doc Parses a <a
%% href="overview-summary.html#References">reference</a> to a module,
%% function, type, or application

parse_ref(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_ref(Ts, L) of
		{ok, T} ->
		    T;
		{error, E} ->
		    throw_error({parse_ref, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_ref, E}, L)
    end.

%% ---------------------------------------------------------------------

%% @doc Parses the content of
%% <a href="overview-summary.html#ftag-see">`@see'</a> references.
parse_see(S, L) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    N = edoc_lib:count($\n, S1),
    L1 = L + N,
    Text = edoc_lib:strip_space(S2),
    {parse_ref(S1, L), edoc_wiki:parse_xml(Text, L1)}.

%% ---------------------------------------------------------------------

%% @doc Parses the content of
%% <a href="overview-summary.html#ftag-param">`@param'</a> tags.
parse_param(S, L) ->
    {S1, S2} = edoc_lib:split_at_space(edoc_lib:strip_space(S)),
    case edoc_lib:strip_space(S1) of
	"" -> throw_error(parse_param, L);
	Name ->
	    Text = edoc_lib:strip_space(S2),
	    {list_to_atom(Name), edoc_wiki:parse_xml(Text, L)}
    end.

%% ---------------------------------------------------------------------

%% @doc EDoc exception specification parsing. Parses the content of
%% <a href="overview-summary.html#ftag-throws">`@throws'</a> declarations.

parse_throws(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_throws(Ts, L) of
		{ok, Spec} ->
		    Spec;
		{error, E} ->
		    throw_error({parse_throws, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_throws, E}, L)
    end.

%% ---------------------------------------------------------------------

-spec throw_error(term(), erl_anno:line()) -> no_return().

throw_error({parse_spec, E}, L) ->
    throw_error({"specification", E}, L);
throw_error({parse_typedef, E}, L) ->
    throw_error({"type definition", E}, L);
throw_error({parse_ref, E}, L) ->
    throw_error({"reference", E}, L);
throw_error({parse_throws, E}, L) ->
    throw_error({"throws-declaration", E}, L);
throw_error(parse_param, L) ->
    throw({error, L, "missing parameter name"});
throw_error({Where, E}, L) when is_list(Where) ->
    throw({error,L,{"unknown error parsing ~ts: ~P.",[Where,E,15]}}).

%% vim: ft=erlang
