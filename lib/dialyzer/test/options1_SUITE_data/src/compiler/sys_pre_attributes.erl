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
%%     $Id: sys_pre_attributes.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
%% Purpose : Transform Erlang compiler attributes

-module(sys_pre_attributes).

-export([parse_transform/2]).

-define(OPTION_TAG, attributes).

-record(state, {forms,
		pre_ops = [],
		post_ops = [],
		options}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inserts, deletes and replaces Erlang compiler attributes.
%%
%% Valid options are:
%%
%%    {attribute, insert,  AttrName, NewAttrVal}
%%    {attribute, replace, AttrName, NewAttrVal}   % replace first occurrence
%%    {attribute, delete,  AttrName}
%%
%% The transformation is performed in two passes:
%%
%% pre_transform
%% -------------
%% Searches for attributes in the list of Forms in order to
%% delete or replace them. 'delete' will delete all occurrences
%% of attributes with the given name. 'replace' will replace the
%% first occurrence of the attribute. This pass is will only be
%% performed if there are replace or delete operations stated
%% as options.
%%
%% post_transform
%% -------------
%% Looks up the module attribute and inserts the new attributes
%% directly after. This pass will only be performed if there are
%% any attributes left to be inserted after pre_transform. The left
%% overs will be those replace operations that not has been performed
%% due to that the pre_transform pass did not find the attribute plus
%% all insert operations.

parse_transform(Forms, Options) ->
    S = #state{forms = Forms, options = Options},
    S2 = init_transform(S),
    report_verbose("Pre  options: ~p~n", [S2#state.pre_ops], S2),
    report_verbose("Post options: ~p~n", [S2#state.post_ops], S2),
    S3 = pre_transform(S2),
    S4 = post_transform(S3),
    S4#state.forms.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Computes the lists of pre_ops and post_ops that are
%% used in the real transformation.
init_transform(S) ->
    case S#state.options of
	Options when list(Options) ->
	    init_transform(Options, S);
	Option ->
	    init_transform([Option], S)
    end.

init_transform([{attribute, insert, Name, Val} | Tail], S) ->
    Op = {insert, Name, Val},
    PostOps = [Op | S#state.post_ops],
    init_transform(Tail, S#state{post_ops = PostOps});
init_transform([{attribute, replace, Name, Val} | Tail], S) ->
    Op = {replace, Name, Val},
    PreOps = [Op | S#state.pre_ops],
    PostOps = [Op | S#state.post_ops],
    init_transform(Tail, S#state{pre_ops = PreOps, post_ops = PostOps});
init_transform([{attribute, delete, Name} | Tail], S) ->
    Op = {delete, Name},
    PreOps = [Op | S#state.pre_ops],
    init_transform(Tail, S#state{pre_ops = PreOps});
init_transform([], S) ->
    S;
init_transform([_ | T], S) ->
    init_transform(T, S);
init_transform(BadOpt, S) ->
    report_error("Illegal option (ignored): ~p~n", [BadOpt], S),
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle delete and perhaps replace

pre_transform(S) when S#state.pre_ops == [] ->
    S;
pre_transform(S) ->
    pre_transform(S#state.forms, [], S).

pre_transform([H | T], Acc, S) ->
    case H of
	{attribute, Line, Name, Val} ->
	    case lists:keysearch(Name, 2, S#state.pre_ops) of
		false ->
		    pre_transform(T, [H | Acc], S);

		{value, {replace, Name, NewVal}} ->
		    report_warning("Replace attribute ~p: ~p -> ~p~n",
				   [Name, Val, NewVal],
				   S),
		    New = {attribute, Line, Name, NewVal},
		    Pre = lists:keydelete(Name, 2, S#state.pre_ops),
		    Post = lists:keydelete(Name, 2, S#state.post_ops),
		    S2 = S#state{pre_ops = Pre, post_ops = Post},
		    if
			Pre == [] ->
			    %% No need to search the rest of the Forms
			    Forms = lists:reverse(Acc, [New | T]),
			    S2#state{forms = Forms};
			true ->
			    pre_transform(T, [New | Acc], S2)
		    end;

		{value, {delete, Name}} ->
		    report_warning("Delete attribute ~p: ~p~n",
				   [Name, Val],
				   S),
		    pre_transform(T, Acc, S)
	    end;
	_Any ->
	    pre_transform(T, [H | Acc], S)
    end;
pre_transform([], Acc, S) ->
    S#state{forms = lists:reverse(Acc)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle insert and perhaps replace

post_transform(S) when S#state.post_ops == [] ->
    S;
post_transform(S) ->
    post_transform(S#state.forms, [], S).

post_transform([H | T], Acc, S) ->
    case H of
	{attribute, Line, module, Val} ->
	    Acc2 = lists:reverse([{attribute, Line, module, Val} | Acc]),
	    Forms = Acc2 ++ attrs(S#state.post_ops, Line, S) ++ T,
	    S#state{forms = Forms, post_ops = []};
	_Any ->
	    post_transform(T, [H | Acc], S)
    end;
post_transform([], Acc, S) ->
    S#state{forms = lists:reverse(Acc)}.

attrs([{replace, Name, NewVal} | T], Line, S) ->
    report_verbose("Insert attribute ~p: ~p~n", [Name, NewVal], S),
    [{attribute, Line, Name, NewVal} | attrs(T, Line, S)];
attrs([{insert, Name, NewVal} | T], Line, S) ->
    report_verbose("Insert attribute ~p: ~p~n", [Name, NewVal], S),
    [{attribute, Line, Name, NewVal} | attrs(T, Line, S)];
attrs([], _, _) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Report functions.
%%
%% Errors messages are controlled with the 'report_errors' compiler option
%% Warning messages are controlled with the 'report_warnings' compiler option
%% Verbose messages are controlled with the 'verbose' compiler option

report_error(Format, Args, S) ->
    case is_error(S) of
	true ->
	    io:format("~p: * ERROR * " ++ Format, [?MODULE | Args]);
	false ->
	    ok
    end.

report_warning(Format, Args, S) ->
    case is_warning(S) of
	true ->
	    io:format("~p: * WARNING * " ++ Format, [?MODULE | Args]);
	false ->
	    ok
    end.

report_verbose(Format, Args, S) ->
    case is_verbose(S) of
	true ->
	    io:format("~p: " ++ Format, [?MODULE | Args]);
	false ->
	    ok
    end.

is_error(S) ->
    lists:member(report_errors, S#state.options) or is_verbose(S).

is_warning(S) ->
    lists:member(report_warnings, S#state.options) or is_verbose(S).

is_verbose(S) ->
    lists:member(verbose, S#state.options).
