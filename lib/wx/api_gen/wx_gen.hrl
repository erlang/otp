%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-record(class,
	{
	  name    = undefined,
	  parent  = undefined,
	  methods = [],
	  attributes = [],
	  event   = false,
	  file    = undefined,
	  options  = undefined,
	  abstract = false,
	  id,                      % Unique integer identifer
	  doc
	 }).

-record(method, 
	{
	  name        = undefined, % real name
	  alias       = undefined, % erlang func name
	  type        = void,      % method type
 	  min_arity   = undefined, % Required args
 	  max_arity   = undefined, % Optional args
	  defined_in  = undefined, % 
	  params      = [],        % arguments to the function
	  method_type = member,    % kind of method, e.g constructor
	  where       = both,      % Erl or c or both
	  id          = undefined, % Id (integer)
	  doc,                     % Extra documentation
	  virtual,                 % Is virtual?
	  pre_hook    = [],        % Pre hook  before call in c-code
	  post_hook   = [],        % Post hook after call in c-code
	  opts        = []         % Options
	}
       ).

-record(param,
	{
	  name    = undefined,
	  type    = undefined,
	  def     = none,
	  in      = true,
	  where   = both,     % both in c and erl or only in either 
	  prot    = public,   % only used by attributes
	  alt     = undefined,% c-only alternative usually a length indicator
	  acc                 % access function if protected and needed
	 }).

-record(type,
	{
	  name     = undefined,  % typename
	  base     = undefined,  % basename int, char, float ..
	  by_val   = true,       % or false = by_ref
	  single   = true,       % Single or array (list)
	  mod      = [],         % const ...
	  ref      = undefined,  % pointer or reference
	  xml      = undefined   % xml identity 
	 }
       ).

-record(enum, {from, skip="", as_atom=false, vals}).
-record(const,{name,val,enum,is_const}).

-define(error(What), 
	erlang:error({{?MODULE,?LINE},{get(current_class),get(current_func)},What})).

-define(warning(Str,Args),
	io:format("~p:~p Warning:"++Str, [?MODULE,?LINE] ++ Args)).
	
%-ifdef(TRACE_COMMENT).
%-define(WTC(X), w("// ~p:~p ~p~n",[?MODULE, ?LINE, X])).
%-else.
-define(WTC(X), void).
%-endif.
