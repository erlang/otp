
-record(func,
	{
	  name        = undefined, % real name
	  type        = void,      % method type
	  params      = [],        % arguments to the function
	  where       = both,      % C code and/or erlang
	  id          = undefined, % Integer
	  alt         = undefined, % Function alternative
	  ext         = undefined  % Have ARB or other extension
	 }
       ).

-record(arg,
	{
	  name    = undefined,
	  type    = undefined,
	  in      = true,
	  where   = both, % both in c and erl or only in either 
	  alt     = undefined
	 }).

-record(type,
	{
	  name     = undefined,  % typename
	  base     = undefined,  % basename int, char, float ..
	  size     = 4,          % in bytes
	  by_val   = true,       % or false = by_ref
	  single   = true,       % Single or array (list)
	  ref      = undefined,  % {pointer, N}
	  mod      = []          % const ...
	 }
       ).

-record(def, {name, val, type}).  %% type=hex, int, string

-define(error(What), 
	erlang:error({{?MODULE,?LINE},{get(current_func)},What})).

-define(warning(Str,Args),
	io:format("~p:~p Warning:"++Str, [?MODULE,?LINE] ++ Args)).
