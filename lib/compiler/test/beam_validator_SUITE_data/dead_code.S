{module, dead_code}.  %% version = 0

{exports, [{execute,0}]}.

{attributes, []}.

{labels, 6}.


{function, execute, 0, 2}.
  {label,1}.
    {func_info,{atom,dead_code},{atom,execute},0}.
  {label,2}.
    {allocate,0,0}.
    {call_ext,0,{extfunc,foo,fie,0}}.
    {test,is_ne,{f,4},[{x,0},{integer,0}]}.
    {test,is_ne,{f,4},[{x,0},{integer,1}]}.
  {label,3}.
    {test,is_ne,{f,4},[{x,0},{integer,2}]}.
    {test,is_ne,{f,4},[{x,0},{integer,3}]}.
    {case_end,{x,0}}.
  {label,4}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,5}.
    {case_end,{x,0}}.
