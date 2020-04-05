{module, branch_to_try_handler}.  %% version = 0

{exports, [{main,1}]}.

{attributes, []}.

{labels, 11}.

{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"t.erl",4}]}.
    {func_info,{atom,branch_to_try_handler},{atom,main},1}.
  {label,2}.
    {allocate,2,1}.
    {move,{x,0},{y,0}}.
    {'try',{y,1},{f,3}}.
    {move,{atom,ignored},{x,0}}.
    {line,[{location,"t.erl",6}]}.
    {call,1,{f,6}}.
    {'%',{type_info,{x,0},{t_atom,[ignored]}}}.
    {line,[{location,"t.erl",7}]}.
    %%
    %% Fail directly to the try handler instead of throwing an exception; this
    %% will crash the emulator.
    %%
    {bif,tuple_size,{f,3},[{y,0}],{x,0}}.
    %%
    {test,is_eq_exact,{f,4},[{x,0},{integer,1}]}.
    {move,{atom,error},{x,0}}.
    {try_end,{y,1}}.
    {deallocate,2}.
    return.
  {label,3}.
    {try_case,{y,1}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,2}.
    return.
  {label,4}.
    {line,[{location,"t.erl",7}]}.
    {badmatch,{x,0}}.

{function, id, 1, 6}.
  {label,5}.
    {line,[{location,"t.erl",13}]}.
    {func_info,{atom,branch_to_try_handler},{atom,id},1}.
  {label,6}.
    {'%',{type_info,{x,0},{t_atom,[ignored]}}}.
    return.
