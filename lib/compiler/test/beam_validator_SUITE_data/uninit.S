{module, uninit}.  %% version = 0

{exports, []}.

{attributes, []}.

{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    {allocate,1,2}.
    {move,{y,0},{x,0}}.
    {'%live',1}.
    {call,1,{f,10}}.
    {bif,'+',{f,0},[{x,0},{y,0}],{x,0}}.
    {'%live',1}.
    {deallocate,1}.
    return.

{function, sum_2, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,sum_2},2}.
  {label,4}.
    {allocate,1,2}.
    {'%live',1}.
    {call,1,{f,10}}.
    {bif,'+',{f,0},[{x,0},{y,0}],{x,0}}.
    {'%live',1}.
    {deallocate,1}.
    return.

{function, sum_3, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,sum_3},2}.
  {label,6}.
    {allocate_zero,1,2}.
    {'%live',1}.
    {call,1,{f,10}}.
    {bif,'+',{f,0},[{x,0},{y,0}],{x,0}}.
    {'%live',1}.
    {deallocate,1}.
    return.

{function, id, 1, 10}.
  {label,9}.
    {func_info,{atom,t},{atom,id},1}.
  {label,10}.
    return.
