{module, uninit}.  %% version = 0

{exports, [{sum_1,2},{sum_2,2},{sum_3,2}]}.

{attributes, []}.

{labels, 9}.

{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    {allocate,1,2}.
    {move,{y,0},{x,0}}.
    {'%live',1}.
    {call,1,{f,8}}.
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
    {call,1,{f,8}}.
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
    {call,1,{f,8}}.
    {bif,'+',{f,0},[{x,0},{y,0}],{x,0}}.
    {'%live',1}.
    {deallocate,1}.
    return.

{function, id, 1, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,id},1}.
  {label,8}.
    return.
