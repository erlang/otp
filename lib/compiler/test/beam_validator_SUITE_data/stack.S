{module, stack}.  %% version = 0

{exports, [{a,2},{b,2},{c,2},{d,2},{e,2},{bad_1,0},{bad_2,0},{foo,0}]}.

{attributes, []}.

{labels, 17}.


{function, a, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,a},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,1}}.
    {move,{x,0},{y,0}}.
    {'%live',0}.
    {call,0,{f,16}}.
    {bif,'+',{f,0},[{y,0},{y,1}],{x,0}}.
    {'%live',1}.
    return.


{function, b, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,b},2}.
  {label,4}.
    {deallocate,2}.
    return.


{function, c, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,c},2}.
  {label,6}.
    {allocate,2,2}.
    {move,{x,1},{y,1}}.
    {move,{x,0},{y,0}}.
    {'%live',0}.
    {call,0,{f,16}}.
    {bif,'+',{f,0},[{y,0},{y,1}],{x,0}}.
    {'%live',1}.
    {deallocate,2}.
    {deallocate,2}.
    return.


{function, d, 2, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,d},2}.
  {label,8}.
    {allocate,2,2}.
    {allocate,2,2}.
    {deallocate,2}.
    return.

{function, e, 2, 14}.
  {label,13}.
    {func_info,{atom,t},{atom,e},2}.
  {label,14}.
    {allocate,2,2}.
    {move,{atom,ok},{x,0}}.
    {deallocate,5}.
    return.

	
{function, bad_1, 0, 10}.
  {label,9}.
    {func_info,{atom,t},{atom,bad_1},0}.
  {label,10}.
    {allocate_zero,2,10}.
    {move,{y,0},{x,0}}.
    return.


{function, bad_2, 0, 12}.
  {label,11}.
    {func_info,{atom,t},{atom,bad_2},0}.
  {label,12}.
    {allocate_zero,2,0}.
    {move,{y,0},{x,0}}.
    return.

{function, foo, 0, 16}.
  {label,15}.
    {func_info,{atom,t},{atom,foo},0}.
  {label,16}.
    {move,{atom,ok},{x,0}}.
    return.
