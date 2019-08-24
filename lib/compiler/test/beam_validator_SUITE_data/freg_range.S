{module, freg_range}.  %% version = 0

{exports, [{sum_1,2},{sum_2,2},{sum_3,2},{sum_4,2}]}.

{attributes, []}.

{labels, 9}.


{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    fclearerror.
    {bif,fadd,{f,0},[{fr,-1},{fr,1}],{fr,0}}.
    {'%live',1}.
    return.


{function, sum_2, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,sum_2},2}.
  {label,4}.
    {fconv,{x,0},{fr,0}}.
    fclearerror.
    {bif,fadd,{f,0},[{fr,0},{fr,1024}],{fr,0}}.
    {'%live',1}.
    return.


{function, sum_3, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,sum_3},2}.
  {label,6}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    fclearerror.
    {bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,-1}}.
    {'%live',1}.
    return.


{function, sum_4, 2, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,sum_4},2}.
  {label,8}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    fclearerror.
    {bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,1024}}.
    {'%live',1}.
    return.

