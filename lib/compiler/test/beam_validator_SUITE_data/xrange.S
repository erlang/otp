{module, xrange}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{prod,2},{sum,2},{sum_prod,3}]}.

{attributes, []}.

{labels, 8}.


{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    {bif,'+',{f,0},[{x,-1},{x,1}],{x,0}}.
    {'%live',1}.
    return.


{function, sum_2, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,sum_2},2}.
  {label,4}.
    {bif,'+',{f,0},[{x,0},{x,1024}],{x,0}}.
    {'%live',1}.
    return.


{function, sum_3, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,sum_3},2}.
  {label,6}.
    {bif,'+',{f,0},[{x,0},{x,1}],{x,-1}}.
    {'%live',1}.
    return.


{function, sum_4, 2, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,sum_4},2}.
  {label,8}.
    {bif,'+',{f,0},[{x,0},{x,1}],{x,1024}}.
    {'%live',1}.
    return.

