{module, undef_label}.  %% version = 0

{exports, []}.

{attributes, []}.

{labels, 7}.


{function, t, 1, 2}.
  {label,1}.
    {func_info,{atom,undef_label},{atom,t},1}.
  {label,2}.
    {test,is_eq_exact,{f,42},[{x,0},{atom,x}]}.
    {move,{atom,ok},{x,0}}.
    return.

{function, x, 1, 17}.
  {label,3}.
    {func_info,{atom,undef_label},{atom,x},1}.
  {label,4}.
    return.
