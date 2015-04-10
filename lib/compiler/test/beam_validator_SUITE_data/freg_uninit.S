{module, freg_uninit}.  %% version = 0

{exports, [{sum_1,2},{sum_2,2}]}.

{attributes, []}.

{labels, 7}.


{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    {fconv,{x,0},{fr,0}}.
    fclearerror.
    {bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    return.


{function, sum_2, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,sum_2},2}.
  {label,4}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    fclearerror.
    {fcheckerror,{f,0}}.
    {call,2,{f,6}}.
    {bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    return.

{function, foo, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,foo},2}.
  {label,6}.
    return.
