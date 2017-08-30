{module, freg_state}.  %% version = 0

{exports, [{sum_1,2},{sum_2,2},{sum_3,2},{sum_4,2},{sum_5,2}]}.

{attributes, []}.


{function, sum_1, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,sum_1},2}.
  {label,2}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    {bif,fmul,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    {'%live',1}.
    return.

{function, sum_2, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,sum_2},2}.
  {label,4}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    fclearerror.
    {bif,fmul,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    {fmove,{fr,0},{x,0}}.
    {'%live',1}.
    return.

{function, sum_3, 2, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,sum_3},2}.
  {label,6}.
    {fconv,{x,0},{fr,0}}.
    {fconv,{x,1},{fr,1}}.
    fclearerror.
    {bif,fmul,{f,0},[{fr,0},{fr,1}],{fr,0}}.
    {bif,'-',{f,0},[{x,1},{x,0}],{x,1}}.
    {fcheckerror,{f,0}}.
    {fmove,{fr,0},{x,0}}.
    {'%live',1}.
    return.

{function, sum_4, 2, 8}.
  {label,6}.
    {func_info,{atom,t},{atom,sum_4},2}.
  {label,8}.
    {fcheckerror,{f,0}}.
    {fmove,{fr,0},{x,0}}.
    {'%live',1}.
    return.

{function, sum_5, 2, 10}.
  {label,9}.
    {func_info,{atom,t},{atom,sum_5},2}.
  {label,10}.
    fclearerror.
    fclearerror.
    return.
