{module, state_after_fault_in_catch}.  %% version = 0

{exports, [{foo,1},{if_end,1},{case_end,1},{badmatch,1}]}.

{attributes, []}.

{labels, 12}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,foo},1}.
  {label,2}.
    {allocate,1,0}.
    {'catch',{y,0},{f,3}}.
    {move,{atom,apa},{x,0}}.
    {call_ext,1,{extfunc,erlang,error,1}}.
  {label,3}.
    {catch_end,{y,0}}.
    {move,{x,1},{x,0}}.
    return.

{function, if_end, 1, 5}.
  {label,4}.
    {func_info,{atom,state_after_fault_in_catch},{atom,if_end},1}.
  {label,5}.
    {allocate,1,0}.
    {'catch',{y,0},{f,6}}.
    if_end.
  {label,6}.
    {catch_end,{y,0}}.
    {move,{x,1},{x,0}}.
    return.

{function, case_end, 1, 8}.
  {label,7}.
    {func_info,{atom,state_after_fault_in_catch},{atom,case_end},1}.
  {label,8}.
    {allocate,1,1}.
    {'catch',{y,0},{f,9}}.
    {case_end,{x,0}}.
  {label,9}.
    {catch_end,{y,0}}.
    {move,{x,1},{x,0}}.
    return.

{function, badmatch, 1, 11}.
  {label,10}.
    {func_info,{atom,state_after_fault_in_catch},{atom,badmatch},1}.
  {label,11}.
    {allocate,1,1}.
    {'catch',{y,0},{f,12}}.
    {badmatch,{x,0}}.
  {label,12}.
    {catch_end,{y,0}}.
    {move,{x,1},{x,0}}.
    return.

