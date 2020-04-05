{module, call_without_stack}.  %% version = 0

{exports, [{remote,2},{local,2}]}.

{attributes, []}.

{labels, 9}.

{function, remote, 2, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,remote},2}.
  {label,2}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    if_end.

{function, local, 2, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,local},2}.
  {label,4}.
    {call,2,{f,2}}.
    if_end.
