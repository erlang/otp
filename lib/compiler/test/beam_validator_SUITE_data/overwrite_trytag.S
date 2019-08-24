{module, overwrite_trytag}.  %% version = 0

{exports, [{foo,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,overwrite_trytag},{atom,foo},1}.
  {label,2}.
    {allocate_zero,3,1}.
    {'try',{y,2},{f,3}}.
    {move,{x,0},{y,1}}.
    {'%live',1}.
    {call,1,{f,2}}.
    {kill,{y,2}}.
    {try_end,{y,2}}.
    {move,{x,0},{y,2}}.
    {move,{y,1},{x,0}}.
    {call,1,{f,2}}.
    {move,{y,2},{x,0}}.
    {jump,{f,4}}.
  {label,3}.
    {try_case,{y,2}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,0}}.
    {move,{y,1},{x,0}}.
    {call,1,{f,2}}.
    {bif,raise,{f,0},[{y,2},{y,0}],{x,0}}.
    {'%live',1}.
  {label,4}.
    {deallocate,3}.
    return.


{function, module_info, 0, 6}.
  {label,5}.
    {func_info,{atom,overwrite_trytag},{atom,module_info},0}.
  {label,6}.
    {move,{atom,overwrite_trytag},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {func_info,{atom,overwrite_trytag},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,overwrite_trytag},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
