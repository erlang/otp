{module, overwrite_catchtag}.  %% version = 0

{exports, [{foo,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,overwrite_catchtag},{atom,foo},1}.
  {label,2}.
    {allocate,1,1}.
    {'catch',{y,0},{f,3}}.
    {move,{x,0},{y,0}}. % <= {catchtag,3}
    {call,1,{f,2}}.
  {label,3}.
    {catch_end,{y,0}}.
    {deallocate,1}.
    return.


{function, module_info, 0, 5}.
  {label,4}.
    {func_info,{atom,overwrite_catchtag},{atom,module_info},0}.
  {label,5}.
    {move,{atom,overwrite_catchtag},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {func_info,{atom,overwrite_catchtag},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,overwrite_catchtag},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
