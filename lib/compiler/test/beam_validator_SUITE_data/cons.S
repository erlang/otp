{module, cons}.  %% version = 0

{exports, [{foo,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,cons},{atom,foo},1}.
  {label,2}.
%    {test,is_nonempty_list,{f,1},[{x,0}]}.
    {test_heap,3,1}.
    {get_list,{x,0},{x,1},{x,2}}. % <= {bad_type,{needed,cons},{actual,term}}
    {put_tuple,2,{x,0}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {'%live',1}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {func_info,{atom,cons},{atom,module_info},0}.
  {label,4}.
    {move,{atom,cons},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {func_info,{atom,cons},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,cons},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
