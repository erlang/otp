{module, compiler_bug}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{sum,2}]}.

{attributes, []}.

{labels, 7}.


{function, sum, 2, 2}.
  {label,1}.
    {line,[{location,"compiler_bug.erl",4}]}.
    {func_info,{atom,compiler_bug},{atom,sum},2}.
  {label,2}.
    {line,[{location,"compiler_bug.erl",5}]}.
    {gc_bif,'+',{f,0},2,[{y,0},{y,1}],{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,compiler_bug},{atom,module_info},0}.
  {label,4}.
    {move,{atom,compiler_bug},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,compiler_bug},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,compiler_bug},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
