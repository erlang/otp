{module, bad_try_catch_nesting}.  %% version = 0

{exports, [{main,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 11}.


{function, main, 2, 2}.
  {label,1}.
    {line,[{location,"bad_try_catch_nesting.erl",4}]}.
    {func_info,{atom,bad_try_catch_nesting},{atom,main},2}.
  {label,2}.
    {allocate_zero,3,2}.
    {'try',{y,1},{f,5}}.
    {move,{x,1},{y,0}}.
    {'try',{y,2},{f,3}}.
    {line,[{location,"bad_try_catch_nesting.erl",7}]}.
    {call_fun,0}.
    {try_end,{y,2}}.
    {jump,{f,4}}.
  {label,3}.
    {try_case,{y,2}}.
    {test,is_ne_exact,{f,4},[{x,0},{atom,error}]}.
    {line,[]}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
  {label,4}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"bad_try_catch_nesting.erl",12}]}.
    {call_fun,0}.
    {try_end,{y,1}}.
    {deallocate,3}.
    return.
  {label,5}.
    {try_case,{y,1}}.
    {test,is_eq_exact,{f,6},[{x,0},{atom,throw}]}.
    {deallocate,3}.
    return.
  {label,6}.
    {line,[]}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,bad_try_catch_nesting},{atom,module_info},0}.
  {label,8}.
    {move,{atom,bad_try_catch_nesting},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,bad_try_catch_nesting},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,bad_try_catch_nesting},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
