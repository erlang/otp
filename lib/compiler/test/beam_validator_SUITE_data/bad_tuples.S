{module, bad_tuples}.  %% version = 0

{exports, [{heap_overflow,1},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 13}.

{function, heap_overflow, 1, 6}.
  {label,5}.
    {line,[{location,"bad_tuples.erl",10}]}.
    {func_info,{atom,bad_tuples},{atom,heap_overflow},1}.
  {label,6}.
    {test_heap,2,1}.
    {put_tuple2,{x,0},{list,[{atom,ok},{x,0}]}}.
    return.


{function, module_info, 0, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,bad_tuples},{atom,module_info},0}.
  {label,10}.
    {move,{atom,bad_tuples},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,bad_tuples},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,bad_tuples},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
