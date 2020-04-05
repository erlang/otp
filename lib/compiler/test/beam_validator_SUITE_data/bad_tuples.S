{module, bad_tuples}.  %% version = 0

{exports, [{heap_overflow,1},
           {long,2},
           {module_info,0},
           {module_info,1},
           {self_referential,1},
           {short,1}]}.

{attributes, []}.

{labels, 13}.


{function, short, 1, 2}.
  {label,1}.
    {line,[{location,"bad_tuples.erl",4}]}.
    {func_info,{atom,bad_tuples},{atom,short},1}.
  {label,2}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,ok}}.
    {move,{x,1},{x,0}}.
    return.


{function, long, 2, 4}.
  {label,3}.
    {line,[{location,"bad_tuples.erl",7}]}.
    {func_info,{atom,bad_tuples},{atom,long},2}.
  {label,4}.
    {test_heap,6,2}.
    {put_tuple,2,{x,2}}.
    {put,{x,0}}.
    {put,{x,1}}.
    {put,{atom,too_long}}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.


{function, heap_overflow, 1, 6}.
  {label,5}.
    {line,[{location,"bad_tuples.erl",10}]}.
    {func_info,{atom,bad_tuples},{atom,heap_overflow},1}.
  {label,6}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,ok}}.
    {put,{x,0}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    return.


{function, self_referential, 1, 8}.
  {label,7}.
    {line,[{location,"bad_tuples.erl",13}]}.
    {func_info,{atom,bad_tuples},{atom,self_referential},1}.
  {label,8}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,ok}}.
    {put,{x,1}}.
    {move,{x,1},{x,0}}.
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
