{module, bad_catch_try}.  %% version = 0

{exports, [{bad_1,1},
           {bad_2,1},
           {bad_3,1},
           {bad_4,1},
           {bad_5,1},
           {bad_6,1},
           {foo,1},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 31}.


{function, bad_1, 1, 2}.
  {label,1}.
    {func_info,{atom,bad_catch_try},{atom,bad_1},1}.
  {label,2}.
    {allocate,1,1}.
    {'catch',{x,0},{f,3}}.
    {call,1,{f,26}}.
  {label,3}.
    {catch_end,{x,0}}.
    {test,is_tuple,{f,4},[{x,0}]}.
    {test,test_arity,{f,4},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,4},[{x,1},{atom,'EXIT'}]}.
    {move,{atom,error},{x,0}}.
    {deallocate,1}.
    return.
  {label,4}.
    {move,{atom,ok},{x,0}}.
    {deallocate,1}.
    return.


{function, bad_2, 1, 6}.
  {label,5}.
    {func_info,{atom,bad_catch_try},{atom,bad_2},1}.
  {label,6}.
    {allocate,1,1}.
    {'catch',{y,0},{f,7}}.
    {call,1,{f,26}}.
  {label,7}.
    {catch_end,{x,9}}.
    {test,is_tuple,{f,8},[{x,0}]}.
    {test,test_arity,{f,8},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,8},[{x,1},{atom,'EXIT'}]}.
    {move,{atom,error},{x,0}}.
    {deallocate,1}.
    return.
  {label,8}.
    {move,{atom,ok},{x,0}}.
    {deallocate,1}.
    return.


{function, bad_3, 1, 10}.
  {label,9}.
    {func_info,{atom,bad_catch_try},{atom,bad_3},1}.
  {label,10}.
    {allocate,2,1}.
    {move,{atom,kalle},{y,1}}.
    {'catch',{y,0},{f,11}}.
    {call,1,{f,26}}.
  {label,11}.
    {catch_end,{y,1}}.
    {test,is_tuple,{f,12},[{x,0}]}.
    {test,test_arity,{f,12},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,12},[{x,1},{atom,'EXIT'}]}.
    {move,{atom,error},{x,0}}.
    {deallocate,1}.
    return.
  {label,12}.
    {move,{atom,ok},{x,0}}.
    {deallocate,1}.
    return.


{function, bad_4, 1, 14}.
  {label,13}.
    {func_info,{atom,bad_catch_try},{atom,bad_4},1}.
  {label,14}.
    {allocate_zero,1,1}.
    {'try',{x,0},{f,15}}.
    {call,1,{f,26}}.
    {try_end,{y,0}}.
    {move,{atom,ok},{x,0}}.
    {jump,{f,16}}.
  {label,15}.
    {try_case,{y,0}}.
    {test,is_ne_exact,{f,16},[{x,0},{atom,error}]}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
    {'%live',1}.
  {label,16}.
    {deallocate,1}.
    return.


{function, bad_5, 1, 18}.
  {label,17}.
    {func_info,{atom,bad_catch_try},{atom,bad_5},1}.
  {label,18}.
    {allocate_zero,2,1}.
    {'try',{y,0},{f,19}}.
    {call,1,{f,26}}.
    {try_end,{y,0}}.
    {move,{atom,ok},{x,0}}.
    {jump,{f,20}}.
  {label,19}.
    {move,{x,0},{y,1}}.
    {try_case,{y,1}}.
    {test,is_ne_exact,{f,20},[{x,0},{atom,error}]}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
    {'%live',1}.
  {label,20}.
    {deallocate,1}.
    return.


{function, bad_6, 1, 22}.
  {label,21}.
    {func_info,{atom,bad_catch_try},{atom,bad_6},1}.
  {label,22}.
    {allocate_zero,1,1}.
    {'try',{y,0},{f,23}}.
    {call,1,{f,26}}.
    {move,{integer,1},{y,1}}.
    {try_end,{y,0}}.
    {move,{atom,ok},{x,0}}.
    {jump,{f,24}}.
  {label,23}.
    {try_case,{y,0}}.
    {test,is_ne_exact,{f,24},[{x,0},{atom,error}]}.
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
    {'%live',1}.
  {label,24}.
    {deallocate,1}.
    return.


{function, foo, 1, 26}.
  {label,25}.
    {func_info,{atom,bad_catch_try},{atom,foo},1}.
  {label,26}.
    return.


{function, module_info, 0, 28}.
  {label,27}.
    {func_info,{atom,bad_catch_try},{atom,module_info},0}.
  {label,28}.
    {move,{atom,bad_catch_try},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 30}.
  {label,29}.
    {func_info,{atom,bad_catch_try},{atom,module_info},1}.
  {label,30}.
    {move,{x,0},{x,1}}.
    {move,{atom,bad_catch_try},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
