{module, unsafe_catch}.  %% version = 0

{exports, [{small,2}]}.

{attributes, []}.

{labels, 14}.


{function, small, 2, 5}.
  {label,4}.
    {func_info,{atom,t},{atom,small},2}.
  {label,5}.
    {allocate_zero,2,2}.
    {'catch',{y,1},{f,6}}.
    {bs_init2,{f,0},1,0,2,{field_flags,[]},{x,2}}.
    {bs_put_integer,{f,0},
                    {integer,8},
                    1,
                    {field_flags,[unsigned,big]},
                    {x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{x,0}}.
  {label,6}.
    {catch_end,{y,1}}.
    {test,is_tuple,{f,7},[{x,0}]}.
    {test,test_arity,{f,7},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,7},[{x,1},{atom,'EXIT'}]}.
    {bs_init2,{f,0},0,0,0,{field_flags,[]},{x,0}}.
  {label,7}.
    {'catch',{y,1},{f,8}}.
    {bs_init2,{f,0},2,0,1,{field_flags,[]},{x,1}}.
    {bs_put_integer,{f,0},
                    {integer,16},
                    1,
                    {field_flags,[unsigned,big]},
                    {y,0}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
  {label,8}.
    {catch_end,{y,1}}.
    {test,is_tuple,{f,9},[{x,0}]}.
    {test,test_arity,{f,9},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,9},[{x,1},{atom,'EXIT'}]}.
    {bs_init2,{f,0},0,0,0,{field_flags,[]},{x,0}}.
  {label,9}.
    {move,{integer,0},{x,1}}.
    {bif,size,{f,0},[{x,0}],{x,2}}.
    {bs_add,{f,0},[{x,1},{x,2},1],{x,1}}.
    {bif,size,{f,0},[{y,0}],{x,2}}.
    {bs_add,{f,0},[{x,1},{x,2},1],{x,1}}.
    {bs_init2,{f,0},{x,1},0,2,{field_flags,[]},{x,1}}.
    {bs_put_binary,{f,0},
                   {atom,all},
                   8,
                   {field_flags,[unsigned,big]},
                   {y,0}}.
    {bs_put_binary,{f,0},
                   {atom,all},
                   8,
                   {field_flags,[unsigned,big]},
                   {x,0}}.
    {move,{x,1},{x,0}}.
    {deallocate,2}.
    return.
