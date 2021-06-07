{module, receive_stacked}.  %% version = 0

{exports, [{f1,0},
           {f2,0},
           {f3,0},
           {f4,0},
           {f5,0},
           {f6,0},
           {f7,0},
           {f8,0},
           {id,1},
           {m1,0},
           {m2,0},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 57}.


{function, f1, 0, 2}.
  {label,1}.
    {line,[{location,"receive_stacked.erl",15}]}.
    {func_info,{atom,receive_stacked},{atom,f1},0}.
  {label,2}.
    {allocate_zero,1,0}.
  {label,3}.
    {loop_rec,{f,5},{x,0}}.
    {move,{x,0},{y,0}}.
    {test,is_integer,{f,4},[{y,0}]}.
    remove_message.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",18}]}.
    {call,1,{f,52}}.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,4}.
    {loop_rec_end,{f,3}}.
  {label,5}.
    {wait,{f,3}}.


{function, f2, 0, 7}.
  {label,6}.
    {line,[{location,"receive_stacked.erl",22}]}.
    {func_info,{atom,receive_stacked},{atom,f2},0}.
  {label,7}.
    {allocate_zero,2,0}.
  {label,8}.
    {loop_rec,{f,10},{x,0}}.
    {test,is_nonempty_list,{f,9},[{x,0}]}.
    {get_list,{x,0},{y,1},{x,0}}.
    {test,is_nil,{f,9},[{x,0}]}.
    {test_heap,3,0}.
    remove_message.
    {put_tuple,2,{y,0}}.
    {put,{atom,ok}}.
    {put,{y,1}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",26}]}.
    {call,1,{f,52}}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {deallocate,2}.
    return.
  {label,9}.
    {loop_rec_end,{f,8}}.
  {label,10}.
    {wait,{f,8}}.


{function, f3, 0, 12}.
  {label,11}.
    {line,[{location,"receive_stacked.erl",30}]}.
    {func_info,{atom,receive_stacked},{atom,f3},0}.
  {label,12}.
    {allocate_zero,2,0}.
  {label,13}.
    {loop_rec,{f,15},{x,0}}.
    {test,is_nonempty_list,{f,14},[{x,0}]}.
    {get_hd,{x,0},{y,1}}.
    {test,is_integer,{f,14},[{y,1}]}.
    {test_heap,3,0}.
    remove_message.
    {put_tuple,2,{y,0}}.
    {put,{atom,ok}}.
    {put,{y,1}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",34}]}.
    {call,1,{f,52}}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {deallocate,2}.
    return.
  {label,14}.
    {loop_rec_end,{f,13}}.
  {label,15}.
    {wait,{f,13}}.


{function, f4, 0, 17}.
  {label,16}.
    {line,[{location,"receive_stacked.erl",38}]}.
    {func_info,{atom,receive_stacked},{atom,f4},0}.
  {label,17}.
    {allocate_zero,2,0}.
  {label,18}.
    {loop_rec,{f,20},{x,0}}.
    {test,is_nonempty_list,{f,19},[{x,0}]}.
    {get_tl,{x,0},{y,1}}.
    {test,is_list,{f,19},[{y,1}]}.
    {test_heap,3,0}.
    remove_message.
    {put_tuple,2,{y,0}}.
    {put,{atom,ok}}.
    {put,{y,1}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",42}]}.
    {call,1,{f,52}}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {deallocate,2}.
    return.
  {label,19}.
    {loop_rec_end,{f,18}}.
  {label,20}.
    {wait,{f,18}}.


{function, f5, 0, 22}.
  {label,21}.
    {line,[{location,"receive_stacked.erl",46}]}.
    {func_info,{atom,receive_stacked},{atom,f5},0}.
  {label,22}.
    {allocate_zero,2,0}.
  {label,23}.
    {loop_rec,{f,25},{x,0}}.
    {test,is_tuple,{f,24},[{x,0}]}.
    {test,test_arity,{f,24},[{x,0},1]}.
    {get_tuple_element,{x,0},0,{y,1}}.
    {test,is_integer,{f,24},[{y,1}]}.
    remove_message.
    {put_map_assoc,{f,0},{literal,#{}},{y,0},0,{list,[{atom,key},{y,1}]}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",50}]}.
    {call,1,{f,52}}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {deallocate,2}.
    return.
  {label,24}.
    {loop_rec_end,{f,23}}.
  {label,25}.
    {wait,{f,23}}.


{function, f6, 0, 27}.
  {label,26}.
    {line,[{location,"receive_stacked.erl",54}]}.
    {func_info,{atom,receive_stacked},{atom,f6},0}.
  {label,27}.
    {allocate_zero,1,0}.
  {label,28}.
    {loop_rec,{f,30},{x,0}}.
    {test,bs_start_match3,{f,29},1,[{x,0}],{x,0}}.
    {test,bs_get_integer2,
          {f,29},
          1,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[56,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {x,1}}.
    {test,bs_get_binary2,
          {f,29},
          1,
          [{x,0},
           {atom,all},
           8,
           {field_flags,[{anno,[56,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {y,0}}.
    {'%',
        {no_bin_opt,
            {binary_used_in,{gc_bif,byte_size,{f,29},0,[{y,0}],{x,0}}},
            [56,{file,"receive_stacked.erl"}]}}.
    {line,[{location,"receive_stacked.erl",56}]}.
    {gc_bif,byte_size,{f,29},0,[{y,0}],{x,0}}.
    {test,is_lt,{f,29},[{integer,8},{x,0}]}.
    remove_message.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",57}]}.
    {call,1,{f,52}}.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,29}.
    {loop_rec_end,{f,28}}.
  {label,30}.
    {wait,{f,28}}.


{function, f7, 0, 32}.
  {label,31}.
    {line,[{location,"receive_stacked.erl",61}]}.
    {func_info,{atom,receive_stacked},{atom,f7},0}.
  {label,32}.
    {allocate_zero,1,0}.
  {label,33}.
    {loop_rec,{f,35},{x,0}}.
    {test,bs_start_match3,{f,34},1,[{x,0}],{x,0}}.
    {test,bs_get_integer2,
          {f,34},
          1,
          [{x,0},
           {integer,8},
           1,
           {field_flags,[{anno,[63,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {x,1}}.
    {test,bs_get_binary2,
          {f,34},
          1,
          [{x,0},
           {atom,all},
           8,
           {field_flags,[{anno,[63,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {y,0}}.
    {'%',{no_bin_opt,{binary_used_in,{test,is_binary,{f,34},[{y,0}]}},
                     [63,{file,"receive_stacked.erl"}]}}.
    {test,is_eq_exact,{f,34},[{y,0},{literal,<<0,1,2,3>>}]}.
    remove_message.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",64}]}.
    {call,1,{f,52}}.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,34}.
    {loop_rec_end,{f,33}}.
  {label,35}.
    {wait,{f,33}}.


{function, f8, 0, 37}.
  {label,36}.
    {line,[{location,"receive_stacked.erl",68}]}.
    {func_info,{atom,receive_stacked},{atom,f8},0}.
  {label,37}.
    {allocate_zero,1,0}.
  {label,38}.
    {loop_rec,{f,40},{x,0}}.
    {test,bs_start_match3,{f,39},1,[{x,0}],{x,1}}.
    {test,bs_get_integer2,
          {f,39},
          2,
          [{x,1},
           {integer,8},
           1,
           {field_flags,[{anno,[70,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {x,2}}.
    {test,bs_get_binary2,
          {f,39},
          2,
          [{x,1},
           {atom,all},
           8,
           {field_flags,[{anno,[70,{file,"receive_stacked.erl"}]},
                         unsigned,big]}],
          {y,0}}.
    {'%',{no_bin_opt,{[{x,1},{y,0}],{loop_rec_end,{f,38}},not_handled},
                     [70,{file,"receive_stacked.erl"}]}}.
    {test,is_eq_exact,{f,39},[{x,0},{literal,<<0,1,2,3>>}]}.
    remove_message.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",71}]}.
    {call,1,{f,52}}.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,39}.
    {loop_rec_end,{f,38}}.
  {label,40}.
    {wait,{f,38}}.


{function, m1, 0, 42}.
  {label,41}.
    {line,[{location,"receive_stacked.erl",75}]}.
    {func_info,{atom,receive_stacked},{atom,m1},0}.
  {label,42}.
    {allocate_zero,1,0}.
  {label,43}.
    {loop_rec,{f,45},{x,0}}.
    {test,is_map,{f,44},[{x,0}]}.
    {get_map_elements,{f,44},{x,0},{list,[{atom,key},{y,0}]}}.
    {test,is_integer,{f,44},[{y,0}]}.
    remove_message.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",78}]}.
    {call,1,{f,52}}.
    {test_heap,2,0}.
    {put_list,{y,0},nil,{x,0}}.
    {deallocate,1}.
    return.
  {label,44}.
    {loop_rec_end,{f,43}}.
  {label,45}.
    {wait,{f,43}}.


{function, m2, 0, 47}.
  {label,46}.
    {line,[{location,"receive_stacked.erl",82}]}.
    {func_info,{atom,receive_stacked},{atom,m2},0}.
  {label,47}.
    {allocate_zero,4,0}.
    {move,{atom,key1},{x,0}}.
    {line,[{location,"receive_stacked.erl",83}]}.
    {call,1,{f,52}}.
    {move,{x,0},{y,3}}.
    {move,{atom,key2},{x,0}}.
    {line,[{location,"receive_stacked.erl",84}]}.
    {call,1,{f,52}}.
    {move,{x,0},{y,2}}.
  {label,48}.
    {loop_rec,{f,50},{x,0}}.
    {test,is_map,{f,49},[{x,0}]}.
    {get_map_elements,{f,49},{x,0},{list,[{y,3},{y,1}]}}.
    {get_map_elements,{f,49},{x,0},{list,[{y,2},{y,0}]}}.
    {test,is_integer,{f,49},[{y,1}]}.
    {test,is_integer,{f,49},[{y,0}]}.
    remove_message.
    {kill,{y,2}}.
    {kill,{y,3}}.
    {move,{integer,42},{x,0}}.
    {line,[{location,"receive_stacked.erl",87}]}.
    {call,1,{f,52}}.
    {test_heap,3,0}.
    {put_tuple,2,{x,0}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {deallocate,4}.
    return.
  {label,49}.
    {loop_rec_end,{f,48}}.
  {label,50}.
    {wait,{f,48}}.


{function, id, 1, 52}.
  {label,51}.
    {line,[{location,"receive_stacked.erl",91}]}.
    {func_info,{atom,receive_stacked},{atom,id},1}.
  {label,52}.
    return.


{function, module_info, 0, 54}.
  {label,53}.
    {line,[]}.
    {func_info,{atom,receive_stacked},{atom,module_info},0}.
  {label,54}.
    {move,{atom,receive_stacked},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 56}.
  {label,55}.
    {line,[]}.
    {func_info,{atom,receive_stacked},{atom,module_info},1}.
  {label,56}.
    {move,{x,0},{x,1}}.
    {move,{atom,receive_stacked},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
