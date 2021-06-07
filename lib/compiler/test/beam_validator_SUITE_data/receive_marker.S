{module, receive_marker}.  %% version = 0

{exports, [{t1,1},{t2,1},{t3,1}]}.

{attributes, []}.

{labels, 16}.

{function, t1, 1, 2}.
  {label,1}.
    {line,[{location,"receive_marker.S",0}]}.
    {func_info,{atom,receive_marker},{atom,t1},1}.
  {label,2}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {call_ext,0,{extfunc,erlang,make_ref,0}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,hello},{x,0}]}}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"receive_marker.S",0}]}.
    send.
  {label,3}.
    {loop_rec,{f,5},{x,0}}.
    {test,is_tagged_tuple,{f,4},[{x,0},3,{atom,reply}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_eq_exact,{f,4},[{x,1},{y,0}]}.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},2,{x,0}}.
    remove_message.
    {put_tuple2,{x,0},{list,[{atom,foo},{x,0}]}}.
    {deallocate,1}.
    return.
  {label,4}.
    {loop_rec_end,{f,3}}.
  {label,5}.
    %% timeout instruction missing on return.
    {move,{atom,timeout},{x,0}}.
    {deallocate,1}.
    return.

{function, t2, 1, 7}.
  {label,6}.
    {line,[{location,"receive_marker.S",0}]}.
    {func_info,{atom,receive_marker},{atom,t2},1}.
  {label,7}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {call_ext,0,{extfunc,erlang,make_ref,0}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,hello},{x,0}]}}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"receive_marker.S",0}]}.
    send.
  {label,8}.
    {loop_rec,{f,10},{x,0}}.
    {test,is_tagged_tuple,{f,9},[{x,0},3,{atom,reply}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_eq_exact,{f,9},[{x,1},{y,0}]}.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},2,{x,0}}.
    remove_message.
    {put_tuple2,{x,0},{list,[{atom,foo},{x,0}]}}.
    {deallocate,1}.
    return.
  {label,9}.
    {loop_rec_end,{f,8}}.
  {label,10}.
    %% timeout instruction missing on tail call
    {move,{atom,timeout},{x,0}}.
    {call_last,1,{f,2},1}.

{function, t3, 1, 12}.
  {label,11}.
    {line,[{location,"receive_marker.S",0}]}.
    {func_info,{atom,receive_marker},{atom,t3},1}.
  {label,12}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {call_ext,0,{extfunc,erlang,make_ref,0}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,hello},{x,0}]}}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"receive_marker.S",0}]}.
    send.
  {label,13}.
    {loop_rec,{f,15},{x,0}}.
    {test,is_tagged_tuple,{f,14},[{x,0},3,{atom,reply}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_eq_exact,{f,14},[{x,1},{y,0}]}.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},2,{x,0}}.
    %% remove_message instruction missing on return.
    {put_tuple2,{x,0},{list,[{atom,foo},{x,0}]}}.
    {deallocate,1}.
    return.
  {label,14}.
    {loop_rec_end,{f,13}}.
  {label,15}.
    timeout.
    {move,{atom,timeout},{x,0}}.
    {deallocate,1}.
    return.
