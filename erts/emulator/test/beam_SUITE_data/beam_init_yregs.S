{module, beam_init_yregs}.  %% version = 0

{exports, [{beam_init_yregs,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, beam_init_yregs, 0, 2}.
  {label,1}.
    {line,[{location,"beam_init_yregs.erl",4}]}.
    {func_info,{atom,beam_init_yregs},{atom,beam_init_yregs},0}.
  {label,2}.
    {allocate,53,0}.
    {init_yregs,{list,[{y,0},
                       {y,1},
                       {y,2},
                       {y,3},
                       {y,4},
                       {y,5},
                       {y,6},
                       {y,7},
                       {y,8},
                       {y,9},
                       {y,10},
                       {y,11},
                       {y,12},
                       {y,13},
                       {y,14},
                       {y,15},
                       {y,16},
                       {y,17},
                       {y,18},
                       {y,19},
                       {y,20},
                       {y,21},
                       {y,22},
                       {y,25},
                       {y,26},
                       {y,27},
                       {y,28},
                       {y,29},
                       {y,30},
                       {y,31},
                       {y,32},
                       {y,33},
                       {y,35},
                       {y,36},
                       {y,37},
                       {y,38},
                       {y,40},
                       {y,41},
                       {y,42},
                       {y,43},
                       {y,44},
                       {y,45},
                       {y,46},
                       {y,47},
                       {y,48},
                       {y,49},
                       {y,50},
                       {y,51},
                       {y,52}]}}.
    {move,{atom,ok},{y,23}}.
    {move,{atom,ok},{y,24}}.
    {move,{atom,ok},{y,34}}.
    {move,{atom,ok},{y,39}}.
    {call_ext,0,{extfunc,erlang,garbage_collect,0}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,53}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,beam_init_yregs},{atom,module_info},0}.
  {label,4}.
    {move,{atom,beam_init_yregs},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,beam_init_yregs},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,beam_init_yregs},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
