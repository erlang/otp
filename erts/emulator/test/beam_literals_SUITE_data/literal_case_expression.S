{module, literal_case_expression}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{x,0},{y,0},
	   {zi1,0},{zi2,0},{za1,0},{za2,0}]}.

{attributes, []}.

{labels, 32}.


{function, x, 0, 2}.
  {label,1}.
    {func_info,{atom,literal_case_expression},{atom,x},0}.
  {label,2}.
    {test,is_integer,{f,5},[{integer,343434343434}]}.
    {select_val,{integer,343434343434},
                {f,5},
                {list,[{integer,343434343434},
                       {f,3},
                       {integer,397439},
                       {f,4},
                       {integer,3976554567454},
                       {f,4}]}}.
  {label,3}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,4}.
    {move,{atom,error},{x,0}}.
    return.
  {label,5}.
    {case_end,{integer,343434343434}}.


{function, y, 0, 7}.
  {label,6}.
    {func_info,{atom,literal_case_expression},{atom,y},0}.
  {label,7}.
    {test,is_float,{f,10},[{float,34.0000}]}.
    {select_val,{float,34.0000},
                {f,10},
                {list,[{float,34.0000},
                       {f,8},
                       {float,397.655},
                       {f,9},
                       {float,39.7439},
                       {f,9}]}}.
  {label,8}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,9}.
    {move,{atom,error},{x,0}}.
    return.
  {label,10}.
    {case_end,{float,34.0000}}.

{function, zi1, 0, 16}.
  {label,15}.
    {func_info,{atom,literal_case_expression},{atom,zi1},0}.
  {label,16}.
    {test,is_integer,{f,19},[{integer,42}]}.
    {select_val,{integer,42},
                {f,18},
                {list,[{integer,42},
                       {f,17},
                       {integer,1000},
                       {f,18}]}}.
  {label,17}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,18}.
    {move,{atom,error},{x,0}}.
    return.
  {label,19}.
    {case_end,{integer,42}}.

{function, zi2, 0, 16}.
  {label,20}.
    {func_info,{atom,literal_case_expression},{atom,zi2},0}.
  {label,21}.
    {test,is_integer,{f,23},[{integer,42}]}.
    {select_val,{integer,42},
                {f,23},
                {list,[{integer,42},
                       {f,22},
                       {integer,1000},
                       {f,23}]}}.
  {label,22}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,23}.
    {move,{atom,error},{x,0}}.
    return.

{function, za1, 0, 25}.
  {label,24}.
    {func_info,{atom,literal_case_expression},{atom,za1},0}.
  {label,25}.
    {test,is_atom,{f,28},[{atom,x}]}.
    {select_val,{atom,x},
                {f,27},
                {list,[{atom,a},
                       {f,27},
                       {atom,x},
                       {f,26}]}}.
  {label,26}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,27}.
    {move,{atom,error},{x,0}}.
    return.
  {label,28}.
    {case_end,{atom,x}}.

{function, za2, 0, 30}.
  {label,29}.
    {func_info,{atom,literal_case_expression},{atom,za2},0}.
  {label,30}.
    {test,is_atom,{f,32},[{atom,x}]}.
    {select_val,{atom,x},
                {f,32},
                {list,[{atom,a},
                       {f,32},
                       {atom,x},
                       {f,31}]}}.
  {label,31}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,32}.
    {move,{atom,error},{x,0}}.
    return.

{function, module_info, 0, 12}.
  {label,11}.
    {func_info,{atom,literal_case_expression},{atom,module_info},0}.
  {label,12}.
    {move,{atom,literal_case_expression},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {func_info,{atom,literal_case_expression},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,literal_case_expression},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
