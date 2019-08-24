{module, call_last}.  %% version = 0

{exports, [{a,1},{b,1},{bar,1},{foo,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 13}.


{function, a, 1, 2}.
  {label,1}.
    {func_info,{atom,t},{atom,a},1}.
  {label,2}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {bif,'+',{f,0},[{y,0},{integer,1}],{x,0}}.
    {'%live',1}.
    {call,1,{f,6}}.
    {bif,'+',{f,0},[{y,0},{integer,2}],{x,0}}.
    {'%live',1}.
    {call_last,1,{f,8},2}.


{function, b, 1, 4}.
  {label,3}.
    {func_info,{atom,t},{atom,b},1}.
  {label,4}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {bif,'+',{f,0},[{y,0},{integer,1}],{x,0}}.
    {'%live',1}.
    {call,1,{f,6}}.
    {move,{y,0},{x,1}}.
    {move,{integer,0},{x,0}}.
    {call_ext_last,2,{extfunc,lists,seq,2},2}.


{function, foo, 1, 6}.
  {label,5}.
    {func_info,{atom,t},{atom,foo},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,glurf},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,put,2}}.


{function, bar, 1, 8}.
  {label,7}.
    {func_info,{atom,t},{atom,bar},1}.
  {label,8}.
    {bif,get,{f,0},[{atom,glurf}],{x,1}}.
    {bif,'+',{f,0},[{x,0},{x,1}],{x,0}}.
    {'%live',1}.
    return.


{function, module_info, 0, 10}.
  {label,9}.
    {func_info,{atom,t},{atom,module_info},0}.
  {label,10}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {func_info,{atom,t},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,t},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
