{module, accessing_tags}.  %% version = 0

{exports, [{foo,1},{bar,1}]}.

{attributes, []}.

{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,accessing_tags},{atom,foo},1}.
  {label,2}.
    {allocate,1,1}.
    {'catch',{y,0},{f,3}}.
    {move,{y,0},{x,0}}.				%Retrieve the catch tag.
    {call,1,{f,2}}.
  {label,3}.
    {catch_end,{y,0}}.
    {deallocate,1}.
    return.

{function, bar, 1, 5}.
  {label,4}.
    {func_info,{atom,accessing_tags},{atom,bar},1}.
  {label,5}.
    {allocate,1,1}.
    {'try',{y,0},{f,6}}.
    {move,{y,0},{x,0}}.				%Retrieve the try tag.
    {call,5,{f,2}}.
  {label,6}.
    {catch_end,{y,0}}.
    {deallocate,1}.
    return.
