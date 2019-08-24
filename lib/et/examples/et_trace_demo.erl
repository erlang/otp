%%
%% %CopyrightBegin%
%% %CopyrightEnd%
%%

%module
-module(et_trace_demo).

-export([test/0]).

test() ->
    et_viewer:start([
        {title,"Coffee Order"},
        {trace_global,true},
        {trace_pattern,{et,max}},
        {max_actors,10}
      ]),
      %% dbg:p(all,call),
      %% dbg:tpl(et, trace_me, 5, []),
      Drink = {drink,iced_chai_latte},
      Size = {size,grande},
      Milk = {milk,whole},
      Flavor = {flavor,vanilla},
      et:trace_me(99,customer,barrista1,place_order,[Drink,Size,Milk,Flavor]),
      et:trace_me(80,barrista1,register,enter_order,[Drink,Size,Flavor]),
      et:trace_me(80,register,barrista1,give_total,"$5"),
      et:trace_me(80,barrista1,barrista1,get_cup,[Drink,Size]),
      et:trace_me(80,barrista1,barrista2,give_cup,[]),
      et:trace_me(90,barrista1,customer,request_money,"$5"),
      et:trace_me(90,customer,barrista1,pay_money,"$5"),
      et:trace_me(80,barrista2,barrista2,get_chai_mix,[]),
      et:trace_me(80,barrista2,barrista2,add_flavor,[Flavor]),
      et:trace_me(80,barrista2,barrista2,add_milk,[Milk]),
      et:trace_me(80,barrista2,barrista2,add_ice,[]),
      et:trace_me(80,barrista2,barrista2,swirl,[]),
      et:trace_me(80,barrista2,customer,give_tasty_beverage,[Drink,Size]),
      ok.
%module
