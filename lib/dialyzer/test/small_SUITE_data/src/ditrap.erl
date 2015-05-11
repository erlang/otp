%% A bug reported by Tail-f Systems. The problem is that record types
%% are included without properly limiting their depth.

-module(ditrap).

-define(tref(T), ?MODULE:T).
-define(fref(T), ?MODULE:T).

-export_type([ module_rec/0
             , typedef_rec/0
             , type_spec_fun/0
             ]).

-record(type, {
          base :: 'builtin' | external:random_type() | ?tref(typedef_rec()),
          type_spec_fun :: ?fref(type_spec_fun())
         }).

-record(typedef, {type :: #type{}}).

-record(typedefs, {
          map :: ?tref(typedef_rec()),
          parent :: 'undefined' | #typedefs{}
         }).

-record(sn, {
          module :: ?tref(module_rec()),
          typedefs :: #typedefs{},
          type :: 'undefined' | #type{},
          keys :: 'undefined' | [#sn{}],
          children = [] :: [#sn{}]
         }).

-record(augment, {children = [] :: [#sn{}]}).

-record(module, {
          submodules = [] :: [{#module{}, external:pos()}],
          typedefs = #typedefs{} :: #typedefs{},
          children = [] :: [#sn{}],
          remote_augments = [] :: [{ModuleName :: atom(), [#augment{}]}],
          local_augments  = [] :: [#augment{}]
         }).

-type typedef_rec() :: #typedef{}.
-type module_rec() :: #module{}.

-type type_spec_fun() :: undefined | fun((#type{}, #module{}) -> any()).
