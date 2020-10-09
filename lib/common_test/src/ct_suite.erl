-module(ct_suite).

%%------------------------------------------------------------------
%% Test Suite Behaviour
%% ------------------------------------------------------------------
-export_type([ct_testname/0,
              ct_groupname/0,
              ct_config/0,
              ct_status/0,
              ct_group_props/0,
              ct_groupdef/0,
              ct_subgroups/0,
              ct_groupref/0
             ]).

-type ct_testname() :: atom().
-type ct_groupname() :: atom().
-type ct_config() :: [{atom(), term()}].
-type ct_status() :: ok | skipped | failed.
-type ct_group_props() :: [term()].
-type ct_groupdef() ::
        {ct_groupname(), ct_group_props(),
         [ct_testname() | ct_groupdef() | {group,ct_groupname()}]}.
-type ct_subgroups() ::
        {ct_groupname(), ct_group_props()}
      | {ct_groupname(), ct_group_props(), ct_subgroups()}.
-type ct_groupref() ::
        {group, ct_groupname()}
      | {group, ct_groupname(), ct_group_props()}
      | {group, ct_groupname(), ct_group_props(), ct_subgroups()}.

-callback all() -> [ct_testname() | ct_groupref()]
                 | {skip, Reason::term()}.

-callback groups() -> [ct_groupdef()].

-callback suite() -> [tuple()].

-callback init_per_suite(ct_config()) ->
  NewConfig::ct_config() | {skip,Reason::term()}
    | {skip_and_save,Reason::term(),ct_config()}.

-callback end_per_suite(ct_config()) ->
  term() | {save_config,ct_config()}.

-callback group(ct_groupname()) -> [tuple()].

-callback init_per_group(ct_groupname(), ct_config()) ->
  NewConfig::ct_config() | {skip,Reason::term()}.

-callback end_per_group(ct_groupname(), ct_config()) ->
  term() | {return_group_result, ct_status()}.

-callback init_per_testcase(ct_testname(), ct_config()) ->
  NewConfig::ct_config() | {fail,Reason::term()} | {skip,Reason::term()}.

-callback end_per_testcase(ct_testname(), ct_config()) ->
  term() | {fail,Reason::term()} | {save_config,ct_config()}.

%% only all/0 is mandatory
-optional_callbacks([groups/0,
                     suite/0,
                     init_per_suite/1,
                     end_per_suite/1,
                     group/1,
                     init_per_group/2,
                     end_per_group/2,
                     init_per_testcase/2,
                     end_per_testcase/2
                    ]).
