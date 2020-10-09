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
-type ct_status() :: ok |
            skipped |
            failed.
-type ct_group_props() :: [
                parallel |
                sequence |
                ct_shuffle() |
                {ct_group_repeat_type(), ct_test_repeat()}
            ] |
            default.
-type ct_shuffle() :: shuffle |
            {shuffle, ct_shuffle_seed()}.
-type ct_shuffle_seed() :: {integer(), integer(), integer()}.
-type ct_group_repeat_type() :: repeat |
            repeat_until_all_ok |
            repeat_until_all_fail |
            repeat_until_any_ok |
            repeat_until_any_fail.
-type ct_test_repeat() :: integer() |
            forever.
-type ct_groupdef() :: {ct_groupname(), ct_group_props(), [
                ct_testname() |
                ct_groupdef() |
                {group, ct_groupname()} |
                ct_testcase_ref()
            ]}.
-type ct_subgroups() :: {ct_groupname(), ct_group_props()} |
            {ct_groupname(), ct_group_props(), ct_subgroups()}.
-type ct_groupref() :: {group, ct_groupname()} |
            {group, ct_groupname(), ct_group_props()} |
            {group, ct_groupname(), ct_group_props(), ct_subgroups()}.
-type ct_testcase_ref() :: {testcase, ct_testname(), ct_testcase_repeat_props()}.
-type ct_testcase_repeat_props() :: {repeat, ct_test_repeat()} |
            {repeat_until_ok, ct_test_repeat()} |
            {repeat_until_fail, ct_test_repeat()}.
-type ct_group_info() :: {timetrap, ct_group_info_timetrap()} |
            {require, ct_group_info_required()} |
            {require, Name :: atom(), ct_group_info_required()} |
            {userdata, UserData :: term()} |
            {silent_connections, Conns :: [atom()]} |
            {stylesheet, CSSFile :: string()} |
            {ct_hooks, CTHs :: ct_hooks()}.
-type ct_group_info_timetrap() :: MilliSec :: integer() |
            {seconds, integer()} |
            {minutes, integer()} |
            {hours, integer()} |
            {Mod :: module(), Func :: atom(), Args :: list()} |
            ct_group_info_timetrap_fun().
-type ct_group_info_timetrap_fun() :: fun().
-type ct_group_info_required() :: Key :: atom() |
            {Key :: atom(), SubKeys :: ct_group_info_required_subkeys()} |
            {Key :: atom(), Subkey :: atom()} |
            {Key :: atom(), Subkey :: atom(), SubKeys :: ct_group_info_required_subkeys()}.
-type ct_group_info_required_subkeys() :: SubKey :: atom() |
            [SubKey :: atom()].
-type ct_hooks() :: [
                CTHModule :: atom() |
                {CTHModule :: atom(), CTHInitArgs :: term()} |
                {CTHModule :: atom(), CTHInitArgs :: term(), CTHPriority :: term()}
            ].

-callback all() ->
    [ct_testname() | ct_groupref() | ct_testcase_ref()] |
    {skip, Reason::term()}.

-callback groups() ->
    [ct_groupdef()].

-callback suite() ->
    [ct_group_info()].

-callback init_per_suite(ct_config()) ->
    NewConfig::ct_config() |
    {skip,Reason::term()} |
    {skip_and_save,Reason::term(),ct_config()}.

-callback end_per_suite(ct_config()) ->
    term() |
    {save_config,ct_config()}.

-callback group(ct_groupname()) ->
    [ct_group_info()].

-callback init_per_group(ct_groupname(), ct_config()) ->
    NewConfig::ct_config() |
    {skip,Reason::term()}.

-callback end_per_group(ct_groupname(), ct_config()) ->
    term() |
    {return_group_result, ct_status()}.

-callback init_per_testcase(ct_testname(), ct_config()) ->
    NewConfig::ct_config() |
    {fail,Reason::term()} |
    {skip,Reason::term()}.

-callback end_per_testcase(ct_testname(), ct_config()) ->
    term() |
    {fail,Reason::term()} |
    {save_config,ct_config()}.

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
