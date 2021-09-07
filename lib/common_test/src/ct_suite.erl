-module(ct_suite).

%%------------------------------------------------------------------
%% Test Suite Behaviour
%% ------------------------------------------------------------------
-export_type([ct_testname/0,
              ct_groupname/0,
              ct_config/0,
              ct_status/0,
              ct_group_def/0,
              ct_test_def/0,
              ct_info/0
             ]).

-type ct_testname() :: atom().
-type ct_groupname() :: atom().
-type ct_config() :: [{Key :: atom(), Value :: term()}].
-type ct_status() :: ok |
            skipped |
            failed.
-type ct_group_props() :: [
                parallel |
                sequence |
                shuffle |
                {shuffle, Seed :: {integer(), integer(), integer()}} |
                {ct_group_repeat_type(), ct_test_repeat()}
            ].
-type ct_group_props_ref() ::
            ct_group_props() |
            default.
-type ct_group_repeat_type() :: repeat |
            repeat_until_all_ok |
            repeat_until_all_fail |
            repeat_until_any_ok |
            repeat_until_any_fail.
-type ct_test_repeat() :: integer() |
            forever.
-type ct_group_def() :: {ct_groupname(), ct_group_props(), [
                ct_testname() |
                ct_group_def() |
                {group, ct_groupname()} |
                ct_testcase_ref()
            ]}.
-type ct_subgroups_def() :: {ct_groupname(), ct_group_props_ref()} |
            {ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_group_ref() :: {group, ct_groupname()} |
            {group, ct_groupname(), ct_group_props_ref()} |
            {group, ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_testcase_ref() :: {testcase, ct_testname(), ct_testcase_repeat_prop()}.
-type ct_testcase_repeat_prop() :: {repeat, ct_test_repeat()} |
            {repeat_until_ok, ct_test_repeat()} |
            {repeat_until_fail, ct_test_repeat()}.
-type ct_info() :: {timetrap, ct_info_timetrap()} |
            {require, ct_info_required()} |
            {require, Name :: atom(), ct_info_required()} |
            {userdata, UserData :: term()} |
            {silent_connections, Conns :: [atom()]} |
            {stylesheet, CSSFile :: string()} |
            {ct_hooks, CTHs :: ct_hooks()}.
-type ct_info_timetrap() :: MilliSec :: integer() |
            {seconds, integer()} |
            {minutes, integer()} |
            {hours, integer()} |
            {Mod :: atom(), Func :: atom(), Args :: list()} |
            ct_info_timetrap_fun().
-type ct_info_timetrap_fun() :: fun().
-type ct_info_required() :: Key :: atom() |
            {Key :: atom(), SubKeys :: ct_info_required_subkeys()} |
            {Key :: atom(), SubKey :: atom()} |
            {Key :: atom(), SubKey :: atom(), SubKeys :: ct_info_required_subkeys()}.
-type ct_info_required_subkeys() :: SubKey :: atom() |
            [SubKey :: atom()].
-type ct_hooks() :: [
                CTHModule :: atom() |
                {CTHModule :: atom(), CTHInitArgs :: term()} |
                {CTHModule :: atom(), CTHInitArgs :: term(), CTHPriority :: integer()}
            ].
-type ct_test_def() :: ct_testname() | ct_group_ref() | ct_testcase_ref().

-callback all() ->
    [TestDef :: ct_test_def()] |
    {skip, Reason :: term()}.

-callback groups() ->
    [GroupDef :: ct_group_def()].

-callback suite() ->
    [Info :: ct_info()].

-callback init_per_suite(Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {skip, Reason :: term()} |
    {skip_and_save, Reason :: term(), SaveConfig :: ct_config()}.

-callback end_per_suite(Config :: ct_config()) ->
    term() |
    {save_config, SaveConfig :: ct_config()}.

-callback group(GroupName :: ct_groupname()) ->
    [Info :: ct_info()].

-callback init_per_group(GroupName :: ct_groupname(), Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {skip, Reason :: term()}.

-callback end_per_group(GroupName :: ct_groupname(), Config :: ct_config()) ->
    term() |
    {return_group_result, Status :: ct_status()}.

-callback init_per_testcase(TestCase :: ct_testname(), Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {fail, Reason :: term()} |
    {skip, Reason :: term()}.

-callback end_per_testcase(TestCase :: ct_testname(), Config :: ct_config()) ->
    term() |
    {fail, Reason :: term()} |
    {save_config, SaveConfig :: ct_config()}.

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
