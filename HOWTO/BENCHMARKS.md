Benchmarking Erlang/OTP
=======================

The Erlang/OTP source tree contains a number of benchmarks. The same framework
is used to run these benchmarks as is used to run tests. Therefore in order to
run benchmarks you have to [release the tests][] just as you normally would.

Note that many of these benchmarks were developed to test a specific feature
under a specific setting. We strive to keep the benchmarks up-to-date, but alas
time is not an endless resource so some benchmarks will be outdated and 
irrelevant.

Running the benchmarks
----------------------

As with testing, `ts` is used to run the benchmarks. Before running any 
benchmarks you have to [install the tests][]. To get a listing of all 
benchmarks you have available call `ts:benchmarks()`.

To run all benchmarks call `ts:bench()`. This will run all benchmarks using 
the emulator which is in your `$PATH` (Note that this does not have to be the
same as from which the benchmarks were built from). All the results of the 
benchmarks are put in a folder in `$TESTROOT/test_server/` called 
`YYYY_MO_DDTHH_MI_SS`. 

Each benchmark is run multiple times and the data for all runs is collected in
the files within the benchmark folder. All benchmarks are written so that 
higher values are better. 

Writing benchmarks
------------------

Benchmarks are just normal testcases in Common Test suites. They are marked as
benchmarks by being included in the `AppName_bench.spec` which is located in
`lib/AppName/test/` for the applications which have benchmarks. Note that you
might want to add a skip clause to `AppName.spec` for the benchmarks if you do
not want them to be run in the nightly tests.

Results of benchmarks are sent using the ct_event mechanism and automatically
collected and formatted by ts. 

    ct_event:notify(
        #event{name = benchmark_data, 
               data = [{value,TPS}]}).

The application, suite and testcase associated with the value is automatically
detected. If you want to supply your own you can include `suite` andor `name`
with the data. i.e.

    ct_event:notify(
        #event{name = benchmark_data, 
               data = [{suite,"erts_bench"},
			           {name,"ets_transactions_per_sec"},
			           {value,TPS}]}).
					   
The reason for using the internal ct_event and not ct is because the benchmark 
code has to be backwards compatible with at least R14. 

The value which is reported should be as raw as possible. i.e. you should not
do any averaging of the value before reporting. The tools we use to collect the
benchmark data over time will do averages, means, stddev and more with the data.
So the more data which is sent using `ct_event` the better.

Viewing benchmarks
------------------

At the moment of writing this HOWTO the tool for viewing benchmark results is
not available as opensource. This will hopefully change in the near future.


   [release the tests]: TESTING.md#releasing-tests
   [install the tests]: TESTING.md#configuring-the-test-environment

