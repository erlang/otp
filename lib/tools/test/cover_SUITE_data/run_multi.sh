erlc -o ebin src/cover.erl
erlc -o test/cover_SUITE_data/ test/cover_SUITE_data/add_fun_trans.erl
erlc -o test/cover_SUITE_data/ test/cover_SUITE_data/run_multi.erl
erlc +debug_info -o test/cover_SUITE_data/ -pa test/cover_SUITE_data/ test/cover_SUITE_data/multi.erl
erl -pa test/cover_SUITE_data/ -s run_multi run multi_before.html -s init stop
erl -pa ebin -pa test/cover_SUITE_data/ -s run_multi run multi_after.html -s init stop
rm test/cover_SUITE_data/add_fun_trans.beam
rm test/cover_SUITE_data/multi.beam
rm test/cover_SUITE_data/run_multi.beam
